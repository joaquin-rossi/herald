-- |
-- Module      : Herald.Score
-- Description : Scores: voices written as sequences of notes, rests and bars.
--
-- A score is a list of voices. Each voice is a sequence of 'Score'
-- operations read from left to right, like a staff of sheet music, paired
-- with a t'VoiceConfig' saying how it is performed: its instrument and place
-- in the stereo field. The tempo and tuning are the same for every voice
-- and come from the t'ScoreConfig'.
--
-- @
-- melody :: 'Score' m => m ()
-- melody = do
--   'setKey' 'gMinor'
--   'setMeter' (Meter 3 4)
--   'tellNotes' 'quarter' [g4, a4, b4]
--   'tellBar'
--   'tellNote' ('dotted' 'half') c5
--
-- song :: Either t'ScoreError' 'Sound'
-- song = 'playScoreMono' 'defaultConfig' {'tempo' = 100} [('defaultVoice', melody)]
-- @
--
-- Pitches follow the rules of notation: an unwritten accidental comes from
-- earlier in the same bar or else from the key signature, and a written one
-- lasts until the next 'tellBar'. Like the signatures at the start of a
-- staff, each voice sets its own key with 'setKey' (C major otherwise) and
-- its time signature with 'setMeter', after which every bar is checked to
-- add up to it.
--
-- Voices are written against the 'Score' class, so they can only use these
-- operations, and t'ScoreT' is the interpreter that renders them to sound.
module Herald.Score
  ( -- * Writing voices
    Score (..),
    tellNotes,

    -- * Rendering
    ScoreT,
    ScoreConfig (..),
    defaultConfig,
    VoiceConfig (..),
    defaultVoice,
    playScoreMono,
    playScoreStereo,

    -- * Errors
    ScoreError (..),
    ErrorReason (..),
    displayScoreError,
  )
where

import Control.Monad (forM_, when, zipWithM)
import Control.Monad.Except (MonadError (..))
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Vector.Unboxed qualified as V
import Herald.Duration
import Herald.Dynamic
import Herald.Instrument
import Herald.Pitch
import Herald.Sound
import Herald.Tuning

-- | The operations a voice is written with.
class (Monad m) => Score m where
  -- | A note of the given length and pitch. After 'tellTie' it continues the
  -- tied note instead, which must have the same letter and octave and keeps
  -- its accidental across bar lines.
  tellNote :: Beats -> Pitch -> m ()

  -- | A note tied to the next one, which must be a 'tellNote' or another
  -- 'tellTie' of the same pitch: they sound as a single note, possibly
  -- across bar lines.
  tellTie :: Beats -> Pitch -> m ()

  -- | Several pitches sounding together for the given length.
  tellChord :: Beats -> [Pitch] -> m ()

  -- | A trill of @k@ notes over the given length, alternating between a
  -- pitch and an auxiliary one, starting with the first. As in notation, an
  -- accidental on the auxiliary pitch doesn't carry on through the bar.
  tellTrill :: Beats -> Int -> Pitch -> Pitch -> m ()

  -- | A rest of the given length.
  tellRest :: Beats -> m ()

  -- | Start the next bar: check that the current one adds up to the time
  -- signature, and stop applying its accidentals. A tie may cross it.
  tellBar :: m ()

  -- | Change the key signature from here on.
  setKey :: Key -> m ()

  -- | Change the time signature from here on. From then on, every bar must
  -- add up to it, including the last one; write a pickup bar with leading
  -- rests.
  setMeter :: Meter -> m ()

  -- | Change the dynamic from here on.
  setDynamic :: Dynamic -> m ()

-- | Consecutive notes, all of the same length.
tellNotes :: (Score m) => Beats -> [Pitch] -> m ()
tellNotes b = mapM_ (tellNote b)

-- | Settings shared by every voice of a score.
data ScoreConfig = ScoreConfig
  { tempo :: Bpm,
    tuning :: Tuning
  }

-- | 120 beats per minute, tuned to 'a440'.
defaultConfig :: ScoreConfig
defaultConfig =
  ScoreConfig
    { tempo = 120,
      tuning = a440
    }

-- | How a voice is performed.
data VoiceConfig = VoiceConfig
  { instrument :: Instrument,
    -- | Place in the stereo field; ignored by 'playScoreMono'.
    pan :: Pan
  }

-- | The additive instrument in the centre.
defaultVoice :: VoiceConfig
defaultVoice =
  VoiceConfig
    { instrument = additive,
      pan = 0
    }

-- | Why a score can't be rendered.
data ScoreError = ScoreError
  { -- | The voice, counting from 1.
    errorVoice :: Int,
    -- | The bar, counting from 1.
    errorBar :: Int,
    errorReason :: ErrorReason
  }
  deriving (Show, Eq)

-- | What went wrong in a t'ScoreError'.
data ErrorReason
  = -- | A bar doesn't add up to the time signature: its length and the
    -- expected one, in beats.
    WrongBarLength Beats Beats
  | -- | A tie continues into a note of a different pitch.
    TieToDifferentPitch
  | -- | A tie isn't followed by a note of the same pitch.
    UnfinishedTie
  deriving (Show, Eq)

-- | A human-readable description of a t'ScoreError'.
displayScoreError :: ScoreError -> String
displayScoreError (ScoreError v b reason) =
  "voice " ++ show v ++ ", bar " ++ show b ++ ": " ++ case reason of
    WrongBarLength actual expected ->
      "the bar has " ++ show actual ++ " beats but the meter has " ++ show expected
    TieToDifferentPitch -> "a tie continues into a different pitch"
    UnfinishedTie -> "a tie is not followed by a note of the same pitch"

-- | Renders voices to sound. Voices can't reach its state or errors: only
-- the 'Score' operations.
newtype ScoreT a = ScoreT (Render a)
  deriving newtype (Functor, Applicative, Monad)

instance Score ScoreT where
  tellNote b p = ScoreT (noteR b p)
  tellTie b p = ScoreT (tieR b p)
  tellChord b ps = ScoreT (chordR b ps)
  tellTrill b k p q = ScoreT (trillR b k p q)
  tellRest b = ScoreT (eventR b (Rest b))
  tellBar = ScoreT barR
  setKey k = ScoreT (modify' (\st -> st {stateKey = k}))
  setMeter m = ScoreT (modify' (\st -> st {stateMeter = Just m}))
  setDynamic d = ScoreT (modify' (\st -> st {stateDynamic = d}))

-- | The machinery behind t'ScoreT'.
type Render = StateT ScoreState (WriterT Track (Either ScoreError))

-- | What a voice carries forward as it advances through the score.
data ScoreState = ScoreState
  { -- | Key signature in effect.
    stateKey :: Key,
    -- | Time signature in effect, if any.
    stateMeter :: Maybe Meter,
    -- | Dynamic in effect.
    stateDynamic :: Dynamic,
    -- | Accidentals written so far in the current bar, by letter and octave.
    stateBarAccidentals :: Map (Letter, Octave) Accidental,
    -- | Current bar, counting from 1.
    stateBar :: Int,
    -- | Beats written so far in the current bar.
    stateBarBeats :: Beats,
    -- | A tied note waiting for its continuation.
    stateTie :: Maybe Tie
  }

-- | A tied note: its pitch, dynamic and how long it has lasted so far.
data Tie = Tie
  { tieWritten :: (Letter, Octave),
    tieSemitones :: Semitones,
    tieDynamic :: Dynamic,
    tieBeats :: Beats
  }

-- | Something a voice plays, before choosing how it sounds.
data Event
  = -- | Silence.
    Rest Beats
  | -- | One pitch, or several sounding together.
    Notes Dynamic Beats [Semitones]
  | -- | A trill of @k@ notes between two pitches.
    Trill Dynamic Beats Int Semitones Semitones

-- | What a voice plays, in order.
newtype Track = Track [Event]
  deriving newtype (Semigroup, Monoid)

-- | Render the voices in mono and mix them with 'balanceSound', or return
-- the first error.
--
-- >>> let short = setMeter (Meter 4 4) >> tellNotes quarter [c4, d4, e4] >> tellBar
-- >>> either displayScoreError (const "ok") (playScoreMono defaultConfig [(defaultVoice, short)])
-- "voice 1, bar 1: the bar has 3.0 beats but the meter has 4.0"
playScoreMono :: ScoreConfig -> [(VoiceConfig, ScoreT ())] -> Either ScoreError Sound
playScoreMono config voices = balanceSound . map snd <$> renderVoices config voices

-- | Render the voices, place each at its 'pan', and mix them with
-- 'balanceStereo', or return the first error.
playScoreStereo :: ScoreConfig -> [(VoiceConfig, ScoreT ())] -> Either ScoreError Stereo
playScoreStereo config voices =
  balanceStereo . map (\(v, s) -> panSound (pan v) s) <$> renderVoices config voices

-- | Render each voice independently, without mixing, all starting from the
-- same configuration, or return the first error.
renderVoices :: ScoreConfig -> [(VoiceConfig, ScoreT ())] -> Either ScoreError [(VoiceConfig, Sound)]
renderVoices config = zipWithM render [1 ..]
  where
    render k (v, ScoreT voice) = case runWriterT (execStateT (voice >> finishR) start) of
      Left e -> Left e {errorVoice = k}
      Right (_, Track events) -> Right (v, V.concat (map (renderEvent config v) events))
    start =
      ScoreState
        { stateKey = cMajor,
          stateMeter = Nothing,
          stateDynamic = mezzoForte,
          stateBarAccidentals = Map.empty,
          stateBar = 1,
          stateBarBeats = 0,
          stateTie = Nothing
        }

-- | How an event sounds in a voice.
renderEvent :: ScoreConfig -> VoiceConfig -> Event -> Sound
renderEvent config v = \case
  Rest b -> silence (seconds b)
  Notes d b [n] -> gain (dynamicGain d) (renderNote (instrument v) (freq n) (seconds b))
  Notes d b ns -> gain (dynamicGain d) (chord (instrument v) (map freq ns) (seconds b))
  Trill d b k n m -> gain (dynamicGain d) (trill (instrument v) k (freq n) (freq m) (seconds b))
  where
    seconds b = b * (60.0 / tempo config)
    freq = tune (tuning config)

-- | Check what can only be checked once a voice has ended.
finishR :: Render ()
finishR = do
  requireNoTie
  beats <- gets stateBarBeats
  when (beats > 0) checkBar

-- | Fail with the given reason at the current bar.
failWith :: ErrorReason -> Render a
failWith reason = do
  b <- gets stateBar
  throwError (ScoreError 0 b reason)

barR :: Render ()
barR = do
  checkBar
  modify' $ \st ->
    st
      { stateBar = stateBar st + 1,
        stateBarBeats = 0,
        stateBarAccidentals = Map.empty
      }

-- | Check that the current bar adds up to the time signature, if any.
checkBar :: Render ()
checkBar = do
  meter <- gets stateMeter
  beats <- gets stateBarBeats
  forM_ meter $ \m ->
    when (abs (beats - meterBeats m) > 1e-9) $
      failWith (WrongBarLength beats (meterBeats m))

-- | Semitones of a written pitch, following notation rules: a written
-- accidental lasts until the end of the bar for that letter and octave;
-- an unwritten one comes from earlier in the bar, or else from the key.
resolve :: Pitch -> Render Semitones
resolve p = do
  k <- gets stateKey
  case accidental p of
    Just a -> do
      modify' $ \st -> st {stateBarAccidentals = Map.insert here a (stateBarAccidentals st)}
      pure (pitchSemitones k p)
    Nothing -> do
      written <- gets (Map.lookup here . stateBarAccidentals)
      pure (pitchSemitones k p {accidental = written})
  where
    here = (letter p, octave p)

-- | Like 'resolve', but without recording the accidental, as for the
-- auxiliary note of a trill.
resolveAux :: Pitch -> Render Semitones
resolveAux p = do
  k <- gets stateKey
  written <- gets (Map.lookup (letter p, octave p) . stateBarAccidentals)
  pure (pitchSemitones k p {accidental = maybe written Just (accidental p)})

-- | Move the bar forward by @b@ beats.
advance :: Beats -> Render ()
advance b = modify' (\st -> st {stateBarBeats = stateBarBeats st + b})

-- | Play an event lasting @b@ beats.
eventR :: Beats -> Event -> Render ()
eventR b event = do
  requireNoTie
  tell $ Track [event]
  advance b

-- | Fail if a tie is waiting for its continuation.
requireNoTie :: Render ()
requireNoTie = do
  t <- gets stateTie
  forM_ t $ \_ -> failWith UnfinishedTie

-- | Extend a tied note by @b@ beats of the given pitch.
continueTie :: Beats -> Pitch -> Tie -> Render Tie
continueTie b p t = do
  when ((letter p, octave p) /= tieWritten t) $ failWith TieToDifferentPitch
  advance b
  pure t {tieBeats = tieBeats t + b}

noteR :: Beats -> Pitch -> Render ()
noteR b p =
  gets stateTie >>= \case
    Nothing -> do
      n <- resolve p
      d <- gets stateDynamic
      eventR b (Notes d b [n])
    Just tie -> do
      tie' <- continueTie b p tie
      modify' (\st -> st {stateTie = Nothing})
      tell $ Track [Notes (tieDynamic tie') (tieBeats tie') [tieSemitones tie']]

tieR :: Beats -> Pitch -> Render ()
tieR b p =
  gets stateTie >>= \case
    Nothing -> do
      n <- resolve p
      d <- gets stateDynamic
      advance b
      modify' (\st -> st {stateTie = Just (Tie (letter p, octave p) n d b)})
    Just tie -> do
      tie' <- continueTie b p tie
      modify' (\st -> st {stateTie = Just tie'})

chordR :: Beats -> [Pitch] -> Render ()
chordR b ps = do
  ns <- mapM resolve ps
  d <- gets stateDynamic
  eventR b (Notes d b ns)

trillR :: Beats -> Int -> Pitch -> Pitch -> Render ()
trillR b k p q = do
  n <- resolve p
  m <- resolveAux q
  d <- gets stateDynamic
  eventR b (Trill d b k n m)
