-- |
-- Module      : Herald.Score
-- Description : Scores: voices written as sequences of notes, rests and bars.
--
-- A score is a list of voices. Each voice is a sequence of @tell*@ actions
-- read from left to right, like a staff of sheet music:
--
-- @
-- melody :: t'Score' m => m ()
-- melody = do
--   'setKey' 'gMinor'
--   'setMeter' (Meter 3 4)
--   'tellNotes' 'quarter' [g4, a4, b4]
--   'tellBar'
--   'tellNote' ('dotted' 'half') c5
--
-- song :: 'Sound'
-- song = 'playScore' 'defaultConfig' {'tempo' = 100} [melody]
-- @
--
-- Pitches follow the rules of notation: an unwritten accidental comes from
-- earlier in the same bar or else from the key signature, and a written one
-- lasts until the next 'tellBar'. Like the signatures at the start of a
-- staff, each voice sets its own key with 'setKey' (C major otherwise) and
-- its time signature with 'setMeter', after which every bar is checked to
-- add up to it.
module Herald.Score
  ( -- * Rendering
    ScoreConfig (..),
    defaultConfig,
    playScore,
    playScoreEither,

    -- * Errors
    ScoreError (..),
    ErrorReason (..),
    displayScoreError,

    -- * Scores
    Score,
    ScoreT,
    ScoreEnv,
    ScoreState,
    Track,

    -- * Notes and rests
    tellNote,
    tellNotes,
    tellTie,
    tellChord,
    tellTrill,
    tellRest,
    tellBar,

    -- * Instrument and tuning
    askInstrument,
    withInstrument,
    askTuning,

    -- * Tempo, key, meter and dynamics
    getTempo,
    setTempo,
    getBeatDuration,
    getKey,
    setKey,
    getMeter,
    setMeter,
    getDynamic,
    setDynamic,
  )
where

import Control.Monad (forM_, when, zipWithM)
import Control.Monad.Except (MonadError (..))
import Control.Monad.RWS.Strict
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Vector.Unboxed qualified as V
import Herald.Duration
import Herald.Dynamic
import Herald.Instrument
import Herald.Pitch
import Herald.Sound
import Herald.Tuning

-- | The configurable starting point of every voice in a score.
data ScoreConfig = ScoreConfig
  { -- | Instrument voices play unless they choose another one.
    instrument :: Instrument,
    -- | Initial tempo.
    tempo :: Bpm,
    -- | Tuning of every voice.
    tuning :: Tuning
  }

-- | The additive instrument at 120 beats per minute, tuned to 'a440'.
defaultConfig :: ScoreConfig
defaultConfig =
  ScoreConfig
    { instrument = additive,
      tempo = 120,
      tuning = a440
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

-- | Settings that apply to a block of the score and are restored after it.
data ScoreEnv = ScoreEnv
  { envInstrument :: Instrument,
    envTuning :: Tuning
  }

-- | What a voice carries forward as it advances through the score.
data ScoreState = ScoreState
  { -- | Tempo in effect.
    stateTempo :: Bpm,
    -- | Key signature in effect.
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

-- | A tied note: what it sounds like and how long it has lasted so far.
data Tie = Tie
  { tieWritten :: (Letter, Octave),
    tieFreq :: Hz,
    tieSeconds :: Seconds,
    tieInstrument :: Instrument,
    tieDynamic :: Dynamic
  }

-- | The sound a voice has produced, as chunks to be joined once at the end.
newtype Track = Track [Sound]
  deriving newtype (Semigroup, Monoid)

-- | The capabilities a voice needs. Write voices against this constraint;
-- 'playScore' runs them in 'ScoreT'.
type Score m =
  ( MonadReader ScoreEnv m,
    MonadWriter Track m,
    MonadState ScoreState m,
    MonadError ScoreError m
  )

-- | The monad voices run in.
newtype ScoreT a = Score {runScoreT :: RWST ScoreEnv Track ScoreState (Either ScoreError) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader ScoreEnv,
      MonadWriter Track,
      MonadState ScoreState,
      MonadError ScoreError
    )

-- | Render each voice independently, all starting from the same
-- configuration, and mix them with 'balanceSound'. Throws an error
-- described by 'displayScoreError' if a voice is malformed.
playScore :: ScoreConfig -> [ScoreT ()] -> Sound
playScore config = either (error . displayScoreError) id . playScoreEither config

-- | Like 'playScore', but returning the first error instead of throwing it.
--
-- >>> let short = setMeter (Meter 4 4) >> tellNotes quarter [c4, d4, e4] >> tellBar
-- >>> either displayScoreError (const "ok") (playScoreEither defaultConfig [short])
-- "voice 1, bar 1: the bar has 3.0 beats but the meter has 4.0"
playScoreEither :: ScoreConfig -> [ScoreT ()] -> Either ScoreError Sound
playScoreEither config voices = balanceSound <$> zipWithM render [1 ..] voices
  where
    render k v = case execRWST (runScoreT (v >> finishVoice)) env start of
      Left e -> Left e {errorVoice = k}
      Right (_, Track chunks) -> Right (V.concat chunks)
    env = ScoreEnv {envInstrument = instrument config, envTuning = tuning config}
    start =
      ScoreState
        { stateTempo = tempo config,
          stateKey = cMajor,
          stateMeter = Nothing,
          stateDynamic = mezzoForte,
          stateBarAccidentals = Map.empty,
          stateBar = 1,
          stateBarBeats = 0,
          stateTie = Nothing
        }

-- | Check what can only be checked once a voice has ended.
finishVoice :: (Score m) => m ()
finishVoice = do
  requireNoTie
  beats <- gets stateBarBeats
  when (beats > 0) checkBar

-- | Fail with the given reason at the current bar.
failWith :: (Score m) => ErrorReason -> m a
failWith reason = do
  b <- gets stateBar
  throwError (ScoreError 0 b reason)

-- | The instrument in effect.
askInstrument :: (Score m) => m Instrument
askInstrument = asks envInstrument

-- | Play a block with another instrument.
withInstrument :: (Score m) => Instrument -> m a -> m a
withInstrument i = local (\env -> env {envInstrument = i})

-- | The tuning of the score.
askTuning :: (Score m) => m Tuning
askTuning = asks envTuning

-- | The tempo in effect.
getTempo :: (Score m) => m Bpm
getTempo = gets stateTempo

-- | Change the tempo from here on.
setTempo :: (Score m) => Bpm -> m ()
setTempo t = modify' (\st -> st {stateTempo = t})

-- | How long a beat lasts at the tempo in effect.
getBeatDuration :: (Score m) => m Seconds
getBeatDuration = (60.0 /) <$> getTempo

-- | The key signature in effect.
getKey :: (Score m) => m Key
getKey = gets stateKey

-- | Change the key signature from here on.
setKey :: (Score m) => Key -> m ()
setKey k = modify' (\st -> st {stateKey = k})

-- | The time signature in effect, if any.
getMeter :: (Score m) => m (Maybe Meter)
getMeter = gets stateMeter

-- | Change the time signature from here on. From then on, every bar must add
-- up to it, including the last one; write a pickup bar with leading rests.
setMeter :: (Score m) => Meter -> m ()
setMeter m = modify' (\st -> st {stateMeter = Just m})

-- | The dynamic in effect.
getDynamic :: (Score m) => m Dynamic
getDynamic = gets stateDynamic

-- | Change the dynamic from here on.
setDynamic :: (Score m) => Dynamic -> m ()
setDynamic d = modify' (\st -> st {stateDynamic = d})

-- | Start the next bar: check that the current one adds up to the time
-- signature, and stop applying its accidentals. A tie may cross it.
tellBar :: (Score m) => m ()
tellBar = do
  checkBar
  modify' $ \st ->
    st
      { stateBar = stateBar st + 1,
        stateBarBeats = 0,
        stateBarAccidentals = Map.empty
      }

-- | Check that the current bar adds up to the time signature, if any.
checkBar :: (Score m) => m ()
checkBar = do
  meter <- getMeter
  beats <- gets stateBarBeats
  forM_ meter $ \m ->
    when (abs (beats - meterBeats m) > 1e-9) $
      failWith (WrongBarLength beats (meterBeats m))

-- | Semitones of a written pitch, following notation rules: a written
-- accidental lasts until the end of the bar for that letter and octave;
-- an unwritten one comes from earlier in the bar, or else from the key.
resolve :: (Score m) => Pitch -> m Semitones
resolve p = do
  k <- getKey
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
resolveAux :: (Score m) => Pitch -> m Semitones
resolveAux p = do
  k <- getKey
  written <- gets (Map.lookup (letter p, octave p) . stateBarAccidentals)
  pure (pitchSemitones k p {accidental = maybe written Just (accidental p)})

-- | Frequency of a semitone count in the score's tuning.
frequency :: (Score m) => Semitones -> m Hz
frequency n = (`tune` n) <$> askTuning

-- | Move the bar forward by @b@ beats.
advance :: (Score m) => Beats -> m ()
advance b = modify' (\st -> st {stateBarBeats = stateBarBeats st + b})

-- | Emit a sound at the current dynamic.
emit :: (Score m) => Dynamic -> Sound -> m ()
emit d s = tell $ Track [gain (dynamicGain d) s]

-- | Emit a sound lasting @b@ beats at the current tempo and dynamic.
tellBeats :: (Score m) => Beats -> (Seconds -> Sound) -> m ()
tellBeats b sound = do
  requireNoTie
  bd <- getBeatDuration
  d <- getDynamic
  emit d (sound (b * bd))
  advance b

-- | Fail if a tie is waiting for its continuation.
requireNoTie :: (Score m) => m ()
requireNoTie = do
  t <- gets stateTie
  forM_ t $ \_ -> failWith UnfinishedTie

-- | Extend a tied note by @b@ beats of the given pitch.
continueTie :: (Score m) => Beats -> Pitch -> Tie -> m Tie
continueTie b p t = do
  when ((letter p, octave p) /= tieWritten t) $ failWith TieToDifferentPitch
  bd <- getBeatDuration
  advance b
  pure t {tieSeconds = tieSeconds t + b * bd}

-- | A rest of the given length.
tellRest :: (Score m) => Beats -> m ()
tellRest b = tellBeats b silence

-- | A note of the given length and pitch. After 'tellTie' it continues the
-- tied note instead, which must have the same letter and octave and keeps
-- its accidental across bar lines.
tellNote :: (Score m) => Beats -> Pitch -> m ()
tellNote b p =
  gets stateTie >>= \case
    Nothing -> do
      f <- resolve p >>= frequency
      i <- askInstrument
      tellBeats b (renderNote i f)
    Just t -> do
      t' <- continueTie b p t
      modify' (\st -> st {stateTie = Nothing})
      emit (tieDynamic t') (renderNote (tieInstrument t') (tieFreq t') (tieSeconds t'))

-- | Consecutive notes, all of the same length.
tellNotes :: (Score m) => Beats -> [Pitch] -> m ()
tellNotes b = mapM_ (tellNote b)

-- | A note tied to the next one, which must be a 'tellNote' or another
-- 'tellTie' of the same pitch: they sound as a single note, possibly across
-- bar lines.
tellTie :: (Score m) => Beats -> Pitch -> m ()
tellTie b p =
  gets stateTie >>= \case
    Nothing -> do
      f <- resolve p >>= frequency
      i <- askInstrument
      d <- getDynamic
      bd <- getBeatDuration
      advance b
      modify' (\st -> st {stateTie = Just (Tie (letter p, octave p) f (b * bd) i d)})
    Just t -> do
      t' <- continueTie b p t
      modify' (\st -> st {stateTie = Just t'})

-- | Several pitches sounding together for the given length.
tellChord :: (Score m) => Beats -> [Pitch] -> m ()
tellChord b ps = do
  fs <- mapM (\p -> resolve p >>= frequency) ps
  i <- askInstrument
  tellBeats b $ chord i fs

-- | A trill of @k@ notes over the given length, alternating between a pitch
-- and an auxiliary one, starting with the first. As in notation, an
-- accidental on the auxiliary pitch doesn't carry on through the bar.
tellTrill :: (Score m) => Beats -> Int -> Pitch -> Pitch -> m ()
tellTrill b k p q = do
  f <- resolve p >>= frequency
  g <- resolveAux q >>= frequency
  i <- askInstrument
  tellBeats b $ trill i k f g
