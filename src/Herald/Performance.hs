-- | Renders "Herald.Score" voices to sound.
--
-- >>> let short = setMeter (Meter 4 4) >> tellNotes quarter [c4, d4, e4] >> tellBar
-- >>> either displayScoreError (const "ok") (playScoreMono defaultConfig [(defaultVoice, short)])
-- "voice 1, bar 1: the bar has 3.0 beats but the meter has 4.0"
module Herald.Performance
  ( Performance,
    ScoreConfig (..),
    defaultConfig,
    VoiceConfig (..),
    defaultVoice,
    playScoreMono,
    playScoreStereo,
    ScoreError (..),
    ErrorReason (..),
    displayScoreError,
  )
where

import Control.Monad (forM_, when, zipWithM)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Vector.Unboxed qualified as V
import Herald.Performance.Instrument
import Herald.Score
import Herald.Score.Duration
import Herald.Score.Dynamic
import Herald.Score.Pitch
import Herald.Sound
import Herald.Tuning

-- | Shared by every voice.
data ScoreConfig = ScoreConfig
  { tempo :: Bpm,
    tuning :: Tuning
  }

-- | 120 bpm in equal temperament at 440 Hz.
defaultConfig :: ScoreConfig
defaultConfig =
  ScoreConfig
    { tempo = 120,
      tuning = a440
    }

-- | How a voice is performed.
data VoiceConfig = VoiceConfig
  { instrument :: Instrument,
    -- | Ignored by 'playScoreMono'.
    pan :: Pan
  }

-- | The additive instrument, centred.
defaultVoice :: VoiceConfig
defaultVoice =
  VoiceConfig
    { instrument = additive,
      pan = 0
    }

-- | Voices and bars count from 1.
data ScoreError = ScoreError
  { errorVoice :: Int,
    errorBar :: Int,
    errorReason :: ErrorReason
  }
  deriving (Show, Eq)

-- | A notation mistake in a voice.
data ErrorReason
  = -- | Actual and expected beats.
    WrongBarLength Beats Beats
  | TieToDifferentPitch
  | UnfinishedTie
  deriving (Show, Eq)

-- | As in @voice 2, bar 49: the bar has 4.25 beats but the meter has 4.0@.
displayScoreError :: ScoreError -> String
displayScoreError (ScoreError v b reason) =
  "voice " ++ show v ++ ", bar " ++ show b ++ ": " ++ case reason of
    WrongBarLength actual expected ->
      "the bar has " ++ show actual ++ " beats but the meter has " ++ show expected
    TieToDifferentPitch -> "a tie continues into a different pitch"
    UnfinishedTie -> "a tie is not followed by a note of the same pitch"

-- | Voices can only use the 'Score' operations, not its state or errors.
newtype Performance a = Performance (Perform a)
  deriving newtype (Functor, Applicative, Monad)

instance Score Performance where
  tellNote b p = Performance (noteP b p)
  tellTie b p = Performance (tieP b p)
  tellChord b ps = Performance (chordP b ps)
  tellTrill b k p q = Performance (trillP b k p q)
  tellRest b = Performance (soundP b silence)
  tellBar = Performance barP
  setKey k = Performance (modify' (\st -> st {stateKey = k}))
  setMeter m = Performance (modify' (\st -> st {stateMeter = Just m}))
  setDynamic d = Performance (modify' (\st -> st {stateDynamic = d}))

type Perform =
  ReaderT (ScoreConfig, VoiceConfig) (StateT PerformState (WriterT Track (Either ScoreError)))

data PerformState = PerformState
  { stateKey :: Key,
    stateMeter :: Maybe Meter,
    stateDynamic :: Dynamic,
    stateBarAccidentals :: Map (Letter, Octave) Accidental,
    stateBar :: Int,
    stateBarBeats :: Beats,
    stateTie :: Maybe Tie
  }

data Tie = Tie
  { tieWritten :: (Letter, Octave),
    tieSemitones :: Semitones,
    tieDynamic :: Dynamic,
    tieBeats :: Beats
  }

-- | Chunks joined once at the end: appending vectors note by note would be
-- quadratic.
newtype Track = Track [Sound]
  deriving newtype (Semigroup, Monoid)

-- | Perform the voices in mono.
playScoreMono :: ScoreConfig -> [(VoiceConfig, Performance ())] -> Either ScoreError Sound
playScoreMono config voices = balanceSound . map snd <$> performVoices config voices

-- | Perform the voices in stereo.
playScoreStereo :: ScoreConfig -> [(VoiceConfig, Performance ())] -> Either ScoreError Stereo
playScoreStereo config voices =
  balanceStereo . map (\(v, s) -> panSound (pan v) s) <$> performVoices config voices

performVoices :: ScoreConfig -> [(VoiceConfig, Performance ())] -> Either ScoreError [(VoiceConfig, Sound)]
performVoices config = zipWithM perform [1 ..]
  where
    perform k (v, Performance voice) =
      case runWriterT (execStateT (runReaderT (voice >> finishP) (config, v)) start) of
        Left e -> Left e {errorVoice = k}
        Right (_, Track chunks) -> Right (v, V.concat chunks)
    start =
      PerformState
        { stateKey = cMajor,
          stateMeter = Nothing,
          stateDynamic = mezzoForte,
          stateBarAccidentals = Map.empty,
          stateBar = 1,
          stateBarBeats = 0,
          stateTie = Nothing
        }

finishP :: Perform ()
finishP = do
  requireNoTie
  beats <- gets stateBarBeats
  when (beats > 0) checkBar

failWith :: ErrorReason -> Perform a
failWith reason = do
  b <- gets stateBar
  throwError (ScoreError 0 b reason)

barP :: Perform ()
barP = do
  checkBar
  modify' $ \st ->
    st
      { stateBar = stateBar st + 1,
        stateBarBeats = 0,
        stateBarAccidentals = Map.empty
      }

checkBar :: Perform ()
checkBar = do
  meter <- gets stateMeter
  beats <- gets stateBarBeats
  forM_ meter $ \m ->
    when (abs (beats - meterBeats m) > 1e-9) $
      failWith (WrongBarLength beats (meterBeats m))

-- | A written accidental lasts until the end of the bar for that letter and
-- octave; an unwritten one comes from earlier in the bar, else the key.
resolve :: Pitch -> Perform Semitones
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

-- | Like 'resolve' without recording the accidental, for trill auxiliaries.
resolveAux :: Pitch -> Perform Semitones
resolveAux p = do
  k <- gets stateKey
  written <- gets (Map.lookup (letter p, octave p) . stateBarAccidentals)
  pure (pitchSemitones k p {accidental = maybe written Just (accidental p)})

advance :: Beats -> Perform ()
advance b = modify' (\st -> st {stateBarBeats = stateBarBeats st + b})

seconds :: Beats -> Perform Seconds
seconds b = asks (\(config, _) -> b * (60.0 / tempo config))

frequency :: Semitones -> Perform Hz
frequency n = asks (\(config, _) -> tune (tuning config) n)

voiceInstrument :: Perform Instrument
voiceInstrument = asks (instrument . snd)

emit :: Dynamic -> Sound -> Perform ()
emit d s = tell $ Track [gain (dynamicGain d) s]

soundP :: Beats -> (Seconds -> Sound) -> Perform ()
soundP b sound = do
  requireNoTie
  t <- seconds b
  d <- gets stateDynamic
  emit d (sound t)
  advance b

requireNoTie :: Perform ()
requireNoTie = do
  t <- gets stateTie
  forM_ t $ \_ -> failWith UnfinishedTie

continueTie :: Beats -> Pitch -> Tie -> Perform Tie
continueTie b p t = do
  when ((letter p, octave p) /= tieWritten t) $ failWith TieToDifferentPitch
  advance b
  pure t {tieBeats = tieBeats t + b}

noteP :: Beats -> Pitch -> Perform ()
noteP b p =
  gets stateTie >>= \case
    Nothing -> do
      f <- resolve p >>= frequency
      i <- voiceInstrument
      soundP b (renderNote i f)
    Just tie -> do
      tie' <- continueTie b p tie
      modify' (\st -> st {stateTie = Nothing})
      f <- frequency (tieSemitones tie')
      t <- seconds (tieBeats tie')
      i <- voiceInstrument
      emit (tieDynamic tie') (renderNote i f t)

tieP :: Beats -> Pitch -> Perform ()
tieP b p =
  gets stateTie >>= \case
    Nothing -> do
      n <- resolve p
      d <- gets stateDynamic
      advance b
      modify' (\st -> st {stateTie = Just (Tie (letter p, octave p) n d b)})
    Just tie -> do
      tie' <- continueTie b p tie
      modify' (\st -> st {stateTie = Just tie'})

chordP :: Beats -> [Pitch] -> Perform ()
chordP b ps = do
  fs <- mapM (\p -> resolve p >>= frequency) ps
  i <- voiceInstrument
  soundP b (chord i fs)

trillP :: Beats -> Int -> Pitch -> Pitch -> Perform ()
trillP b k p q = do
  f <- resolve p >>= frequency
  g <- resolveAux q >>= frequency
  i <- voiceInstrument
  soundP b (trill i k f g)
