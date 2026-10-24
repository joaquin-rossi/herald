module Herald.Music where

import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Herald.Instrument
import Herald.Music.Pitch
import Herald.Sound

type Beats = Double

type Bpm = Double

whole :: Beats
whole = 4

half :: Beats
half = 2

quarter :: Beats
quarter = 1

eighth :: Beats
eighth = 1 / 2

sixteenth :: Beats
sixteenth = 1 / 4

thirtySecond :: Beats
thirtySecond = 1 / 8

dotted :: Beats -> Beats
dotted = (* 1.5)

noteFreq :: Semitones -> Hz
noteFreq n = 440.0 * 2 ** (fromIntegral n / 12)

note :: Instrument -> Semitones -> Seconds -> Sound
note i n = renderNote i (noteFreq n)

-- | Alternate two notes, starting with the first, in @k@ equal notes
-- filling @t@ seconds.
trill :: Instrument -> Int -> Semitones -> Semitones -> Seconds -> Sound
trill i k n m t = concat [note i x (t / fromIntegral k') | x <- take k' (cycle [n, m])]
  where
    k' = max 1 k

chord :: Instrument -> [Semitones] -> Seconds -> Sound
chord i ns t = balanceSound [note i n t | n <- ns]

-- Score

data ScoreEnv = ScoreEnv
  { bpm :: Bpm,
    instrument :: Instrument
  }

type Score m = (MonadReader ScoreEnv m, MonadWriter Sound m)

newtype ScoreT a = Score {runScoreT :: ReaderT ScoreEnv (Writer Sound) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader ScoreEnv,
      MonadWriter Sound
    )

playScore :: ScoreEnv -> ScoreT a -> Sound
playScore env m = execWriter (runReaderT (runScoreT m) env)

askBpm :: (Score m) => m Bpm
askBpm = asks bpm

askBeatDuration :: (Score m) => m Seconds
askBeatDuration = (60.0 /) <$> askBpm

askInstrument :: (Score m) => m Instrument
askInstrument = asks instrument

withInstrument :: (Score m) => Instrument -> m a -> m a
withInstrument i = local (\env -> env {instrument = i})

tellRest :: (Score m) => Beats -> m ()
tellRest b = do
  bd <- askBeatDuration
  tell $ silence (b * bd)

tellNote :: (Score m) => Beats -> Pitch -> m ()
tellNote b p = do
  bd <- askBeatDuration
  i <- askInstrument
  tell $ note i (pitchSemitones p) (b * bd)

tellNotes :: (Score m) => Beats -> [Pitch] -> m ()
tellNotes b = mapM_ (tellNote b)

tellTrill :: (Score m) => Beats -> Int -> Pitch -> Pitch -> m ()
tellTrill b k p q = do
  bd <- askBeatDuration
  i <- askInstrument
  tell $ trill i k (pitchSemitones p) (pitchSemitones q) (b * bd)

tellChord :: (Score m) => Beats -> [Pitch] -> m ()
tellChord b ps = do
  bd <- askBeatDuration
  i <- askInstrument
  tell $ chord i (pitchSemitones <$> ps) (b * bd)

together :: (Score m) => [ScoreT ()] -> m ()
together scores = do
  env <- ask
  tell $ balanceSound [playScore env s | s <- scores]
