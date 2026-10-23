module Herald.Music where

import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Herald.Sound

type Semitones = Int

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

note :: Semitones -> Seconds -> Sound
note n t = mixSound [gain (a / total) (envelope (freq f t)) | (f, a) <- ws]
  where
    fs = inharmonics 0.001 (noteFreq n)
    as = iterate (* 0.5) 1.0
    ws = take 5 $ zip fs as
    total = sum (snd <$> ws)
    len = sampleCount t
    ramp i = min 1.0 (fromIntegral i * 0.001)
    envelope = zipWith (*) [ramp i * ramp (len - 1 - i) | i <- [0 :: Int ..]]

chord :: [Semitones] -> Seconds -> Sound
chord ns t = averageSound [note n t | n <- ns]

-- Score

data ScoreEnv = ScoreEnv
  { bpm :: Bpm
  }
  deriving (Show)

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

tellRest :: (Score m) => Beats -> m ()
tellRest b = do
  bd <- askBeatDuration
  tell $ silence (b * bd)

tellNote :: (Score m) => Semitones -> Beats -> m ()
tellNote n b = do
  bd <- askBeatDuration
  tell $ note n (b * bd)

tellChord :: (Score m) => [Semitones] -> Beats -> m ()
tellChord ns b = do
  bd <- askBeatDuration
  tell $ chord ns (b * bd)

together :: (Score m) => [ScoreT ()] -> m ()
together scores = do
  env <- ask
  tell $ averageSound [playScore env s | s <- scores]
