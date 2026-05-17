module Herald.Music where

import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Herald.Sound

type Semitones = Int

type Beats = Float

noteFreq :: Semitones -> Hz
noteFreq n = 440.0 * (2 ** (1.0 / 12.0)) ** (fromIntegral n)

note :: Semitones -> Seconds -> Sound
note n t = mixSound [(a *) <$> freq' f | (f, a) <- ws]
  where
    freq' f = zipWith3 (\x y z -> x * y * z) attack release output
      where
        output = freq f t
        attack = min 1.0 <$> [0.0, 0.001 ..]
        release = reverse $ take (length output) attack
    fs = inharmonics 0.001 (noteFreq n)
    as = iterate (* 0.5) (1.0 :: Float)
    ws = take 5 $ zip fs as

chord :: [Semitones] -> Seconds -> Sound
chord ns t = mixSound [note n t | n <- ns]

-- Score

data ScoreEnv = ScoreEnv
  { bpm :: Beats
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

askBpm :: (Score m) => m Beats
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

together :: [ScoreT ()] -> ScoreT ()
together scores = do
  env <- ask
  tell $ mixSound [playScore env s | s <- scores]
