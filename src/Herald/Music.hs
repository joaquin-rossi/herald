module Herald.Music where

import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Herald.Sound

type Semitones = Float

type Beats = Float

rest :: Seconds -> Beats -> Sound
rest bd b = silence (b * bd)

note :: Seconds -> Semitones -> Beats -> Sound
note bd n b = freq f (b * bd)
  where
    f :: Hz
    f = 440.0 * (2 ** (1.0 / 12.0)) ** n

chord :: Seconds -> [Semitones] -> Beats -> Sound
chord bd ns b = mixSound [note bd n b | n <- ns]

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

runScore :: ScoreEnv -> ScoreT a -> Sound
runScore env m = execWriter (runReaderT (runScoreT m) env)

askBpm :: (Score m) => m Beats
askBpm = asks bpm

askBeatDuration :: (Score m) => m Seconds
askBeatDuration = (60.0 /) <$> askBpm

tellRest :: (Score m) => Beats -> m ()
tellRest b = do
  bd <- askBeatDuration
  tell $ rest bd b

tellNote :: (Score m) => Semitones -> Beats -> m ()
tellNote n b = do
  bd <- askBeatDuration
  tell $ note bd n b

tellChord :: (Score m) => [Semitones] -> Beats -> m ()
tellChord ns b = do
  bd <- askBeatDuration
  tell $ chord bd ns b
