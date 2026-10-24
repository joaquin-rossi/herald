module Herald.Music
  ( Beats,
    Bpm,
    whole,
    half,
    quarter,
    eighth,
    sixteenth,
    thirtySecond,
    dotted,
    noteFreq,
    note,
    trill,
    chord,
    ScoreConfig (..),
    defaultConfig,
    ScoreEnv,
    ScoreState,
    Score,
    ScoreT,
    playScore,
    askInstrument,
    withInstrument,
    getTempo,
    setTempo,
    getBeatDuration,
    getKey,
    setKey,
    tellBar,
    tellRest,
    tellNote,
    tellNotes,
    tellTrill,
    tellChord,
  )
where

import Control.Monad.RWS.Strict
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
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

-- | The configurable starting point of every voice in a score.
data ScoreConfig = ScoreConfig
  { instrument :: Instrument,
    tempo :: Bpm,
    key :: Key
  }

defaultConfig :: ScoreConfig
defaultConfig =
  ScoreConfig
    { instrument = additive,
      tempo = 120,
      key = cMajor
    }

-- | Settings that apply to a block of the score and are restored after it.
newtype ScoreEnv = ScoreEnv
  { envInstrument :: Instrument
  }

-- | What a voice accumulates as it advances through the score.
data ScoreState = ScoreState
  { -- | Tempo in effect.
    stateTempo :: Bpm,
    -- | Key signature in effect.
    stateKey :: Key,
    -- | Accidentals written so far in the current bar, by letter and octave.
    stateBarAccidentals :: Map (Letter, Octave) Accidental
  }

type Score m = (MonadReader ScoreEnv m, MonadWriter Sound m, MonadState ScoreState m)

newtype ScoreT a = Score {runScoreT :: RWS ScoreEnv Sound ScoreState a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader ScoreEnv,
      MonadWriter Sound,
      MonadState ScoreState
    )

-- | Render each voice independently, all starting from the same
-- configuration, and mix them.
playScore :: ScoreConfig -> [ScoreT ()] -> Sound
playScore config voices = balanceSound [snd (execRWS (runScoreT v) env start) | v <- voices]
  where
    env = ScoreEnv {envInstrument = instrument config}
    start =
      ScoreState
        { stateTempo = tempo config,
          stateKey = key config,
          stateBarAccidentals = Map.empty
        }

askInstrument :: (Score m) => m Instrument
askInstrument = asks envInstrument

withInstrument :: (Score m) => Instrument -> m a -> m a
withInstrument i = local (\env -> env {envInstrument = i})

getTempo :: (Score m) => m Bpm
getTempo = gets stateTempo

-- | Change the tempo from here on.
setTempo :: (Score m) => Bpm -> m ()
setTempo t = modify' (\st -> st {stateTempo = t})

getBeatDuration :: (Score m) => m Seconds
getBeatDuration = (60.0 /) <$> getTempo

getKey :: (Score m) => m Key
getKey = gets stateKey

-- | Change the key signature from here on.
setKey :: (Score m) => Key -> m ()
setKey k = modify' (\st -> st {stateKey = k})

-- | Start the next bar: accidentals written in the previous one stop applying.
tellBar :: (Score m) => m ()
tellBar = modify' (\st -> st {stateBarAccidentals = Map.empty})

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

-- | Emit a sound lasting @b@ beats at the current tempo.
tellBeats :: (Score m) => Beats -> (Seconds -> Sound) -> m ()
tellBeats b sound = do
  bd <- getBeatDuration
  tell $ sound (b * bd)

tellRest :: (Score m) => Beats -> m ()
tellRest b = tellBeats b silence

tellNote :: (Score m) => Beats -> Pitch -> m ()
tellNote b p = do
  n <- resolve p
  i <- askInstrument
  tellBeats b $ note i n

tellNotes :: (Score m) => Beats -> [Pitch] -> m ()
tellNotes b = mapM_ (tellNote b)

tellTrill :: (Score m) => Beats -> Int -> Pitch -> Pitch -> m ()
tellTrill b k p q = do
  n <- resolve p
  m <- resolveAux q
  i <- askInstrument
  tellBeats b $ trill i k n m

tellChord :: (Score m) => Beats -> [Pitch] -> m ()
tellChord b ps = do
  ns <- mapM resolve ps
  i <- askInstrument
  tellBeats b $ chord i ns
