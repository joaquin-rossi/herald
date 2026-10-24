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
-- lasts until the next 'tellBar'. Voices start in C major; like the key
-- signature at the start of a staff, each voice sets its own with 'setKey'.
module Herald.Score
  ( -- * Rendering
    ScoreConfig (..),
    defaultConfig,
    playScore,

    -- * Scores
    Score,
    ScoreT,
    ScoreEnv,
    ScoreState,
    Track,

    -- * Notes and rests
    tellNote,
    tellNotes,
    tellChord,
    tellTrill,
    tellRest,
    tellBar,

    -- * Instrument
    askInstrument,
    withInstrument,

    -- * Tempo and key
    getTempo,
    setTempo,
    getBeatDuration,
    getKey,
    setKey,
  )
where

import Control.Monad.RWS.Strict
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Vector.Unboxed qualified as V
import Herald.Duration
import Herald.Instrument
import Herald.Pitch
import Herald.Sound

-- | The configurable starting point of every voice in a score.
data ScoreConfig = ScoreConfig
  { -- | Instrument voices play unless they choose another one.
    instrument :: Instrument,
    -- | Initial tempo.
    tempo :: Bpm
  }

-- | The additive instrument at 120 beats per minute.
defaultConfig :: ScoreConfig
defaultConfig =
  ScoreConfig
    { instrument = additive,
      tempo = 120
    }

-- | Settings that apply to a block of the score and are restored after it.
newtype ScoreEnv = ScoreEnv
  { envInstrument :: Instrument
  }

-- | What a voice carries forward as it advances through the score.
data ScoreState = ScoreState
  { -- | Tempo in effect.
    stateTempo :: Bpm,
    -- | Key signature in effect.
    stateKey :: Key,
    -- | Accidentals written so far in the current bar, by letter and octave.
    stateBarAccidentals :: Map (Letter, Octave) Accidental
  }

-- | The sound a voice has produced, as chunks to be joined once at the end.
newtype Track = Track [Sound]
  deriving newtype (Semigroup, Monoid)

-- | The capabilities a voice needs. Write voices against this constraint;
-- 'playScore' runs them in 'ScoreT'.
type Score m = (MonadReader ScoreEnv m, MonadWriter Track m, MonadState ScoreState m)

-- | The monad voices run in.
newtype ScoreT a = Score {runScoreT :: RWS ScoreEnv Track ScoreState a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader ScoreEnv,
      MonadWriter Track,
      MonadState ScoreState
    )

-- | Render each voice independently, all starting from the same
-- configuration, and mix them with 'balanceSound'.
playScore :: ScoreConfig -> [ScoreT ()] -> Sound
playScore config voices = balanceSound [render v | v <- voices]
  where
    render v = let Track chunks = snd (execRWS (runScoreT v) env start) in V.concat chunks
    env = ScoreEnv {envInstrument = instrument config}
    start =
      ScoreState
        { stateTempo = tempo config,
          stateKey = cMajor,
          stateBarAccidentals = Map.empty
        }

-- | The instrument in effect.
askInstrument :: (Score m) => m Instrument
askInstrument = asks envInstrument

-- | Play a block with another instrument.
withInstrument :: (Score m) => Instrument -> m a -> m a
withInstrument i = local (\env -> env {envInstrument = i})

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
  tell $ Track [sound (b * bd)]

-- | A rest of the given length.
tellRest :: (Score m) => Beats -> m ()
tellRest b = tellBeats b silence

-- | A note of the given length and pitch.
tellNote :: (Score m) => Beats -> Pitch -> m ()
tellNote b p = do
  n <- resolve p
  i <- askInstrument
  tellBeats b $ note i n

-- | Consecutive notes, all of the same length.
tellNotes :: (Score m) => Beats -> [Pitch] -> m ()
tellNotes b = mapM_ (tellNote b)

-- | Several pitches sounding together for the given length.
tellChord :: (Score m) => Beats -> [Pitch] -> m ()
tellChord b ps = do
  ns <- mapM resolve ps
  i <- askInstrument
  tellBeats b $ chord i ns

-- | A trill of @k@ notes over the given length, alternating between a pitch
-- and an auxiliary one, starting with the first. As in notation, an
-- accidental on the auxiliary pitch doesn't carry on through the bar.
tellTrill :: (Score m) => Beats -> Int -> Pitch -> Pitch -> m ()
tellTrill b k p q = do
  n <- resolve p
  m <- resolveAux q
  i <- askInstrument
  tellBeats b $ trill i k n m
