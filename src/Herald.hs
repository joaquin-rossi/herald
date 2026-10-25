-- |
-- Module      : Herald
-- Description : Write sheet music as Haskell code and synthesize it.
--
-- Herald lets you write scores as Haskell code and render them to audio.
-- This module re-exports everything; import the individual modules for a
-- smaller namespace:
--
-- * "Herald.Score": voices, notes, ties, bars, and tempo, key, meter and
--   dynamic changes.
-- * "Herald.Pitch": written pitches, key signatures and pitch names.
-- * "Herald.Duration": note values and time signatures.
-- * "Herald.Dynamic": dynamics from pianissimo to fortissimo.
-- * "Herald.Tuning": equal temperament and historical temperaments.
-- * "Herald.Instrument": pure tone, additive, organ and harpsichord instruments.
-- * "Herald.Sound": sampled audio, generators and mixing.
-- * "Herald.Sound.IO": WAV output and playback.
--
-- A complete example, rendering two bars to a WAV file:
--
-- @
-- import Herald
--
-- melody :: t'Score' m => m ()
-- melody = do
--   'tellNotes' 'quarter' [g4, bb4, a4, g4]
--   'tellBar'
--   'tellNote' 'half' fs4
--   'tellNote' 'half' g4
--
-- main :: IO ()
-- main = 'saveWav' "melody.wav" ('playScore' 'defaultConfig' {'tempo' = 100} [melody])
-- @
module Herald
  ( module Herald.Duration,
    module Herald.Dynamic,
    module Herald.Instrument,
    module Herald.Pitch,
    module Herald.Score,
    module Herald.Sound,
    module Herald.Sound.IO,
    module Herald.Tuning,
  )
where

import Herald.Duration
import Herald.Dynamic
import Herald.Instrument
import Herald.Pitch
import Herald.Score
import Herald.Sound
import Herald.Sound.IO
import Herald.Tuning
