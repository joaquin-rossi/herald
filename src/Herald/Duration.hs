-- |
-- Module      : Herald.Duration
-- Description : Note durations measured in beats.
--
-- Durations are measured in beats, where a beat is a quarter note. How long
-- a beat lasts in seconds depends on the tempo of the score.
module Herald.Duration
  ( -- * Types
    Beats,
    Bpm,

    -- * Note values
    whole,
    half,
    quarter,
    eighth,
    sixteenth,
    thirtySecond,
    dotted,
  )
where

-- | A duration in beats; a quarter note is one beat.
type Beats = Double

-- | A tempo in beats per minute.
type Bpm = Double

-- | Whole note: four beats.
whole :: Beats
whole = 4

-- | Half note: two beats.
half :: Beats
half = 2

-- | Quarter note: one beat.
quarter :: Beats
quarter = 1

-- | Eighth note: half a beat.
eighth :: Beats
eighth = 1 / 2

-- | Sixteenth note: a quarter of a beat.
sixteenth :: Beats
sixteenth = 1 / 4

-- | Thirty-second note: an eighth of a beat.
thirtySecond :: Beats
thirtySecond = 1 / 8

-- | A dotted note lasts one and a half times as long:
-- @dotted quarter == 1.5@.
dotted :: Beats -> Beats
dotted = (* 1.5)
