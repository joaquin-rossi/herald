-- |
-- Module      : Herald.Duration
-- Description : Note durations and time signatures, measured in beats.
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

    -- * Time signatures
    Meter (..),
    meterBeats,
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

-- | A dotted note lasts one and a half times as long.
--
-- >>> dotted quarter
-- 1.5
dotted :: Beats -> Beats
dotted = (* 1.5)

-- | A time signature: beats per bar over the note value of a beat, as in
-- @Meter 3 4@ for three-four time or @Meter 6 8@ for six-eight.
data Meter = Meter Int Int
  deriving (Show, Eq)

-- | Length of a bar in beats, where a quarter note is one beat.
--
-- >>> meterBeats (Meter 6 8)
-- 3.0
meterBeats :: Meter -> Beats
meterBeats (Meter n d) = fromIntegral n * 4 / fromIntegral d
