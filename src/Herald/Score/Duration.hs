-- | Durations in beats, where a quarter note is one beat.
module Herald.Score.Duration
  ( Beats,
    Bpm,
    whole,
    half,
    quarter,
    eighth,
    sixteenth,
    thirtySecond,
    dotted,
    Meter (..),
    meterBeats,
  )
where

-- | A duration in beats.
type Beats = Double

-- | Beats per minute.
type Bpm = Double

-- | Note values.
whole, half, quarter, eighth, sixteenth, thirtySecond :: Beats
whole = 4
half = 2
quarter = 1
eighth = 1 / 2
sixteenth = 1 / 4
thirtySecond = 1 / 8

-- | One and a half times as long.
dotted :: Beats -> Beats
dotted = (* 1.5)

-- | Time signature, as in @Meter 6 8@.
data Meter = Meter Int Int
  deriving (Show, Eq)

-- | Beats in a bar.
meterBeats :: Meter -> Beats
meterBeats (Meter n d) = fromIntegral n * 4 / fromIntegral d
