module Herald.Sound where

type Seconds = Double

type Hz = Double

type Pulse = Double

type Sound = [Pulse]

sampleRate :: Double
sampleRate = 48000.0

sampleCount :: Seconds -> Int
sampleCount t = round (sampleRate * t)

silence :: Seconds -> Sound
silence t = replicate (sampleCount t) 0

freq :: Hz -> Seconds -> Sound
freq f t = [sin (fromIntegral i * step) | i <- [0 .. sampleCount t - 1]]
  where
    step = (f * 2 * pi) / sampleRate

harmonics :: Hz -> [Hz]
harmonics f = (* f) <$> [1 ..]

inharmonics :: Double -> Hz -> [Hz]
inharmonics b f = zipWith stretch [1 ..] (harmonics f)
  where
    stretch n h = h * sqrt (1 + b * n ** 2)

gain :: Double -> Sound -> Sound
gain g = map (* g)

mixPulse :: [Pulse] -> Pulse
mixPulse = sum

mixSound :: [Sound] -> Sound
mixSound xs
  | all null xs = []
  | otherwise =
      mixPulse (headOr0 <$> xs)
        : mixSound (drop1 <$> xs)
  where
    headOr0 [] = 0
    headOr0 (y : _) = y
    drop1 [] = []
    drop1 (_ : ys) = ys

-- | Mix sounds dividing by the square root of their count, so the result
-- sounds about as loud as a single input. Peaks may exceed the loudest input.
balanceSound :: [Sound] -> Sound
balanceSound xs = gain (1 / sqrt (fromIntegral (max 1 (length xs)))) (mixSound xs)
