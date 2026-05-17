module Herald.Sound where

type Seconds = Float

type Hz = Float

type Pulse = Float

type Sound = [Pulse]

sampleRate :: Float
sampleRate = 48000.0

silence :: Seconds -> Sound
silence t = 0 <$ [0.0 .. sampleRate * t]

freq :: Hz -> Seconds -> Sound
freq f t = sin <$> (* step) <$> [0.0 .. sampleRate * t]
  where
    step = (f * 2 * pi) / sampleRate

harmonics :: Hz -> [Hz]
harmonics f = (* f) <$> [1 ..]

inharmonics :: Float -> Hz -> [Hz]
inharmonics b f = zipWith stretch [1 ..] (harmonics f)
  where
    stretch n h = h * sqrt (1 + b * n ** 2)

mixPulse :: [Pulse] -> Float
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
