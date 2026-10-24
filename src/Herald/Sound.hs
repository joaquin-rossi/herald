module Herald.Sound
  ( Seconds,
    Hz,
    Pulse,
    Sound,
    sampleRate,
    sampleCount,
    silence,
    freq,
    whiteNoise,
    pluck,
    harmonics,
    inharmonics,
    gain,
    mixSound,
    balanceSound,
  )
where

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

-- | Deterministic white noise in [-1, 1) from a linear congruential generator.
whiteNoise :: Int -> Sound
whiteNoise = map unit . drop 1 . iterate step
  where
    step x = (1103515245 * x + 12345) `mod` 2147483648
    unit x = fromIntegral x / 1073741824 - 1

-- | Karplus-Strong plucked string: one period of noise fed back through a
-- delay line with an averaging low-pass filter. @decay@ is the gain applied
-- on every trip around the loop, i.e. once per period.
pluck :: Double -> Hz -> Seconds -> Sound
pluck decay f t = take (sampleCount t) out
  where
    -- The loop is n samples of delay line, minus half a sample from the
    -- averaging filter, plus a fractional delay d from a first-order all-pass
    -- (Jaffe & Smith), so that its total length is exactly one period.
    period = sampleRate / f
    n = max 2 (floor (period + 0.4))
    d = period + 0.5 - fromIntegral n
    c = (1 - d) / (1 + d)
    burst = take n (whiteNoise (round f))
    mean = sum burst / fromIntegral n
    peak = maximum [abs (x - mean) | x <- burst]
    excitation = [(x - mean) / peak | x <- burst]
    averaged = zipWith (\x y -> decay * (x + y) / 2) out (drop 1 out)
    allpassed = zipWith3 (\x x' y' -> c * x + x' - c * y') averaged (0 : averaged) (0 : allpassed)
    out = excitation ++ allpassed

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
