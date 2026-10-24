-- |
-- Module      : Herald.Sound
-- Description : Sampled mono audio, its generators and mixers.
--
-- A 'Sound' is a mono signal sampled at 'sampleRate', stored as an unboxed
-- vector. Samples are nominally in @[-1, 1]@; values outside that range are
-- clipped only when encoding (see "Herald.Sound.IO").
module Herald.Sound
  ( -- * Types
    Seconds,
    Hz,
    Sample,
    Sound,

    -- * Sampling
    sampleRate,
    sampleCount,

    -- * Generators
    silence,
    sine,
    pluck,

    -- * Partials
    harmonics,
    inharmonics,

    -- * Mixing
    gain,
    mixSound,
    balanceSound,
  )
where

import Control.Monad (forM_)
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as V
import Data.Vector.Unboxed.Mutable qualified as MV

-- | A duration in seconds.
type Seconds = Double

-- | A frequency in hertz.
type Hz = Double

-- | A single audio sample.
type Sample = Double

-- | A mono signal sampled at 'sampleRate'.
type Sound = Vector Sample

-- | Samples per second of every 'Sound'.
sampleRate :: Double
sampleRate = 48000.0

-- | Number of samples in a duration, rounded to the nearest sample.
sampleCount :: Seconds -> Int
sampleCount t = round (sampleRate * t)

-- | Silence lasting the given duration.
silence :: Seconds -> Sound
silence t = V.replicate (sampleCount t) 0

-- | A sine wave of the given frequency and duration, starting at phase 0.
sine :: Hz -> Seconds -> Sound
sine f t = V.generate (sampleCount t) (\i -> sin (fromIntegral i * step))
  where
    step = (f * 2 * pi) / sampleRate

-- | Deterministic white noise in @[-1, 1)@ from a linear congruential
-- generator seeded with the given number.
noise :: Int -> [Sample]
noise = map unit . drop 1 . iterate step
  where
    step x = (1103515245 * x + 12345) `mod` 2147483648
    unit x = fromIntegral x / 1073741824 - 1

-- | Karplus-Strong plucked string: one period of noise fed back through a
-- delay line with an averaging low-pass filter. @decay@ is the gain applied
-- on every trip around the loop, i.e. once per period. The result starts at
-- a peak of 1 and fades out on its own.
pluck :: Double -> Hz -> Seconds -> Sound
pluck decay f t = V.create $ do
  out <- MV.new len
  forM_ (zip [0 .. len - 1] excitation) (uncurry (MV.write out))
  let go j averaged' allpassed'
        | n + j >= len = pure ()
        | otherwise = do
            x <- MV.read out j
            y <- MV.read out (j + 1)
            let averaged = decay * (x + y) / 2
                allpassed = c * averaged + averaged' - c * allpassed'
            MV.write out (n + j) allpassed
            go (j + 1) averaged allpassed
  go 0 0 0
  pure out
  where
    len = sampleCount t
    -- The loop is n samples of delay line, minus half a sample from the
    -- averaging filter, plus a fractional delay d from a first-order all-pass
    -- (Jaffe & Smith), so that its total length is exactly one period.
    period = sampleRate / f
    n = max 2 (floor (period + 0.4))
    d = period + 0.5 - fromIntegral n
    c = (1 - d) / (1 + d)
    burst = take n (noise (round f))
    mean = sum burst / fromIntegral n
    peak = maximum [abs (x - mean) | x <- burst]
    excitation = [(x - mean) / peak | x <- burst]

-- | The harmonic series of a fundamental: @f, 2f, 3f, ...@
harmonics :: Hz -> [Hz]
harmonics f = (* f) <$> [1 ..]

-- | The harmonic series stretched by an inharmonicity coefficient @b@, as in
-- stiff strings: the @n@-th partial is @n f sqrt (1 + b n^2)@.
inharmonics :: Double -> Hz -> [Hz]
inharmonics b f = zipWith stretch [1 ..] (harmonics f)
  where
    stretch n h = h * sqrt (1 + b * n ** 2)

-- | Scale every sample by a factor.
gain :: Double -> Sound -> Sound
gain g = V.map (* g)

-- | Sum sounds sample by sample. The result is as long as the longest input;
-- shorter inputs count as silence once they end.
mixSound :: [Sound] -> Sound
mixSound xs = foldl add (V.replicate len 0) xs
  where
    len = maximum (0 : map V.length xs)
    add acc s = V.imap (\i a -> a + if i < V.length s then V.unsafeIndex s i else 0) acc

-- | Mix sounds dividing by the square root of their count, so the result
-- sounds about as loud as a single input. Peaks may exceed the loudest input.
balanceSound :: [Sound] -> Sound
balanceSound xs = gain (1 / sqrt (fromIntegral (max 1 (length xs)))) (mixSound xs)
