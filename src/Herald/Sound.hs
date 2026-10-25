-- | Mono signals sampled at 'sampleRate', and stereo.
module Herald.Sound
  ( Seconds,
    Hz,
    Sample,
    Sound,
    sampleRate,
    sampleCount,
    silence,
    sine,
    pluck,
    harmonics,
    inharmonics,
    gain,
    mixSound,
    balanceSound,
    Pan,
    Stereo (..),
    panSound,
    balanceStereo,
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

-- | An audio sample.
type Sample = Double

-- | A mono signal.
type Sound = Vector Sample

-- | Samples per second.
sampleRate :: Double
sampleRate = 48000.0

-- | Samples in a duration.
sampleCount :: Seconds -> Int
sampleCount t = round (sampleRate * t)

-- | Silence.
silence :: Seconds -> Sound
silence t = V.replicate (sampleCount t) 0

-- | A sine wave.
sine :: Hz -> Seconds -> Sound
sine f t = V.generate (sampleCount t) (\i -> sin (fromIntegral i * step))
  where
    step = (f * 2 * pi) / sampleRate

-- | Deterministic white noise in @[-1, 1)@ from a linear congruential
-- generator.
noise :: Int -> [Sample]
noise = map unit . drop 1 . iterate step
  where
    step x = (1103515245 * x + 12345) `mod` 2147483648
    unit x = fromIntegral x / 1073741824 - 1

-- | Karplus-Strong plucked string. @decay@ is the gain per period.
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

-- | Integer multiples of a frequency.
harmonics :: Hz -> [Hz]
harmonics f = (* f) <$> [1 ..]

-- | Harmonics stretched as in stiff strings: @n f sqrt (1 + b n^2)@.
inharmonics :: Double -> Hz -> [Hz]
inharmonics b f = zipWith stretch [1 ..] (harmonics f)
  where
    stretch n h = h * sqrt (1 + b * n ** 2)

-- | Scale every sample.
gain :: Double -> Sound -> Sound
gain g = V.map (* g)

-- | Sums sample by sample, padding shorter sounds with silence.
mixSound :: [Sound] -> Sound
mixSound xs = foldl add (V.replicate len 0) xs
  where
    len = maximum (0 : map V.length xs)
    add acc s = V.imap (\i a -> a + if i < V.length s then V.unsafeIndex s i else 0) acc

-- | Mixes dividing by the square root of the count, to keep the loudness of
-- a single sound.
balanceSound :: [Sound] -> Sound
balanceSound xs = gain (1 / sqrt (fromIntegral (max 1 (length xs)))) (mixSound xs)

-- | From -1 (left) to 1 (right).
type Pan = Double

-- | A left and a right channel.
data Stereo = Stereo
  { leftChannel :: Sound,
    rightChannel :: Sound
  }
  deriving (Show, Eq)

-- | Constant-power pan law: equally loud at any position.
panSound :: Pan -> Sound -> Stereo
panSound p s = Stereo (gain (cos theta) s) (gain (sin theta) s)
  where
    theta = (max (-1) (min 1 p) + 1) * pi / 4

-- | 'balanceSound' for each channel.
balanceStereo :: [Stereo] -> Stereo
balanceStereo xs = Stereo (balanceSound (map leftChannel xs)) (balanceSound (map rightChannel xs))
