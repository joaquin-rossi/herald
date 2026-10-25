-- | Instruments render a note of a frequency and duration, peaking at most 1.
module Herald.Performance.Instrument
  ( Instrument (..),
    pureTone,
    additive,
    organ,
    harpsichord,
    partials,
    chord,
    trill,
  )
where

import Data.Vector.Unboxed qualified as V
import Herald.Sound

-- | Renders a note of a frequency and duration.
newtype Instrument = Instrument {renderNote :: Hz -> Seconds -> Sound}

-- | Sine partials as frequency and relative amplitude, with short attack and
-- release ramps. Partials above Nyquist are dropped to avoid aliasing.
partials :: [(Hz, Double)] -> Seconds -> Sound
partials ws t = mixSound [gain (a / total) (envelope (sine f t)) | (f, a) <- ws']
  where
    ws' = filter ((< sampleRate / 2) . fst) ws
    total = sum (snd <$> ws')
    len = sampleCount t
    ramp i = min 1.0 (fromIntegral i * 0.001)
    envelope = V.imap (\i x -> ramp i * ramp (len - 1 - i) * x)

-- | A single sine wave.
pureTone :: Instrument
pureTone = Instrument $ \f -> partials [(f, 1)]

-- | Five slightly inharmonic partials.
additive :: Instrument
additive = Instrument $ \f -> partials (take 5 $ zip (inharmonics 0.001 f) (iterate (* 0.5) 1.0))

-- | Pipe organ stops.
organ :: Instrument
organ = Instrument $ \f -> partials [(f * r, a) | (r, a) <- stops]
  where
    stops =
      [ (0.5, 0.5), -- 16'
        (1, 1), -- 8'
        (2, 0.6), -- 4'
        (3, 0.3), -- 2 2/3'
        (4, 0.4), -- 2'
        (8, 0.2) -- 1'
      ]

-- | Faded out over the last 5 ms so notes cut short don't click.
harpsichord :: Instrument
harpsichord = Instrument $ \f t ->
  let len = sampleCount t
      fade = fromIntegral (sampleCount 0.005)
   in V.imap (\i x -> min 1 (fromIntegral (len - 1 - i) / fade) * x) (pluck 0.996 f t)

-- | Notes sounding together.
chord :: Instrument -> [Hz] -> Seconds -> Sound
chord i fs t = balanceSound [renderNote i f t | f <- fs]

-- | @k@ equal notes alternating @f@ and @g@.
trill :: Instrument -> Int -> Hz -> Hz -> Seconds -> Sound
trill i k f g t = V.concat [renderNote i x (t / fromIntegral k') | x <- take k' (cycle [f, g])]
  where
    k' = max 1 k
