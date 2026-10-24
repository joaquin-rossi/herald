module Herald.Instrument
  ( Instrument (..),
    partials,
    additive,
    organ,
    harpsichord,
  )
where

import Herald.Sound

-- | How a single note sounds, given its frequency and duration.
newtype Instrument = Instrument {renderNote :: Hz -> Seconds -> Sound}

-- | Sum of sine partials, given as frequency and relative amplitude,
-- normalized to a peak of 1 and shaped by short attack and release ramps.
-- Partials at or above the Nyquist frequency are dropped to avoid aliasing.
partials :: [(Hz, Double)] -> Seconds -> Sound
partials ws t = mixSound [gain (a / total) (envelope (freq f t)) | (f, a) <- ws']
  where
    ws' = filter ((< sampleRate / 2) . fst) ws
    total = sum (snd <$> ws')
    len = sampleCount t
    ramp i = min 1.0 (fromIntegral i * 0.001)
    envelope = zipWith (*) [ramp i * ramp (len - 1 - i) | i <- [0 :: Int ..]]

-- | Additive synthesis: five slightly inharmonic partials.
additive :: Instrument
additive = Instrument $ \f -> partials (take 5 $ zip (inharmonics 0.001 f) (iterate (* 0.5) 1.0))

-- | Pipe organ: harmonic stops at fixed ratios of the fundamental, with
-- their footage names.
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

-- | Karplus-Strong plucked string, faded out over the last 5 ms so notes
-- cut short don't click.
harpsichord :: Instrument
harpsichord = Instrument $ \f t ->
  let len = sampleCount t
      fade = fromIntegral (sampleCount 0.005)
      envelope = zipWith (*) [min 1 (fromIntegral (len - 1 - i) / fade) | i <- [0 :: Int ..]]
   in envelope (pluck 0.996 f t)
