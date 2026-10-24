-- |
-- Module      : Herald.Instrument
-- Description : Instruments: how a note of a given pitch and length sounds.
--
-- An t'Instrument' turns a frequency and a duration into a 'Sound'. Each
-- instrument normalizes its notes to a peak of at most 1, so they can be
-- mixed with one another.
module Herald.Instrument
  ( -- * Instruments
    Instrument (..),
    pureTone,
    additive,
    organ,
    harpsichord,
    partials,

    -- * Playing notes
    note,
    chord,
    trill,
  )
where

import Data.Vector.Unboxed qualified as V
import Herald.Pitch (Semitones, noteFreq)
import Herald.Sound

-- | How a single note sounds, given its frequency and duration.
newtype Instrument = Instrument {renderNote :: Hz -> Seconds -> Sound}

-- | Sum of sine partials, given as frequency and relative amplitude,
-- normalized to a peak of 1 and shaped by short attack and release ramps.
-- Partials at or above the Nyquist frequency are dropped to avoid aliasing.
partials :: [(Hz, Double)] -> Seconds -> Sound
partials ws t = mixSound [gain (a / total) (envelope (sine f t)) | (f, a) <- ws']
  where
    ws' = filter ((< sampleRate / 2) . fst) ws
    total = sum (snd <$> ws')
    len = sampleCount t
    ramp i = min 1.0 (fromIntegral i * 0.001)
    envelope = V.imap (\i x -> ramp i * ramp (len - 1 - i) * x)

-- | A pure tone: a single sine wave at the note's frequency, with no
-- harmonics.
pureTone :: Instrument
pureTone = Instrument $ \f -> partials [(f, 1)]

-- | Additive synthesis: five slightly inharmonic partials with halving
-- amplitudes.
additive :: Instrument
additive = Instrument $ \f -> partials (take 5 $ zip (inharmonics 0.001 f) (iterate (* 0.5) 1.0))

-- | Pipe organ: harmonic stops at fixed ratios of the fundamental, labelled
-- with their footage.
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

-- | Karplus-Strong plucked string (see 'pluck'), faded out over the last
-- 5 ms so notes cut short don't click.
harpsichord :: Instrument
harpsichord = Instrument $ \f t ->
  let len = sampleCount t
      fade = fromIntegral (sampleCount 0.005)
   in V.imap (\i x -> min 1 (fromIntegral (len - 1 - i) / fade) * x) (pluck 0.996 f t)

-- | A note of the given pitch, in semitones relative to A4, and duration.
note :: Instrument -> Semitones -> Seconds -> Sound
note i n = renderNote i (noteFreq n)

-- | Several notes sounding together, mixed with 'balanceSound'.
chord :: Instrument -> [Semitones] -> Seconds -> Sound
chord i ns t = balanceSound [note i n t | n <- ns]

-- | A trill: @k@ equal notes alternating between two pitches, starting with
-- the first, filling the given duration.
trill :: Instrument -> Int -> Semitones -> Semitones -> Seconds -> Sound
trill i k n m t = V.concat [note i x (t / fromIntegral k') | x <- take k' (cycle [n, m])]
  where
    k' = max 1 k
