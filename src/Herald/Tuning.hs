-- |
-- Module      : Herald.Tuning
-- Description : Tuning systems: equal temperament and historical temperaments.
--
-- A t'Tuning' gives the frequency of each pitch. Besides modern equal
-- temperament, Herald includes historical temperaments as they would have
-- tuned a keyboard in Bach's time, where each key has its own colour.
--
-- Temperaments are described by how far each pitch class deviates from equal
-- temperament, in cents, and are anchored so that A4 sounds at the reference
-- frequency.
module Herald.Tuning
  ( Tuning (..),
    a440,
    equalTemperament,
    temperament,
    werckmeisterIII,
    quarterCommaMeantone,
  )
where

import Herald.Pitch (Semitones)
import Herald.Sound (Hz)

-- | Frequency of a pitch given in semitones relative to A4.
newtype Tuning = Tuning {tune :: Semitones -> Hz}

-- | Equal temperament with A4 at 440 Hz, the modern standard.
--
-- >>> tune a440 12
-- 880.0
a440 :: Tuning
a440 = equalTemperament 440

-- | Equal temperament: every semitone is the same ratio, @2 ** (1 / 12)@.
-- The argument is the frequency of A4, such as 415 Hz for baroque pitch.
--
-- >>> tune (equalTemperament 415) (-12)
-- 207.5
equalTemperament :: Hz -> Tuning
equalTemperament ref = Tuning $ \n -> ref * 2 ** (fromIntegral n / 12)

-- | A temperament given by the deviation from equal temperament, in cents,
-- of each pitch class from C to B, with A4 at the given frequency.
-- Missing deviations count as 0.
temperament :: [Double] -> Hz -> Tuning
temperament cents ref = Tuning $ \n ->
  ref * 2 ** ((fromIntegral n + (offset (n + 9) - offset 9) / 100) / 12)
  where
    offset n = (cents ++ repeat 0) !! (n `mod` 12)

-- | Werckmeister III (1691), a well temperament: every key is usable, but
-- keys with few accidentals are purer than remote ones. The argument is the
-- frequency of A4.
werckmeisterIII :: Hz -> Tuning
werckmeisterIII =
  temperament [0, -9.775, -7.82, -5.865, -9.775, -1.955, -11.73, -3.91, -7.82, -11.73, -3.91, -7.82]

-- | Quarter-comma meantone, the usual Renaissance temperament: pure major
-- thirds in the keys around C, unusable remote keys. The argument is the
-- frequency of A4.
quarterCommaMeantone :: Hz -> Tuning
quarterCommaMeantone =
  temperament [0, -24.0, -6.8, 10.3, -13.7, 3.4, -20.5, -3.4, -27.4, -10.3, 6.8, -17.1]
