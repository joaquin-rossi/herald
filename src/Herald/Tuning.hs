-- | Frequencies of pitches: equal temperament and historical temperaments.
module Herald.Tuning
  ( Tuning (..),
    a440,
    equalTemperament,
    temperament,
    werckmeisterIII,
    quarterCommaMeantone,
  )
where

import Data.Map.Strict qualified as Map
import Herald.Score.Pitch (Semitones)
import Herald.Sound (Hz)

-- | Semitones are relative to A4.
newtype Tuning = Tuning {tune :: Semitones -> Hz}

-- | Equal temperament at 440 Hz.
a440 :: Tuning
a440 = equalTemperament 440

-- | Takes the frequency of A4.
equalTemperament :: Hz -> Tuning
equalTemperament ref = Tuning $ \n -> ref * 2 ** (fromIntegral n / 12)

-- | Deviations from equal temperament in cents for each pitch class from C
-- to B, anchored at the frequency of A4.
temperament :: [Double] -> Hz -> Tuning
temperament cents ref = Tuning $ \n ->
  ref * 2 ** ((fromIntegral n + (offset (n + 9) - offset 9) / 100) / 12)
  where
    deviations = Map.fromList (zip [0 .. 11] cents)
    offset n = Map.findWithDefault 0 (n `mod` 12) deviations

-- | Werckmeister III well temperament.
werckmeisterIII :: Hz -> Tuning
werckmeisterIII =
  temperament [0, -9.775, -7.82, -5.865, -9.775, -1.955, -11.73, -3.91, -7.82, -11.73, -3.91, -7.82]

-- | Quarter-comma meantone.
quarterCommaMeantone :: Hz -> Tuning
quarterCommaMeantone =
  temperament [0, -24.0, -6.8, 10.3, -13.7, 3.4, -20.5, -3.4, -27.4, -10.3, 6.8, -17.1]
