module Herald.Sound where

type Seconds = Float

type Hz = Float

type Pulse = Float

type Sound = [Pulse]

sampleRate :: Float
sampleRate = 48000.0

silence :: Seconds -> Sound
silence = freq 0

freq :: Hz -> Seconds -> Sound
freq hz duration = zipWith3 (\x y z -> x * y * z) release attack output
  where
    step = (hz * 2 * pi) / sampleRate

    attack :: Sound
    attack = map (min 1.0) [0.0, 0.001 ..]

    release :: Sound
    release = reverse $ take (length output) attack

    output :: Sound
    output = map sin $ map (* step) [0.0 .. sampleRate * duration]

mixPulse :: [Pulse] -> Float
mixPulse [] = 0
mixPulse xs = sum xs / fromIntegral (length xs)

mixSound :: [Sound] -> Sound
mixSound xs
  | all null xs = []
  | otherwise =
      mixPulse (map headOr0 xs)
        : mixSound (map drop1 xs)
  where
    headOr0 [] = 0
    headOr0 (y : _) = y

    drop1 [] = []
    drop1 (_ : ys) = ys
