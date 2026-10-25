-- |
-- Module      : Herald.Dynamic
-- Description : Dynamics: how loud notes are played.
--
-- Dynamics are gains relative to 'mezzoForte', the level voices start at.
-- Each step is about 3 dB:
--
-- >>> map dynamicGain [piano, mezzoForte, forte]
-- [0.5,1.0,1.4]
--
-- Louder dynamics can push a mix past full scale, so lower the overall level
-- with 'Herald.Sound.gain' if needed.
module Herald.Dynamic
  ( Dynamic (..),
    pianissimo,
    piano,
    mezzoPiano,
    mezzoForte,
    forte,
    fortissimo,
  )
where

-- | A loudness, as a gain relative to 'mezzoForte'.
newtype Dynamic = Dynamic {dynamicGain :: Double}
  deriving (Show, Eq, Ord)

-- | /pp/, very soft.
pianissimo :: Dynamic
pianissimo = Dynamic 0.35

-- | /p/, soft.
piano :: Dynamic
piano = Dynamic 0.5

-- | /mp/, moderately soft.
mezzoPiano :: Dynamic
mezzoPiano = Dynamic 0.7

-- | /mf/, moderately loud: the level voices start at.
mezzoForte :: Dynamic
mezzoForte = Dynamic 1

-- | /f/, loud.
forte :: Dynamic
forte = Dynamic 1.4

-- | /ff/, very loud.
fortissimo :: Dynamic
fortissimo = Dynamic 2
