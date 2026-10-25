-- | Dynamics as gains relative to 'mezzoForte', where voices start.
module Herald.Score.Dynamic
  ( Dynamic (..),
    pianissimo,
    piano,
    mezzoPiano,
    mezzoForte,
    forte,
    fortissimo,
  )
where

newtype Dynamic = Dynamic {dynamicGain :: Double}
  deriving (Show, Eq, Ord)

pianissimo, piano, mezzoPiano, mezzoForte, forte, fortissimo :: Dynamic
pianissimo = Dynamic 0.35
piano = Dynamic 0.5
mezzoPiano = Dynamic 0.7
mezzoForte = Dynamic 1
forte = Dynamic 1.4
fortissimo = Dynamic 2
