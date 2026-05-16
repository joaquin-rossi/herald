module Main.Music.LittleFugueInGMinor where

import Herald.Music
import Herald.Sound

songLittleFugueInGMinor :: Sound
songLittleFugueInGMinor = (* 0.25) <$> runScore (ScoreEnv {bpm = 144}) score
  where
    score = together [scoreLittleFugueInGMinor1, scoreLittleFugueInGMinor2]

scoreLittleFugueInGMinor1 :: (Score m) => m ()
scoreLittleFugueInGMinor1 = do
  tellNote (-2) 1.0
  tellNote 5 1.0
  tellNote 1 1.5
  tellNote 0 0.5
  --
  tellNote (-2) 0.5
  tellNote 1 0.5
  tellNote 0 0.5
  tellNote (-2) 0.5
  tellNote (-3) 0.5
  tellNote 0 0.5
  tellNote (-7) 1.0
  --
  tellNote (-2) 0.5
  tellNote (-7) 0.5
  tellNote 0 0.5
  tellNote (-7) 0.5
  tellNote 1 0.5
  tellNote 0 0.25
  tellNote (-2) 0.25
  tellNote 0 0.5
  tellNote (-7) 0.5
  --
  tellNote (-2) 0.5
  tellNote (-7) 0.25
  tellNote (-2) 0.25
  tellNote 0 0.5
  tellNote (-7) 0.25
  tellNote 0 0.25
  tellNote 1 0.5
  tellNote 0 0.25
  tellNote (-2) 0.25
  tellNote 0 0.25
  tellNote (-7) 0.25
  tellNote 5 0.25
  tellNote 3 0.25
  --
  tellNote 1 0.25
  tellNote 0 0.25
  tellNote (-2) 0.25
  tellNote 1 0.25
  tellNote 0 0.25
  tellNote (-2) 0.25
  tellNote (-3) 0.25
  tellNote 0 0.25
  tellNote (-2) 0.25
  tellNote (-7) 0.25
  tellNote (-2) 0.25
  tellNote 0 0.25
  tellNote 1 0.25
  tellNote 3 0.25
  tellNote 5 0.25
  tellNote 7 0.25
  --
  tellNote 8 0.25
  tellNote 7 0.25
  tellNote 5 0.25
  tellNote 8 0.25
  tellNote 7 0.25
  tellNote 5 0.25
  tellNote 4 0.25
  tellNote 7 0.25
  tellNote 5 0.5
  tellNote 0 0.5
  tellNote 5 0.5
  tellNote 7 0.5
  --
  tellNote 8 0.25
  tellNote 10 0.25
  tellNote 8 0.25
  tellNote 10 0.25
  do
    -- trill
    tellNote 10 0.125
    tellNote 12 0.125
    tellNote 10 0.125
    tellNote 12 0.125
  tellNote 8 0.25
  tellNote 10 0.25
  tellNote 12 0.25
  tellNote 10 0.25
  tellNote 12 0.25
  tellNote 13 0.25
  tellNote 12 0.25
  tellNote 10 0.25
  tellNote 8 0.25
  tellNote 7 0.25
  --
  tellNote 8 0.25
  tellNote 12 0.25
  tellNote 10 0.25
  tellNote 12 0.25
  tellNote 4 0.25
  tellNote 12 0.25
  tellNote 10 0.25
  tellNote 12 0.25
  tellNote 5 0.25
  tellNote 12 0.25
  tellNote 10 0.25
  tellNote 12 0.25
  tellNote 4 0.25
  tellNote 12 0.25
  tellNote 10 0.25
  tellNote 12 0.25

scoreLittleFugueInGMinor2 :: (Score m) => m ()
scoreLittleFugueInGMinor2 = do
  tellRest 4.0
  --
  tellRest 4.0
  --
  tellRest 4.0
  --
  tellRest 4.0
  --
  tellRest 4.0
  --
  tellNote (-7) 1.0
  tellNote 0 1.0
  tellNote (-4) 1.5
  tellNote (-5) 0.5
  --
  tellNote (-7) 0.5
  tellNote (-4) 0.5
  tellNote (-5) 0.5
  tellNote (-7) 0.5
  tellNote (-8) 0.5
  tellNote (-5) 0.5
  tellNote (-12) 1.0
  --
  tellNote (-7) 0.5
  tellNote (-12) 0.5
  tellNote (-5) 0.5
  tellNote (-12) 0.5
  tellNote (-4) 0.5
  tellNote (-5) 0.25
  tellNote (-7) 0.25
  tellNote (-5) 0.5
  tellNote (-12) 0.5
