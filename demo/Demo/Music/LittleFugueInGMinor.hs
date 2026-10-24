module Demo.Music.LittleFugueInGMinor where

import Herald.Music
import Herald.Sound

songLittleFugueInGMinor :: Sound
songLittleFugueInGMinor = playScore (ScoreEnv {bpm = 144}) score
  where
    score = together [scoreLittleFugueInGMinor1, scoreLittleFugueInGMinor2]

scoreLittleFugueInGMinor1 :: (Score m) => m ()
scoreLittleFugueInGMinor1 = do
  tellNote (-2) quarter
  tellNote 5 quarter
  tellNote 1 (dotted quarter)
  tellNote 0 eighth
  --
  tellNote (-2) eighth
  tellNote 1 eighth
  tellNote 0 eighth
  tellNote (-2) eighth
  tellNote (-3) eighth
  tellNote 0 eighth
  tellNote (-7) quarter
  --
  tellNote (-2) eighth
  tellNote (-7) eighth
  tellNote 0 eighth
  tellNote (-7) eighth
  tellNote 1 eighth
  tellNote 0 sixteenth
  tellNote (-2) sixteenth
  tellNote 0 eighth
  tellNote (-7) eighth
  --
  tellNote (-2) eighth
  tellNote (-7) sixteenth
  tellNote (-2) sixteenth
  tellNote 0 eighth
  tellNote (-7) sixteenth
  tellNote 0 sixteenth
  tellNote 1 eighth
  tellNote 0 sixteenth
  tellNote (-2) sixteenth
  tellNote 0 sixteenth
  tellNote (-7) sixteenth
  tellNote 5 sixteenth
  tellNote 3 sixteenth
  --
  tellNote 1 sixteenth
  tellNote 0 sixteenth
  tellNote (-2) sixteenth
  tellNote 1 sixteenth
  tellNote 0 sixteenth
  tellNote (-2) sixteenth
  tellNote (-3) sixteenth
  tellNote 0 sixteenth
  tellNote (-2) sixteenth
  tellNote (-7) sixteenth
  tellNote (-2) sixteenth
  tellNote 0 sixteenth
  tellNote 1 sixteenth
  tellNote 3 sixteenth
  tellNote 5 sixteenth
  tellNote 7 sixteenth
  --
  tellNote 8 sixteenth
  tellNote 7 sixteenth
  tellNote 5 sixteenth
  tellNote 8 sixteenth
  tellNote 7 sixteenth
  tellNote 5 sixteenth
  tellNote 4 sixteenth
  tellNote 7 sixteenth
  tellNote 5 eighth
  tellNote 0 eighth
  tellNote 5 eighth
  tellNote 7 eighth
  --
  tellNote 8 sixteenth
  tellNote 10 sixteenth
  tellNote 8 sixteenth
  tellNote 10 sixteenth
  do
    -- trill
    tellNote 10 thirtySecond
    tellNote 12 thirtySecond
    tellNote 10 thirtySecond
    tellNote 12 thirtySecond
  tellNote 8 sixteenth
  tellNote 10 sixteenth
  tellNote 12 sixteenth
  tellNote 10 sixteenth
  tellNote 12 sixteenth
  tellNote 13 sixteenth
  tellNote 12 sixteenth
  tellNote 10 sixteenth
  tellNote 8 sixteenth
  tellNote 7 sixteenth
  --
  tellNote 8 sixteenth
  tellNote 12 sixteenth
  tellNote 10 sixteenth
  tellNote 12 sixteenth
  tellNote 4 sixteenth
  tellNote 12 sixteenth
  tellNote 10 sixteenth
  tellNote 12 sixteenth
  tellNote 5 sixteenth
  tellNote 12 sixteenth
  tellNote 10 sixteenth
  tellNote 12 sixteenth
  tellNote 4 sixteenth
  tellNote 12 sixteenth
  tellNote 10 sixteenth
  tellNote 12 sixteenth
  --
  tellNote 8 sixteenth
  tellNote 5 sixteenth
  tellNote 4 sixteenth
  tellNote 5 sixteenth
  tellNote 10 sixteenth
  tellNote 5 sixteenth
  tellNote 4 sixteenth
  tellNote 5 sixteenth
  tellNote 12 sixteenth
  tellNote 5 sixteenth
  tellNote 4 sixteenth
  tellNote 5 sixteenth
  tellNote 10 sixteenth
  tellNote 5 sixteenth
  tellNote 4 sixteenth
  tellNote 5 sixteenth
  --
  tellNote 0 eighth
  tellNote 8 eighth
  tellNote (-2) eighth
  tellNote 7 eighth
  tellNote (-4) eighth
  tellNote 0 eighth
  tellNote 5 eighth
  tellNote 8 eighth
  --
  tellNote 6 eighth
  tellNote 12 eighth
  tellRest eighth
  tellNote 6 eighth
  tellNote 5 eighth
  tellNote 10 eighth
  tellRest eighth
  tellNote 5 eighth
  --
  tellNote 3 sixteenth
  tellNote 1 sixteenth
  tellNote 3 sixteenth
  tellNote 5 sixteenth
  tellNote 3 sixteenth
  tellNote 12 sixteenth
  tellNote 10 sixteenth
  tellNote 12 sixteenth
  tellNote 1 sixteenth
  tellNote 10 sixteenth
  tellNote 9 sixteenth
  tellNote 10 sixteenth
  tellNote 0 sixteenth
  tellNote 9 sixteenth
  tellNote 7 sixteenth
  tellNote 9 sixteenth
  --

scoreLittleFugueInGMinor2 :: (Score m) => m ()
scoreLittleFugueInGMinor2 = do
  tellRest whole
  --
  tellRest whole
  --
  tellRest whole
  --
  tellRest whole
  --
  tellRest whole
  --
  tellNote (-7) quarter
  tellNote 0 quarter
  tellNote (-4) (dotted quarter)
  tellNote (-5) eighth
  --
  tellNote (-7) eighth
  tellNote (-4) eighth
  tellNote (-5) eighth
  tellNote (-7) eighth
  tellNote (-8) eighth
  tellNote (-5) eighth
  tellNote (-12) quarter
  --
  tellNote (-7) eighth
  tellNote (-12) eighth
  tellNote (-5) eighth
  tellNote (-12) eighth
  tellNote (-4) eighth
  tellNote (-5) sixteenth
  tellNote (-7) sixteenth
  tellNote (-5) eighth
  tellNote (-12) eighth
  --
  tellNote (-7) eighth
  tellNote (-12) sixteenth
  tellNote (-7) sixteenth
  tellNote (-5) eighth
  tellNote (-12) sixteenth
  tellNote (-5) sixteenth
  tellNote (-4) eighth
  tellNote (-5) sixteenth
  tellNote (-7) sixteenth
  tellNote (-5) sixteenth
  tellNote (-12) sixteenth
  tellNote 0 sixteenth
  tellNote (-2) sixteenth
  --
  tellNote (-4) sixteenth
  tellNote (-5) sixteenth
  tellNote (-7) sixteenth
  tellNote (-4) sixteenth
  tellNote (-5) sixteenth
  tellNote (-7) sixteenth
  tellNote (-8) sixteenth
  tellNote (-5) sixteenth
  tellNote (-7) sixteenth
  tellNote (-12) sixteenth
  tellNote (-7) sixteenth
  tellNote (-5) sixteenth
  tellNote (-4) sixteenth
  tellNote (-2) sixteenth
  tellNote 0 sixteenth
  tellNote 2 sixteenth
  --
  tellNote 3 sixteenth
  tellNote 1 sixteenth
  tellNote 3 sixteenth
  tellNote 5 sixteenth
  tellNote 3 sixteenth
  tellNote 1 sixteenth
  tellNote 0 sixteenth
  tellNote 3 sixteenth
  tellNote 1 sixteenth
  tellNote 0 sixteenth
  tellNote 1 sixteenth
  tellNote 3 sixteenth
  tellNote 1 sixteenth
  tellNote 0 sixteenth
  tellNote (-2) sixteenth
  tellNote 1 sixteenth
  --
  tellNote 0 eighth
  tellNote (-2) eighth
  tellNote (-3) eighth
  tellNote (-7) eighth
  tellNote (-2) quarter
  tellNote 5 quarter
  --
