module Main.Music.HappyBirthday where

import Herald.Music
import Herald.Sound

songHappyBirthday :: Sound
songHappyBirthday = (* 0.25) <$> playScore (ScoreEnv {bpm = 99}) scoreHappyBirthday

scoreHappyBirthday :: (Score m) => m ()
scoreHappyBirthday = do
  tellRest 2
  tellNote (-2) 0.5
  tellNote (-2) 0.5
  --
  tellNote 0 1
  tellNote (-2) 1
  tellNote 3 1
  --
  tellNote 2 2
  tellNote (-2) 0.5
  tellNote (-2) 0.5
  --
  tellNote 0 1
  tellNote (-2) 1
  tellNote 5 1
  --
  tellNote 3 2
  tellNote (-2) 0.5
  tellNote (-2) 0.5
  --
  tellNote 10 1
  tellNote 7 1
  tellNote 3 1
  --
  tellNote 2 1
  tellNote 0 1
  tellNote 8 0.5
  tellNote 8 0.5
  --
  tellNote 7 1
  tellNote 3 1
  tellNote 5 1
  --
  tellNote 3 3
