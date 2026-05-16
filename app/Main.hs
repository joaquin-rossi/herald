module Main (main) where

import Herald.Music
import Herald.Sound
import Herald.Sound.IO

main :: IO ()
main = do
  let _song = songHappyBirthday
  let song = songFugueInGMinor

  let outputFilePath :: FilePath
      outputFilePath = "/tmp/sound.bin"
  save outputFilePath song
  play outputFilePath

songHappyBirthday :: Sound
songHappyBirthday = (* 0.5) <$> runScore (ScoreEnv {bpm = 99}) scoreHappyBirthday

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

songFugueInGMinor :: Sound
songFugueInGMinor = (* 0.5) <$> runScore (ScoreEnv {bpm = 72}) scoreFugueInGMinor

scoreFugueInGMinor :: (Score m) => m ()
scoreFugueInGMinor = do
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
