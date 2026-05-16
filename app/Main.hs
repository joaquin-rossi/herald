module Main (main) where

import Herald.Sound.IO
import Main.Music.LittleFugueInGMinor

main :: IO ()
main = do
  let path = "/tmp/sound.bin" :: FilePath
  save path songLittleFugueInGMinor
  play path
