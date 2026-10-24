module Main (main) where

import Demo.Music.HappyBirthday
import Demo.Music.LittleFugueInGMinor
import Herald.Sound
import Herald.Sound.IO
import System.Environment
import System.Exit
import System.IO

songs :: [(String, Sound)]
songs =
  [ ("happy-birthday", songHappyBirthday),
    ("little-fugue", songLittleFugueInGMinor)
  ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["play", name] -> play =<< findSong name
    ["wav", name, path] -> saveWav path =<< findSong name
    _ -> usage

findSong :: String -> IO Sound
findSong name = case lookup name songs of
  Just song -> pure song
  Nothing -> do
    hPutStrLn stderr $ "unknown song: " ++ name
    usage

usage :: IO a
usage = do
  prog <- getProgName
  hPutStr stderr . unlines $
    [ "usage:",
      "  " ++ prog ++ " play <song>",
      "  " ++ prog ++ " wav <song> <file.wav>",
      "",
      "songs:"
    ]
      ++ (("  " ++) . fst <$> songs)
  exitFailure
