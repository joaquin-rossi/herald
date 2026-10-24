module Main (main) where

import Demo.Music.HappyBirthday
import Demo.Music.LittleFugueInGMinor
import Herald
import System.Environment
import System.Exit
import System.IO

-- | A song and the instruments it plays with when none are given.
data Song = Song
  { defaultInstruments :: [String],
    -- | 'Nothing' unless given one instrument per default.
    render :: [Instrument] -> Maybe Sound
  }

songs :: [(String, Song)]
songs =
  [ ( "happy-birthday",
      Song ["additive"] $ \case
        [i] -> Just (songHappyBirthday i)
        _ -> Nothing
    ),
    ( "little-fugue",
      Song ["organ", "harpsichord"] $ \case
        [upper, lower] -> Just (songLittleFugueInGMinor upper lower)
        _ -> Nothing
    )
  ]

instruments :: [(String, Instrument)]
instruments =
  [ ("pure", pureTone),
    ("additive", additive),
    ("organ", organ),
    ("harpsichord", harpsichord)
  ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    "play" : name : insts -> play =<< renderSong name insts
    "wav" : name : path : insts -> saveWav path =<< renderSong name insts
    _ -> usage

renderSong :: String -> [String] -> IO Sound
renderSong name insts = do
  song <- find "song" name songs
  let names = if null insts then defaultInstruments song else insts
  is <- mapM (\i -> find "instrument" i instruments) names
  case render song is of
    Just sound -> pure sound
    Nothing -> do
      let n = length (defaultInstruments song)
      hPutStrLn stderr $ name ++ " takes 0 or " ++ show n ++ " instruments, got " ++ show (length insts)
      usage

find :: String -> String -> [(String, a)] -> IO a
find kind name table = case lookup name table of
  Just x -> pure x
  Nothing -> do
    hPutStrLn stderr $ "unknown " ++ kind ++ ": " ++ name
    usage

usage :: IO a
usage = do
  prog <- getProgName
  hPutStr stderr . unlines $
    [ "usage:",
      "  " ++ prog ++ " play <song> [instrument...]",
      "  " ++ prog ++ " wav <song> <file.wav> [instrument...]",
      "",
      "songs (default instruments):"
    ]
      ++ ["  " ++ name ++ " (" ++ unwords (defaultInstruments song) ++ ")" | (name, song) <- songs]
      ++ ["", "instruments:"]
      ++ (("  " ++) . fst <$> instruments)
  exitFailure
