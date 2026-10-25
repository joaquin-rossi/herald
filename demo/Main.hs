module Main (main) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Demo.HappyBirthday
import Demo.LittleFugueInGMinor
import Herald
import System.Environment
import System.Exit
import System.IO

data Song = Song
  { defaultInstruments :: [String],
    -- | 'Nothing' unless given one instrument per default.
    render :: [Instrument] -> Maybe (Either ScoreError AnyAudio)
  }

data AnyAudio = forall a. (Audio a) => AnyAudio a

songs :: Map String Song
songs =
  Map.fromList
    [ ( "happy-birthday",
        Song ["harpsichord", "organ"] $ \case
          [melody, chords] -> Just (AnyAudio <$> songHappyBirthday melody chords)
          _ -> Nothing
      ),
      ( "little-fugue",
        Song ["pure", "additive"] $ \case
          [upper, lower] -> Just (AnyAudio <$> songLittleFugueInGMinor upper lower)
          _ -> Nothing
      )
    ]

instruments :: Map String Instrument
instruments =
  Map.fromList
    [ ("pure", pureTone),
      ("additive", additive),
      ("organ", organ),
      ("harpsichord", harpsichord)
    ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    "play" : name : insts -> renderSong name insts >>= \(AnyAudio a) -> play a
    "wav" : name : path : insts -> renderSong name insts >>= \(AnyAudio a) -> saveWav path a
    _ -> usage

renderSong :: String -> [String] -> IO AnyAudio
renderSong name insts = do
  song <- find "song" name songs
  let names = if null insts then defaultInstruments song else insts
  is <- mapM (\i -> find "instrument" i instruments) names
  case render song is of
    Just (Right sound) -> pure sound
    Just (Left err) -> do
      hPutStrLn stderr $ name ++ ": " ++ displayScoreError err
      exitFailure
    Nothing -> do
      let n = length (defaultInstruments song)
      hPutStrLn stderr $ name ++ " takes 0 or " ++ show n ++ " instruments, got " ++ show (length insts)
      usage

find :: String -> String -> Map String a -> IO a
find kind name table = case Map.lookup name table of
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
      ++ ["  " ++ name ++ " (" ++ unwords (defaultInstruments song) ++ ")" | (name, song) <- Map.toList songs]
      ++ ["", "instruments:"]
      ++ (("  " ++) <$> Map.keys instruments)
  exitFailure
