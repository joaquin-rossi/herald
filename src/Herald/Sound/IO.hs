module Herald.Sound.IO where

import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as B
import Herald.Sound
import System.Process

serialize :: Sound -> B.ByteString
serialize = BB.toLazyByteString . foldMap (BB.floatLE . realToFrac . clamp)
  where
    clamp = max (-1) . min 1

save :: FilePath -> Sound -> IO ()
save filePath = B.writeFile filePath . serialize

play :: FilePath -> IO ()
play filePath = do
  let args = ["-autoexit", "-showmode", "1", "-f", "f32le", "-ar", show (round sampleRate :: Int), filePath]
  putStrLn $ "$ " ++ unwords ("ffplay" : args)
  callProcess "ffplay" args
