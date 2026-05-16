module Herald.Sound.IO where

import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as B
import Data.Foldable
import Data.Functor
import Herald.Sound
import System.Process
import Text.Printf

serialize :: Sound -> B.ByteString
serialize = BB.toLazyByteString . fold . map BB.floatLE

save :: FilePath -> Sound -> IO ()
save filePath = B.writeFile filePath . serialize

play :: FilePath -> IO ()
play filePath = void $ do
  let cmd = printf "ffplay -autoexit -showmode 1 -f f32le -ar %f %s" sampleRate filePath
  system cmd
