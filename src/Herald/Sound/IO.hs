-- |
-- Module      : Herald.Sound.IO
-- Description : Encoding sounds as WAV files and playing them.
--
-- Sounds are written as mono 16-bit PCM WAV at 'sampleRate'. Samples outside
-- @[-1, 1]@ are clipped.
module Herald.Sound.IO
  ( encodeWav,
    saveWav,
    play,
  )
where

import Control.Exception
import Control.Monad
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as B
import Data.Int
import Data.Vector.Unboxed qualified as V
import Data.Word
import GHC.IO.Exception (IOErrorType (..), IOException (..))
import Herald.Sound
import System.Exit
import System.IO (hClose)
import System.Process

-- | Encode as a mono 16-bit PCM WAV file.
encodeWav :: Sound -> B.ByteString
encodeWav sound = BB.toLazyByteString (header <> samples)
  where
    samples = V.foldr (\x b -> BB.int16LE (pcm x) <> b) mempty sound
    rate = round sampleRate :: Word32
    dataSize = fromIntegral (2 * V.length sound) :: Word32
    header =
      mconcat
        [ BB.string7 "RIFF",
          BB.word32LE (36 + dataSize),
          BB.string7 "WAVE",
          BB.string7 "fmt ",
          BB.word32LE 16, -- fmt chunk size
          BB.word16LE 1, -- PCM
          BB.word16LE 1, -- mono
          BB.word32LE rate,
          BB.word32LE (rate * 2), -- byte rate
          BB.word16LE 2, -- block align
          BB.word16LE 16, -- bits per sample
          BB.string7 "data",
          BB.word32LE dataSize
        ]
    pcm x = round (32767 * max (-1) (min 1 x)) :: Int16

-- | Write a sound to a WAV file.
saveWav :: FilePath -> Sound -> IO ()
saveWav filePath = B.writeFile filePath . encodeWav

-- | Play a sound through @ffplay@ (from FFmpeg), which must be on the @PATH@,
-- streaming the WAV over its standard input. Throws if @ffplay@ fails;
-- quitting it early is not an error.
play :: Sound -> IO ()
play sound = do
  let args = ["-autoexit", "-showmode", "1", "-"]
  putStrLn $ "$ " ++ unwords ("ffplay" : args)
  withCreateProcess (proc "ffplay" args) {std_in = CreatePipe} $ \stdin' _ _ ph -> do
    -- ffplay closes the pipe early if the user quits; that's not an error.
    forM_ stdin' $ \h -> ignoreBrokenPipe $ B.hPut h (encodeWav sound) >> hClose h
    code <- waitForProcess ph
    case code of
      ExitSuccess -> pure ()
      ExitFailure n -> ioError . userError $ "ffplay exited with code " ++ show n
  where
    ignoreBrokenPipe = handle $ \e ->
      unless (ioe_type e == ResourceVanished) (throwIO e)
