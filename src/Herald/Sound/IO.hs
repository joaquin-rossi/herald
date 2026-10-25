-- | 16-bit PCM WAV output, and playback through @ffplay@.
module Herald.Sound.IO
  ( Audio (..),
    encodeWav,
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

class Audio a where
  channels :: a -> [Sound]

instance Audio (V.Vector Double) where
  channels s = [s]

instance Audio Stereo where
  channels s = [leftChannel s, rightChannel s]

-- | Samples outside @[-1, 1]@ are clipped.
encodeWav :: (Audio a) => a -> B.ByteString
encodeWav audio = BB.toLazyByteString (header <> samples)
  where
    chs = channels audio
    n = length chs
    frames = maximum (0 : map V.length chs)
    sample s i = if i < V.length s then V.unsafeIndex s i else 0
    samples = foldr (\i b -> foldr (\s b' -> BB.int16LE (pcm (sample s i)) <> b') b chs) mempty [0 .. frames - 1]
    rate = round sampleRate :: Word32
    blockAlign = fromIntegral (2 * n) :: Word16
    dataSize = fromIntegral (2 * n * frames) :: Word32
    header =
      mconcat
        [ BB.string7 "RIFF",
          BB.word32LE (36 + dataSize),
          BB.string7 "WAVE",
          BB.string7 "fmt ",
          BB.word32LE 16, -- fmt chunk size
          BB.word16LE 1, -- PCM
          BB.word16LE (fromIntegral n), -- channels
          BB.word32LE rate,
          BB.word32LE (rate * fromIntegral blockAlign), -- byte rate
          BB.word16LE blockAlign,
          BB.word16LE 16, -- bits per sample
          BB.string7 "data",
          BB.word32LE dataSize
        ]
    pcm x = round (32767 * max (-1) (min 1 x)) :: Int16

saveWav :: (Audio a) => FilePath -> a -> IO ()
saveWav filePath = B.writeFile filePath . encodeWav

-- | Requires @ffplay@ on the @PATH@.
play :: (Audio a) => a -> IO ()
play audio = do
  let args = ["-autoexit", "-showmode", "1", "-"]
  withCreateProcess (proc "ffplay" args) {std_in = CreatePipe} $ \stdin' _ _ ph -> do
    -- ffplay closes the pipe early if the user quits; that's not an error.
    forM_ stdin' $ \h -> ignoreBrokenPipe $ B.hPut h (encodeWav audio) >> hClose h
    code <- waitForProcess ph
    case code of
      ExitSuccess -> pure ()
      ExitFailure c -> ioError . userError $ "ffplay exited with code " ++ show c
  where
    ignoreBrokenPipe = handle $ \e ->
      unless (ioe_type e == ResourceVanished) (throwIO e)
