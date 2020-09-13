module Main

import Data.Buffer
import System
import Printf

%default total

{- Type aliases -}

Hz : Type
Hz = Double

Seconds : Type
Seconds = Double

Bytes : Type
Bytes = Int

{- Constants -}

sampleRate : Hz
sampleRate = 48000.0

sampleSize : Bytes
sampleSize = 8

a440 : Hz
a440 = 440.0

outFilePath : String
outFilePath = "wave.bin"

playCommand : String
playCommand = printf "ffplay -showmode 1 -f f64le -ar %f %s" sampleRate outFilePath

{- Logic -}

wave : (pitch: Hz) ->
       (volume : Double) ->
       (duration : Seconds)
       -> IO (Either String Buffer)
wave pitch volume duration =
  do
    -- allocate buffer
    Just buf <- newBuffer bufferSize
    | pure (Left "Failed to allocate buffer")

    -- fill the buffer with samples
    makeWave sampleCount buf

  where
    -- calculate buffer size
    sampleCount : Int
    sampleCount = cast (sampleRate * duration)

    bufferSize : Bytes
    bufferSize = sampleCount * sampleSize

    step : (pitch : Hz) -> Double
    step pitch = (pitch * 2.0 * pi) / sampleRate

    makeWave : (cur : Int) ->
               Buffer ->
               IO (Either String Buffer)
    makeWave 0 buf = pure (Right buf)
    makeWave cur buf =
      do
        let delta : Int = sampleCount - cur
        let loc : Int = delta * sampleSize
        let sample : Double = ((* volume) . sin . (* (step pitch)) . cast) delta
        setDouble buf loc sample
        makeWave (assert_smaller cur (cur - 1)) buf

saveBuffer : Buffer -> (filePath : String) -> IO (Either FileError Buffer)
saveBuffer buf filePath =
  do
    Right file <- openFile filePath WriteTruncate
    | Left err => pure (Left err)
    size <- rawSize buf
    newBuf <- writeBufferToFile file buf size
    pure (Right newBuf)

main : IO ()
main = do
  let pitch : Hz = a440 * 2
  let volume : Double = 0.5
  let duration : Seconds = 1.0
  Right buf <- wave pitch volume duration
  | Left err => putStrLn err
  Right _ <- saveBuffer buf outFilePath
  | Left err => putStrLn (show err)
  putStrLn "Saved"
  v <- system playCommand
  putStrLn $ "Played: " ++ (show v)
