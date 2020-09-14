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

Samples : Type
Samples = Int

Volume : Type
Volume = Double

Bytes : Type
Bytes = Int

Semitone : Type
Semitone = Int

MidiNote : Type
MidiNote = Int

BPM : Type
BPM = Double

{- Constants -}

sampleRate : Hz
sampleRate = 48000.0

sampleSize : Bytes
sampleSize = 8

a440 : Hz
a440 = 440.0

midiA4 : Int
midiA4 = 69

tempo : BPM
tempo = 104.0

outFilePath : String
outFilePath = "wave.bin"

playCommand : String
playCommand = printf "ffplay -showmode 1 -f f64le -ar %f %s" sampleRate outFilePath

{- Derived Constants -}

quarterNote : Seconds
quarterNote = 60.0 / tempo

halfNote : Seconds
halfNote = quarterNote * 2.0

wholeNote : Seconds
wholeNote = halfNote * 2.0

eigthNote : Seconds
eigthNote = quarterNote * 0.5

sixteenthNote : Seconds
sixteenthNote = eigthNote * 0.5

dottedQuarterNote : Seconds
dottedQuarterNote = quarterNote + eigthNote

dottedEigthNote : Seconds
dottedEigthNote = eigthNote + sixteenthNote

dottedSixteenthNote : Seconds
dottedSixteenthNote = sixteenthNote + (sixteenthNote * 0.5)

tripletQuarterNote : Seconds
tripletQuarterNote = quarterNote - (quarterNote * (1/3))

tripletEigthNote : Seconds
tripletEigthNote = eigthNote - (eigthNote * (1/3))

tripletSixteenthNote : Seconds
tripletSixteenthNote = sixteenthNote - (sixteenthNote * (1/3))

{- Data types -}

record Event where
  constructor MkEvent
  midiNote  : MidiNote
  volume    : Volume
  duration  : Seconds

Show Event where
  show (MkEvent midiNote volume duration) =
    printf "Note: %d Vol: %f Dur: %f" midiNote volume duration

{- Data -}

sequence : List Event
sequence = [
  {- Bar 1 -}
  MkEvent 67 0.5 eigthNote,
  MkEvent  0 0.0 eigthNote,
  MkEvent 67 0.5 eigthNote,
  MkEvent  0 0.0 eigthNote,
  MkEvent 67 0.5 eigthNote,
  MkEvent  0 0.0 eigthNote,
  MkEvent 63 0.3 dottedEigthNote,
  MkEvent 70 0.6 sixteenthNote,
  {- Bar 2 -}
  MkEvent 67 0.5 eigthNote,
  MkEvent  0 0.0 eigthNote,
  MkEvent 63 0.3 dottedEigthNote,
  MkEvent 70 0.6 sixteenthNote,
  MkEvent 67 0.5 quarterNote,
  MkEvent  0 0.0 quarterNote,
  {- Bar 3 -}
  MkEvent 74 0.2 eigthNote,
  MkEvent  0 0.0 eigthNote,
  MkEvent 74 0.2 eigthNote,
  MkEvent  0 0.0 eigthNote,
  MkEvent 74 0.2 eigthNote,
  MkEvent  0 0.0 eigthNote,
  MkEvent 75 0.7 dottedEigthNote,
  MkEvent 70 0.6 sixteenthNote,
  {- Bar 4 -}
  MkEvent 66 0.5 eigthNote,
  MkEvent  0 0.0 eigthNote,
  MkEvent 63 0.7 dottedEigthNote,
  MkEvent 70 0.6 sixteenthNote,
  MkEvent 67 0.5 quarterNote,
  MkEvent  0 0.0 quarterNote
]

{- Helpers -}

-- formula for converting a semitone distance from A4
noteFrequency : Semitone -> Hz
noteFrequency n = a440 * (pow a (the Double (cast n)))
  where
    a : Double
    a = pow 2 (1 / 12)

-- midi note to note frequency
midiFrequency : MidiNote -> Hz
midiFrequency note = noteFrequency (note - midiA4)

-- calculate the number of samples for the given duration
durationSamples : Seconds -> Samples
durationSamples duration = cast (sampleRate * duration)

-- calculate the size in bytes for the given duration
durationSize : Seconds -> Bytes
durationSize duration = (durationSamples duration) * sampleSize

-- calculate the total duration of the given sequence
calcDuration : List Event -> Seconds
calcDuration [] = 0.0
calcDuration ((MkEvent _ _ duration) :: xs) = duration + calcDuration xs

{- Major functions -}

-- fill a buffer with PCM data for a note event
wave : Buffer -> (start : Seconds) -> Event -> IO ()
wave buf start event@(MkEvent midiNote volume duration) =
  do

    putStrLn $ printf "Rendering @ %f: %s" start (show event)

    -- fill the buffer with samples (count down)
    makeWave sampleCount buf

  where

    sampleCount : Samples
    sampleCount = durationSamples duration

    offset : Bytes
    offset = durationSize start

    pitch : Hz
    pitch = midiFrequency midiNote

    step : Double
    step = (pitch * 2.0 * pi) / sampleRate

    makeWave : (cur : Samples) ->
               Buffer ->
               IO ()
    makeWave 0 buf = pure ()
    makeWave cur buf =
      do
        let delta  : Int = sampleCount - cur
        let sample : Double = ((* volume) . sin . (* step) . cast) delta
        let loc    : Int = offset + (delta * sampleSize)
        setDouble buf loc sample
        makeWave (assert_smaller cur (cur - 1)) buf

-- render the sequence of note events into the buffer at the start time
render : List Event -> Buffer -> (start : Seconds) -> IO ()
render [] buf _ = pure ()
render (event@(MkEvent midiNote volume duration) :: xs) buf start =
  do
    wave buf start event
    render xs buf (start + duration)

-- save the buffer to the file path
saveBuffer : Buffer -> (filePath : String) -> IO (Either FileError Buffer)
saveBuffer buf filePath =
  do
    Right file <- openFile filePath WriteTruncate
    | Left err => pure (Left err)
    size <- rawSize buf
    newBuf <- writeBufferToFile file buf size
    pure (Right newBuf)

{- Main -}

main : IO ()
main = do

  -- calculate the total duration of the sequence
  let totalDuration : Seconds = calcDuration sequence

  -- convert
  let bufferSize : Bytes = durationSize totalDuration

  -- allocate buffer
  Just buf <- newBuffer bufferSize
  | putStrLn "Failed to allocate buffer"

  -- render the event sequence into the buffer
  render sequence buf 0.0

  -- save the buffer to a file
  Right _ <- saveBuffer buf outFilePath
  | Left err => putStrLn (show err)

  -- play the sound file
  v <- system playCommand

  pure ()
