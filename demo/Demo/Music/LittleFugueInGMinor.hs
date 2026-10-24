module Demo.Music.LittleFugueInGMinor
  ( songLittleFugueInGMinor,
  )
where

import Herald.Instrument
import Herald.Music
import Herald.Music.Pitch
import Herald.Sound

songLittleFugueInGMinor :: Sound
songLittleFugueInGMinor = gain 0.5 $ playScore (ScoreEnv {bpm = 144, instrument = additive}) score
  where
    score =
      together
        [ withInstrument organ scoreLittleFugueInGMinor1,
          withInstrument harpsichord scoreLittleFugueInGMinor2
        ]

scoreLittleFugueInGMinor1 :: (Score m) => m ()
scoreLittleFugueInGMinor1 = do
  tellNotes quarter [g4, d5]
  tellNote (dotted quarter) bb4
  tellNote eighth a4
  --
  tellNotes eighth [g4, bb4, a4, g4, fs4, a4]
  tellNote quarter d4
  --
  tellNotes eighth [g4, d4, a4, d4, bb4]
  tellNotes sixteenth [a4, g4]
  tellNotes eighth [a4, d4]
  --
  tellNote eighth g4
  tellNotes sixteenth [d4, g4]
  tellNote eighth a4
  tellNotes sixteenth [d4, a4]
  tellNote eighth bb4
  tellNotes sixteenth [a4, g4, a4, d4, d5, c5]
  --
  tellNotes sixteenth [bb4, a4, g4, bb4, a4, g4, fs4, a4, g4, d4, g4, a4, bb4, c5, d5, e5]
  --
  tellNotes sixteenth [f5, e5, d5, f5, e5, d5, cs5, e5]
  tellNotes eighth [d5, a4, d5, e5]
  --
  tellNotes sixteenth [f5, g5, f5, g5]
  tellTrill eighth 4 g5 a5
  tellNotes sixteenth [f5, g5, a5, g5, a5, bb5, a5, g5, f5, e5]
  --
  tellNotes sixteenth [f5, a5, g5, a5, cs5, a5, g5, a5, d5, a5, g5, a5, cs5, a5, g5, a5]
  --
  tellNotes sixteenth [f5, d5, cs5, d5, g5, d5, cs5, d5, a5, d5, cs5, d5, g5, d5, cs5, d5]
  --
  tellNotes eighth [a4, f5, g4, e5, f4, a4, d5, f5]
  --
  tellNotes eighth [eb5, a5]
  tellRest eighth
  tellNotes eighth [eb5, d5, g5]
  tellRest eighth
  tellNote eighth d5
  --
  tellNotes sixteenth [c5, bb4, c5, d5, c5, a5, g5, a5, bb4, g5, fs5, g5, a4, fs5, e5, fs5]
  --

scoreLittleFugueInGMinor2 :: (Score m) => m ()
scoreLittleFugueInGMinor2 = do
  tellRest whole
  --
  tellRest whole
  --
  tellRest whole
  --
  tellRest whole
  --
  tellRest whole
  --
  tellNotes quarter [d4, a4]
  tellNote (dotted quarter) f4
  tellNote eighth e4
  --
  tellNotes eighth [d4, f4, e4, d4, cs4, e4]
  tellNote quarter a3
  --
  tellNotes eighth [d4, a3, e4, a3, f4]
  tellNotes sixteenth [e4, d4]
  tellNotes eighth [e4, a3]
  --
  tellNote eighth d4
  tellNotes sixteenth [a3, d4]
  tellNote eighth e4
  tellNotes sixteenth [a3, e4]
  tellNote eighth f4
  tellNotes sixteenth [e4, d4, e4, a3, a4, g4]
  --
  tellNotes sixteenth [f4, e4, d4, f4, e4, d4, cs4, e4, d4, a3, d4, e4, f4, g4, a4, b4]
  --
  tellNotes sixteenth [c5, bb4, c5, d5, c5, bb4, a4, c5, bb4, a4, bb4, c5, bb4, a4, g4, bb4]
  --
  tellNotes eighth [a4, g4, fs4, d4]
  tellNotes quarter [g4, d5]
  --
