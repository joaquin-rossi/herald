module Demo.Music.LittleFugueInGMinor
  ( songLittleFugueInGMinor,
  )
where

import Herald

-- Two-voice arrangement transcribed from Jeff Bigler's ABC version:
-- https://www.mit.edu/~jcb/little-fugue.html
--
-- Copyright © 2003 by Jeff Bigler.  Permission is hereby granted to copy,
-- distribute, modify, and/or perform this music in any form as long as this
-- copyright notice is included.

-- | The fugue with an instrument for each voice, upper voice first.
songLittleFugueInGMinor :: Instrument -> Instrument -> Sound
songLittleFugueInGMinor upper lower =
  gain 0.5 $
    playScore
      defaultConfig {tempo = 144}
      [ withInstrument upper scoreLittleFugueInGMinor1,
        withInstrument lower scoreLittleFugueInGMinor2
      ]

scoreLittleFugueInGMinor1 :: (Score m) => m ()
scoreLittleFugueInGMinor1 = do
  setKey gMinor
  setMeter (Meter 4 4)
  -- bar 1
  tellNotes quarter [g4, d5]
  tellNote (dotted quarter) b4
  tellNote eighth a4
  -- bar 2
  tellBar
  tellNotes eighth [g4, b4, a4, g4, fs4, a4]
  tellNote quarter d4
  -- bar 3
  tellBar
  tellNotes eighth [g4, d4, a4, d4, b4]
  tellNotes sixteenth [a4, g4]
  tellNotes eighth [a4, d4]
  -- bar 4
  tellBar
  tellNote eighth g4
  tellNotes sixteenth [d4, g4]
  tellNote eighth a4
  tellNotes sixteenth [d4, a4]
  tellNote eighth b4
  tellNotes sixteenth [a4, g4, a4, d4, d5, c5]
  -- bar 5
  tellBar
  tellNotes sixteenth [b4, a4, g4, b4, a4, g4, fs4, a4, g4, d4, g4, a4, b4, c5, d5, natural e5]
  -- bar 6
  tellBar
  tellNotes sixteenth [f5, natural e5, d5, f5, e5, d5, cs5, e5]
  tellNotes eighth [d5, a4, d5, e5]
  -- bar 7
  tellBar
  tellNotes sixteenth [f5, g5, f5, g5]
  tellTrill (dotted eighth) 6 g5 a5
  tellNotes thirtySecond [f5, g5]
  tellNotes sixteenth [a5, g5, a5, b5, a5, g5, f5, natural e5]
  -- bar 8
  tellBar
  tellNotes sixteenth [f5, a5, g5, a5, cs5, a5, g5, a5, d5, a5, g5, a5, c5, a5, g5, a5]
  -- bar 9
  tellBar
  tellNotes sixteenth [f5, d5, cs5, d5, g5, d5, c5, d5, a5, d5, c5, d5, g5, d5, c5, d5]
  -- bar 10
  tellBar
  tellNotes eighth [a4, f5, g4, natural e5, f4, a4, d5, f5]
  -- bar 11
  tellBar
  tellNotes eighth [e5, a5]
  tellRest eighth
  tellNotes eighth [e5, d5, g5]
  tellRest eighth
  tellNote eighth d5
  -- bar 12
  tellBar
  tellNotes sixteenth [c5, b4, c5, d5, c5, a5, g5, a5, b4, g5, fs5, g5, a4, f5, natural e5, f5]
  -- bar 13
  tellBar
  tellNotes eighth [g5, d4, g4, a4]
  tellNotes sixteenth [b4, c5, b4, c5]
  tellTrill (dotted eighth) 6 c5 d5
  tellNotes thirtySecond [b4, c5]
  -- bar 14
  tellBar
  tellNotes sixteenth [d5, c5, d5, e5, d5, c5, b4, a4, b4, d5, c5, d5, fs4, d5, c5, d5]
  -- bar 15
  tellBar
  tellNotes sixteenth [g4, d5, c5, d5, fs4, d5, c5, d5, b4, g4, f4, g4, c5, g4, f4, g4]
  -- bar 16
  tellBar
  tellNotes sixteenth [d5, g4, fs4, g4, c5, g4, f4, g4]
  tellNotes eighth [d4, b4, c4, a4]
  -- bar 17
  tellBar
  tellNotes eighth [b3, d4]
  tellNotes sixteenth [g4, a4, b4, g4]
  tellNotes quarter [d4, a4]
  -- bar 18
  tellBar
  tellNote (dotted quarter) f4
  tellNotes eighth [natural e4, d4, f4, e4, d4]
  -- bar 19
  tellBar
  tellNotes eighth [cs4, natural e4]
  tellNote quarter a3
  tellNotes eighth [d4, a3, e4, a3]
  -- bar 20
  tellBar
  tellNote eighth f4
  tellNotes sixteenth [natural e4, d4]
  tellNotes eighth [e4, a3, d4]
  tellNotes sixteenth [a3, d4]
  tellNote eighth e4
  tellNotes sixteenth [a3, e4]
  -- bar 21
  tellBar
  tellNote eighth f4
  tellNotes sixteenth [natural e4, d4]
  tellNotes eighth [e4, a3, d4, g3]
  tellNote quarter a3
  -- bar 22
  tellBar
  tellNotes eighth [d4, a4, d5, c5, b4, d5, g5, f5]
  -- bar 23
  tellBar
  tellNotes eighth [e5, g4, c5, b4, a4, c5, f5, e5]
  -- bar 24
  tellBar
  tellNote (quarter + sixteenth) d5
  tellNotes sixteenth [g5, fs5, g5]
  tellNote (quarter + sixteenth) c5
  tellNotes sixteenth [b4, a4, c5]
  -- bar 25
  tellBar
  tellNotes sixteenth [b4, a4, g4, b4, a4, g4, fs4, a4, g4, d4, natural e4, f4, g4, d4, g4, a4]
  -- bar 26
  tellBar
  tellNotes sixteenth [b4, g4, b4, c5, d5, a4, d5, c5]
  tellNote (dotted quarter) b4
  tellNote eighth a4
  -- bar 27
  tellBar
  tellNotes eighth [g4, b4, a4, g4, fs4, a4]
  tellNote quarter d4
  -- bar 28
  tellBar
  tellNotes eighth [g4, d4, a4, d4, b4]
  tellNotes sixteenth [a4, g4]
  tellNotes eighth [a4, d4]
  -- bar 29
  tellBar
  tellNote eighth g4
  tellNotes sixteenth [d4, g4]
  tellNote eighth a4
  tellNotes sixteenth [d4, a4]
  tellNote eighth b4
  tellNotes sixteenth [a4, g4, a4, d4, d5, c5]
  -- bar 30
  tellBar
  tellNotes sixteenth [b4, a4, g4, b4, a4, g4, fs4, a4, g4, b4, c5, d5, e5, b4, a4, g4]
  -- bar 31
  tellBar
  tellNotes sixteenth [fs4, a4, b4, c5, d5, a4, g4, natural f4, e4, g4, a4, b4, c5, g4, f4, e4]
  -- bar 32
  tellBar
  tellNotes sixteenth [d4, f4, g4, a4, b4, d5, c5, b4, a4, c5, d5, e5, f5, g5, f5, e5]
  -- bar 33
  tellBar
  tellNotes sixteenth [d5, f5, e5, d5, c5, b4, a4, c5, b4, f4, g4, a4, b4, c5, b4, c5]
  -- bar 34
  tellBar
  tellNotes sixteenth [d5, natural e5, d5, e5]
  tellTrill (dotted eighth) 6 e5 f5
  tellNotes thirtySecond [d5, e5]
  tellNotes sixteenth [f5, eb5, f5, g5, f5, e5, d5, c5]
  -- bar 35
  tellBar
  tellNotes sixteenth [d5, f5, e5, f5, a4, f5, e5, f5, b4, f5, e5, f5, a4, f5, e5, f5]
  -- bar 36
  tellBar
  tellNotes sixteenth [d5, b4, a4, b4, e5, b4, a4, b4, f5, b4, a4, b4, e5, b4, a4, b4]
  -- bar 37
  tellBar
  tellNotes sixteenth [b4, c5, d5, b4, e5, d5, c5, e5, d5, c5, d5, e5, d5, c5, b4, d5]
  -- bar 38
  tellBar
  tellNotes sixteenth [c5, b4, c5, d5, c5, b4, a4, c5, b4, a4, b4, c5, b4, a4, g4, b4]
  -- bar 39
  tellBar
  tellNotes eighth [a4, f5]
  tellRest eighth
  tellNotes eighth [ab4, g4, e5]
  tellRest eighth
  tellNote eighth g4
  -- bar 40
  tellBar
  tellNotes eighth [f4, d5]
  tellRest eighth
  tellNote eighth f4
  tellNotes sixteenth [e4, g4, c5, b4, a4, g4, f4, e4]
  -- bar 41
  tellBar
  tellNotes sixteenth [d4, e4, f4, g4, a4, b4, c5, a4, b4, f4, g4, a4, b4, c5, b4, c5]
  -- bar 42
  tellBar
  tellNotes sixteenth [d5, natural e5, d5, e5]
  tellTrill (dotted eighth) 6 e5 f5
  tellNotes thirtySecond [d5, e5]
  tellNotes sixteenth [f5, e5, f5, g5, f5, g5, f5, eb5]
  -- bar 43
  tellBar
  tellNotes sixteenth [d5, f5, e5, f5, a4, f5, e5, f5, b4, f5, e5, f5, a4, f5, e5, f5]
  -- bar 44
  tellBar
  tellNotes sixteenth [d5, b5, a5, b5, e5, b5, a5, b5, f5, b5, a5, b5, e5, b5, a5, b5]
  -- bar 45
  tellBar
  tellNotes sixteenth [f5, e5, f5, g5]
  tellNote (dotted eighth) c5
  tellNotes sixteenth [a5, b5, a5, b5, c6, b5, a5, g5, f5]
  -- bar 46
  tellBar
  tellNotes sixteenth [e5, d5, e5, f5, e5, d5, c5, b4, a5, g5, a5, b5, a5, g5, f5, e5]
  -- bar 47
  tellBar
  tellNotes sixteenth [d5, c5, d5, e5, d5, c5, b4, a4, g5, f5, g5, ab5, g5, f5, e5, d5]
  -- bar 48
  tellBar
  tellNotes sixteenth [c5, natural b4, c5, d5, c5, d5, e5, c5, f5, e5, d5, c5, b4, ab5, g5, f5]
  -- bar 49
  tellBar
  tellNote (half + sixteenth) e5
  tellNotes sixteenth [c5, d5, e5, f5, g5, ab5, f5]
  -- bar 50
  tellBar
  tellNotes sixteenth [natural b4, c5, d5, b4]
  tellNote eighth g4
  tellRest eighth
  tellNotes quarter [c5, g5]
  -- bar 51
  tellBar
  tellNote (dotted quarter) e5
  tellNotes eighth [d5, c5, e5, d5, c5]
  -- bar 52
  tellBar
  tellNotes eighth [natural b4, d5]
  tellNote quarter g4
  tellNotes eighth [c5, g4, d5, g4]
  -- bar 53
  tellBar
  tellNote eighth e5
  tellNotes sixteenth [d5, c5]
  tellNotes eighth [d5, g4, c5]
  tellNotes sixteenth [g4, c5]
  tellNote eighth d5
  tellNotes sixteenth [g4, d5]
  -- bar 54
  tellBar
  tellNote eighth e5
  tellNotes sixteenth [d5, c5, d5, g4, g5, f5, e5, d5, c5, e5, d5, c5, natural b4, d5]
  -- bar 55
  tellBar
  tellNotes sixteenth [c5, g5, e5, g5, c5, e5, g4, b4, a4, c5, a4, c5, f4, a4, c4, e4]
  -- bar 56
  tellBar
  tellNotes sixteenth [d4, f5, d5, f5, b4, d5, f4, a4, g4, b4, g4, b4, e4, g4, b3, d4]
  -- bar 57
  tellBar
  tellNotes sixteenth [c4, e5, c5, e5, a4, c5, natural e4, g4, fs4, a4, f4, a4, d4, f4, a3, c4]
  -- bar 58
  tellBar
  tellNotes eighth [b3, g4, a3, fs4, g4, d5, f4, d5]
  -- bar 59
  tellBar
  tellNotes sixteenth [g4, b4, a4, g4, d5, a4, d4, c5, b4, d5, c5, b4, f5, c5, f4, e5]
  -- bar 60
  tellBar
  tellNotes sixteenth [d5, f5, e5, d5, g5, d5, g4, f5, natural e5, g5, f5, e5, a5, e5, a4, g5]
  -- bar 61
  tellBar
  tellNotes sixteenth [fs5, e5, d5, c5, b4, d5, a4, d5, g4, ab5, g5, natural f5, g5, f5, e5, d5]
  -- bar 62
  tellBar
  tellNotes quarter [e5, a5]
  tellChord quarter [a5, d5]
  tellNote quarter g5
  -- bar 63
  tellBar
  tellChord quarter [g5, c5]
  tellNote quarter fs5
  tellNotes sixteenth [g3, b3, d4, g4, fs4, g4, natural e4, f4]
  -- bar 64
  tellBar
  tellNote (dotted quarter) g4
  tellNote eighth fs4
  tellNote quarter g4
  tellNotes eighth [a4, d4]
  -- bar 65
  tellBar
  tellNotes sixteenth [d4, g5, fs5, natural e5, d5, c5, b4, a4, b4, d5, c5, d5, fs4, d5, c5, d5]
  -- bar 66
  tellBar
  tellNotes sixteenth [g4, d5, c5, d5, fs4, d5, c5, d5, b4, g4, f4, g4, c5, g4, f4, g4]
  -- bar 67
  tellBar
  tellNotes sixteenth [d5, g4, fs4, g4, c5, g4, f4, g4]
  tellNotes eighth [b4, g5, a4, fs5]
  -- bar 68
  tellBar
  tellChord whole [g3, d4, natural b4, g5]

scoreLittleFugueInGMinor2 :: (Score m) => m ()
scoreLittleFugueInGMinor2 = do
  setKey gMinor
  setMeter (Meter 4 4)
  -- bar 1
  tellRest whole
  -- bar 2
  tellBar
  tellRest whole
  -- bar 3
  tellBar
  tellRest whole
  -- bar 4
  tellBar
  tellRest whole
  -- bar 5
  tellBar
  tellRest whole
  -- bar 6
  tellBar
  tellNotes quarter [d4, a4]
  tellNote (dotted quarter) f4
  tellNote eighth (natural e4)
  -- bar 7
  tellBar
  tellNotes eighth [d4, f4, natural e4, d4, cs4, e4]
  tellNote quarter a3
  -- bar 8
  tellBar
  tellNotes eighth [d4, a3, natural e4, a3, f4]
  tellNotes sixteenth [e4, d4]
  tellNotes eighth [e4, a3]
  -- bar 9
  tellBar
  tellNote eighth d4
  tellNotes sixteenth [a3, d4]
  tellNote eighth (natural e4)
  tellNotes sixteenth [a3, e4]
  tellNote eighth f4
  tellNotes sixteenth [e4, d4, e4, a3, a4, g4]
  -- bar 10
  tellBar
  tellNotes sixteenth [f4, natural e4, d4, f4, e4, d4, cs4, e4, d4, a3, d4, e4, f4, g4, a4, natural b4]
  -- bar 11
  tellBar
  tellNotes sixteenth [c5, b4, c5, d5, c5, b4, a4, c5, b4, a4, b4, c5, b4, a4, g4, b4]
  -- bar 12
  tellBar
  tellNotes eighth [a4, g4, fs4, d4]
  tellNotes quarter [g4, d5]
  -- bar 13
  tellBar
  tellNote (dotted quarter) b4
  tellNotes eighth [a4, g4, b4, a4, g4]
  -- bar 14
  tellBar
  tellNotes eighth [fs4, a4]
  tellNote quarter d4
  tellNotes eighth [g4, d4, a4, d4]
  -- bar 15
  tellBar
  tellNote eighth b4
  tellNotes sixteenth [a4, g4]
  tellNotes eighth [a4, d4, g4]
  tellNotes sixteenth [d4, g4]
  tellNote eighth a4
  tellNotes sixteenth [d4, a4]
  -- bar 16
  tellBar
  tellNote eighth b4
  tellNotes sixteenth [a4, g4, a4, d4, d5, c5, b4, a4, g4, b4, a4, g4, fs4, a4]
  -- bar 17
  tellBar
  tellNotes sixteenth [g4, d4, g4, a4, b4, c5, d5, natural e5, f5, e5, d5, f5, e5, d5, cs5, e5]
  -- bar 18
  tellBar
  tellNotes eighth [d5, a4, d5, natural e5]
  tellNotes sixteenth [f5, g5, f5, g5]
  tellTrill (dotted eighth) 6 g5 a5
  tellNotes thirtySecond [f5, g5]
  -- bar 19
  tellBar
  tellNotes sixteenth [a5, g5, a5, b5, a5, g5, f5, natural e5, f5, a5, g5, a5, cs5, a5, g5, a5]
  -- bar 20
  tellBar
  tellNotes sixteenth [d5, a5, g5, a5, cs5, a5, g5, a5, f5, d5, c5, d5, g5, d5, c5, d5]
  -- bar 21
  tellBar
  tellNotes sixteenth [a5, d5, cs5, d5, g5, d5, c5, d5]
  tellNote eighth f5
  tellNotes sixteenth [natural e5, d5]
  tellTrill (dotted eighth) 6 c5 d5
  tellNote sixteenth d5
  -- bar 22
  tellBar
  tellNotes sixteenth [d5, c5, d5, natural e5, d5, c5, b4, a4, g4, f4, g4, a4, g4, f4, e4, d4]
  -- bar 23
  tellBar
  tellNotes sixteenth [c5, b4, c5, d5, c5, b4, a4, g4, f4, e4, f4, g4, f4, e4, d4, c4]
  -- bar 24
  tellBar
  tellNotes sixteenth [b3, b4, c5, d5]
  tellNote (quarter + sixteenth) e5
  tellNotes sixteenth [a4, b4, c5]
  tellNote quarter d5
  -- bar 25
  tellBar
  tellNotes quarter [g4, d5]
  tellNote (dotted quarter) b4
  tellNote eighth a4
  -- bar 26
  tellBar
  tellNotes sixteenth [g4, b4, a4, g4, fs4, g4, natural e4, f4, g4, d4, e4, f4, g4, d4, g4, a4]
  -- bar 27
  tellBar
  tellNotes sixteenth [b4, c5, b4, c5]
  tellTrill (dotted eighth) 6 c5 d5
  tellNotes thirtySecond [b4, c5]
  tellNotes sixteenth [d5, c5, d5, e5, d5, c5, b4, a4]
  -- bar 28
  tellBar
  tellNotes sixteenth [b4, d5, c5, d5, fs4, d5, c5, d5, g4, d5, c5, d5, f4, d5, c5, d5]
  -- bar 29
  tellBar
  tellNotes sixteenth [b4, g4, fs4, g4, c5, g4, f4, g4, d5, g4, f4, g4, c5, g4, f4, g4]
  -- bar 30
  tellBar
  tellNotes eighth [g4, b4]
  tellNotes sixteenth [c5, b4, a4, c5]
  tellNote (quarter + sixteenth) b4
  tellNotes sixteenth [d5, c5, b4]
  -- bar 31
  tellBar
  tellNote (quarter + sixteenth) a4
  tellNotes sixteenth [c5, b4, a4]
  tellNote (quarter + sixteenth) g4
  tellNotes sixteenth [b4, a4, g4]
  -- bar 32
  tellBar
  tellNote (dotted quarter) f4
  tellNotes eighth [natural e4, f4, eb4, d4, c4]
  -- bar 33
  tellBar
  tellNotes quarter [b3, f4]
  tellNote (dotted quarter) d4
  tellNote eighth c4
  -- bar 34
  tellBar
  tellNotes eighth [b3, d4, c4, b3, a3, c4]
  tellNote quarter f4
  -- bar 35
  tellBar
  tellNotes eighth [b4, f4, c5, f4, d5]
  tellNotes sixteenth [c5, b4]
  tellNotes eighth [c5, f4]
  -- bar 36
  tellBar
  tellNote eighth b4
  tellNotes sixteenth [f4, b4]
  tellNote eighth c5
  tellNotes sixteenth [f4, c5]
  tellNote eighth d5
  tellNotes sixteenth [c5, b4, c5, f4, f5, e5]
  -- bar 37
  tellBar
  tellNotes sixteenth [d5, c5, b4, d5, c5, b4, a4, c5]
  tellNotes eighth [b4, g5]
  tellRest eighth
  tellNote eighth g4
  -- bar 38
  tellBar
  tellNotes eighth [a4, f5]
  tellRest eighth
  tellNotes eighth [f4, g4, f5, c5, natural e5]
  -- bar 39
  tellBar
  tellNotes sixteenth [f5, e5, f5, g5, f5, e5, d5, f5, e5, d5, e5, f5, e5, d5, c5, e5]
  -- bar 40
  tellBar
  tellNotes sixteenth [d5, c5, d5, e5, d5, c5, b4, d5, c5, b4, c5, d5, c5, b4, a4, c5]
  -- bar 41
  tellBar
  tellNotes quarter [b4, f5]
  tellNote (dotted quarter) d5
  tellNote eighth c5
  -- bar 42
  tellBar
  tellNotes eighth [b4, d5, c5, b4, a4, c5]
  tellNote quarter f4
  -- bar 43
  tellBar
  tellNotes eighth [b4, f4, c5, f4, d5]
  tellNotes sixteenth [c5, b4]
  tellNotes eighth [c5, f4]
  -- bar 44
  tellBar
  tellNote eighth b4
  tellNotes sixteenth [f4, b4]
  tellNote eighth c5
  tellNotes sixteenth [f4, c5]
  tellNote eighth d5
  tellNotes sixteenth [c5, b4]
  tellNotes eighth [c5, f4]
  -- bar 45
  tellBar
  tellNotes eighth [d5, e5, f5, f4, b4, f5, b5, a5]
  -- bar 46
  tellBar
  tellNotes eighth [g5, g4, c5, b4, a4, c5, a5, g5]
  -- bar 47
  tellBar
  tellNotes eighth [f5, f4, b4, a4, g4, b4, g5, f5]
  -- bar 48
  tellBar
  tellNotes eighth [e5, e4, ab4, g4]
  tellNote quarter f4
  tellTie quarter g4
  -- bar 49
  tellBar
  tellNote sixteenth g4
  tellNotes sixteenth [g4, a4, natural b4, c5, d5, e5, c5]
  tellNotes eighth [ab4, c5, ab5]
  tellTie eighth f5
  -- bar 50
  tellBar
  tellTie quarter f5
  tellNote sixteenth f5
  tellNotes sixteenth [f5, e5, d5, e5, d5, c5, e5, d5, c5, natural b4, d5]
  -- bar 51
  tellBar
  tellNotes sixteenth [c5, g4, c5, d5, e5, f5, e5, f5, g5, e5, c5, g5, ab5, f5, g5, a5]
  -- bar 52
  tellBar
  tellNotes sixteenth [d5, e5, d5, c5, natural b4, g4, f4, g4, e4, g4, f4, g4, natural b3, g4, f4, g4]
  -- bar 53
  tellBar
  tellNotes sixteenth [c4, g4, f4, g4, natural b3, g4, f4, g4, e4, c5, natural b4, c5, f4, c5, b4, c5]
  -- bar 54
  tellBar
  tellNotes sixteenth [g4, c5, natural b4, c5, g4, b4, a4, b4]
  tellNotes eighth [c4, e4, f4, g4]
  -- bar 55
  tellBar
  tellNotes eighth [c4, g4, e4, c4, f4, c5, a4, f4]
  -- bar 56
  tellBar
  tellNotes eighth [b4, f4, d4, b3, b4, b4, g4, e4]
  -- bar 57
  tellBar
  tellNotes eighth [a4, e4, c4, a3, a4, a4, fs4, d4]
  -- bar 58
  tellBar
  tellNotes sixteenth [g3, b3, a3, g3, d4, a3, d4, c4, g4, b4, a4, g4, d5, a4, d4, c5]
  -- bar 59
  tellBar
  tellNotes quarter [b4, fs4, g4, a4]
  -- bar 60
  tellBar
  tellNotes quarter [b4, natural b4, c5, cs5]
  -- bar 61
  tellBar
  tellNote quarter d5
  tellNotes eighth [natural e4, fs4]
  tellNote quarter g4
  tellNotes eighth [a4, b4]
  -- bar 62
  tellBar
  tellNotes sixteenth [c5, g4, f4, e4, c5, a4, f4, a4, b3, f4, e4, d4, b4, g4, e4, g4]
  -- bar 63
  tellBar
  tellNotes sixteenth [a3, e4, d4, c4, a4, fs4, d4, f4]
  tellNotes quarter [g4, d5]
  -- bar 64
  tellBar
  tellNote (dotted quarter) b4
  tellNotes eighth [a4, g4, b4, a4, g4]
  -- bar 65
  tellBar
  tellNotes eighth [fs4, a4]
  tellNote quarter d4
  tellNotes eighth [g4, d4, a4, d4]
  -- bar 66
  tellBar
  tellNote eighth b4
  tellNotes sixteenth [a4, g4]
  tellNotes eighth [a4, d4, g4]
  tellNotes sixteenth [d4, g4]
  tellNote eighth a4
  tellNotes sixteenth [d4, a4]
  -- bar 67
  tellBar
  tellNote eighth b4
  tellNotes sixteenth [a4, g4]
  tellNotes eighth [a4, d4, g4, e4, c4, d4]
  -- bar 68
  tellBar
  tellChord whole [g3, d4, natural b4]
