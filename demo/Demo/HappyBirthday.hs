module Demo.HappyBirthday
  ( songHappyBirthday,
  )
where

import Herald

songHappyBirthday :: Instrument -> Instrument -> Either ScoreError Sound
songHappyBirthday melodyInstrument chordsInstrument =
  gain 0.5
    <$> playScoreMono
      defaultConfig {tempo = 99}
      [ (defaultVoice {instrument = melodyInstrument}, melody),
        (defaultVoice {instrument = chordsInstrument}, accompaniment)
      ]

melody :: (Score m) => m ()
melody = do
  setKey cMajor
  setMeter (Meter 3 4)
  -- bar 1
  tellRest half
  happy g4
  -- bar 2
  tellBar
  tellNotes quarter [a4, g4, c5]
  -- bar 3
  tellBar
  tellNote half b4
  happy g4
  -- bar 4
  tellBar
  tellNotes quarter [a4, g4, d5]
  -- bar 5
  tellBar
  tellNote half c5
  happy g4
  -- bar 6
  tellBar
  tellNotes quarter [g5, e5, c5]
  -- bar 7
  tellBar
  tellNotes quarter [b4, a4]
  happy f5
  -- bar 8
  tellBar
  tellNotes quarter [e5, c5, d5]
  -- bar 9
  tellBar
  tellNote (dotted half) c5
  where
    happy p = tellNote (dotted eighth) p >> tellNote sixteenth p

accompaniment :: (Score m) => m ()
accompaniment = do
  setKey cMajor
  setMeter (Meter 3 4)
  -- bar 1
  tellRest (dotted half)
  -- bar 2
  tellBar
  tellChord (dotted half) cChord
  -- bar 3
  tellBar
  tellChord (dotted half) g7Chord
  -- bar 4
  tellBar
  tellChord (dotted half) g7Chord
  -- bar 5
  tellBar
  tellChord (dotted half) cChord
  -- bar 6
  tellBar
  tellChord (dotted half) c7Chord
  -- bar 7
  tellBar
  tellChord (dotted half) fChord
  -- bar 8
  tellBar
  tellChord half cChord
  tellChord quarter g7Chord
  -- bar 9
  tellBar
  tellChord (dotted half) cChord
  where
    cChord = [c3, e3, g3]
    g7Chord = [b2, d3, f3, g3]
    c7Chord = [c3, e3, g3, bb3]
    fChord = [c3, f3, a3]
