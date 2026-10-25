module Demo.Music.HappyBirthday
  ( songHappyBirthday,
  )
where

import Herald

songHappyBirthday :: Instrument -> Sound
songHappyBirthday i = gain 0.5 $ playScore defaultConfig {tempo = 99, instrument = i} [scoreHappyBirthday]

scoreHappyBirthday :: (Score m) => m ()
scoreHappyBirthday = do
  setMeter (Meter 3 4)
  tellRest 2
  tellNotes 0.5 [g4, g4]
  tellBar
  tellNotes 1 [a4, g4, c5]
  tellBar
  tellNote 2 b4
  tellNotes 0.5 [g4, g4]
  tellBar
  tellNotes 1 [a4, g4, d5]
  tellBar
  tellNote 2 c5
  tellNotes 0.5 [g4, g4]
  tellBar
  tellNotes 1 [g5, e5, c5]
  tellBar
  tellNotes 1 [b4, a4]
  tellNotes 0.5 [f5, f5]
  tellBar
  tellNotes 1 [e5, c5, d5]
  tellBar
  tellNote 3 c5
