module Demo.Music.HappyBirthday
  ( songHappyBirthday,
  )
where

import Herald.Instrument
import Herald.Music
import Herald.Music.Pitch
import Herald.Sound

songHappyBirthday :: Sound
songHappyBirthday = gain 0.5 $ playScore (ScoreEnv {bpm = 99, instrument = additive}) scoreHappyBirthday

scoreHappyBirthday :: (Score m) => m ()
scoreHappyBirthday = do
  tellRest 2
  tellNotes 0.5 [g4, g4]
  --
  tellNotes 1 [a4, g4, c5]
  --
  tellNote 2 b4
  tellNotes 0.5 [g4, g4]
  --
  tellNotes 1 [a4, g4, d5]
  --
  tellNote 2 c5
  tellNotes 0.5 [g4, g4]
  --
  tellNotes 1 [g5, e5, c5]
  --
  tellNotes 1 [b4, a4]
  tellNotes 0.5 [f5, f5]
  --
  tellNotes 1 [e5, c5, d5]
  --
  tellNote 3 c5
