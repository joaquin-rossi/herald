-- | The language voices are written in. Interpreters are instances of
-- 'Score'; "Herald.Performance" renders to sound.
module Herald.Score
  ( Score (..),
    tellNotes,
  )
where

import Herald.Score.Duration
import Herald.Score.Dynamic
import Herald.Score.Pitch

-- | Pitches follow notation rules: an unwritten accidental comes from earlier
-- in the bar, else from the key (C major by default); a written one lasts
-- until the next 'tellBar'.
class (Monad m) => Score m where
  -- | After 'tellTie', continues the tied note instead: same letter and
  -- octave, keeping its accidental across bar lines.
  tellNote :: Beats -> Pitch -> m ()

  -- | Ties into the next note, which must have the same pitch.
  tellTie :: Beats -> Pitch -> m ()

  tellChord :: Beats -> [Pitch] -> m ()

  -- | @tellTrill b k p q@: @k@ notes alternating @p@ and @q@. The accidental
  -- of @q@ doesn't carry on through the bar.
  tellTrill :: Beats -> Int -> Pitch -> Pitch -> m ()

  tellRest :: Beats -> m ()

  -- | Ends the bar, which must add up to the meter, and clears accidentals.
  tellBar :: m ()

  setKey :: Key -> m ()

  -- | From then on, every bar must add up to it, including the last one.
  setMeter :: Meter -> m ()

  setDynamic :: Dynamic -> m ()

-- | Notes of the same length.
tellNotes :: (Score m) => Beats -> [Pitch] -> m ()
tellNotes b = mapM_ (tellNote b)
