-- | Write sheet music as Haskell code and synthesize it.
module Herald
  ( module Herald.Score,
    module Herald.Score.Pitch,
    module Herald.Score.Duration,
    module Herald.Score.Dynamic,
    module Herald.Performance,
    module Herald.Performance.Instrument,
    module Herald.Tuning,
    module Herald.Sound,
    module Herald.Sound.IO,
  )
where

import Herald.Performance
import Herald.Performance.Instrument
import Herald.Score
import Herald.Score.Duration
import Herald.Score.Dynamic
import Herald.Score.Pitch
import Herald.Sound
import Herald.Sound.IO
import Herald.Tuning
