-- |
-- Module      : Herald.Pitch
-- Description : Written pitches and key signatures.
--
-- Pitches are written as in sheet music: a letter, an octave in scientific
-- pitch notation (middle C is 'c4', concert A is 'a4') and an optional
-- accidental. A pitch without a written accidental takes the one from the
-- key signature, and from earlier in the bar when inside a score (see
-- "Herald.Score").
--
-- Every pitch from octave 0 to 8 has a name: @c4@ is C, @cs4@ is C sharp and
-- @cb4@ is C flat. Accidentals can also be applied with 'sharp', 'flat' and
-- 'natural':
--
-- >>> pitchSemitones cMajor (sharp f4) == pitchSemitones cMajor fs4
-- True
module Herald.Pitch
  ( -- * Pitches
    Semitones,
    Octave,
    Letter (..),
    Accidental (..),
    Pitch (..),
    flat,
    sharp,
    natural,

    -- * Key signatures
    Key (..),
    keyAccidental,
    sharps,
    flats,
    cMajor,
    gMinor,

    -- * Semitones
    pitchSemitones,

    -- * Pitch names
    -- $names
    cb0,
    c0,
    cs0,
    db0,
    d0,
    ds0,
    eb0,
    e0,
    es0,
    fb0,
    f0,
    fs0,
    gb0,
    g0,
    gs0,
    ab0,
    a0,
    as0,
    bb0,
    b0,
    bs0,
    cb1,
    c1,
    cs1,
    db1,
    d1,
    ds1,
    eb1,
    e1,
    es1,
    fb1,
    f1,
    fs1,
    gb1,
    g1,
    gs1,
    ab1,
    a1,
    as1,
    bb1,
    b1,
    bs1,
    cb2,
    c2,
    cs2,
    db2,
    d2,
    ds2,
    eb2,
    e2,
    es2,
    fb2,
    f2,
    fs2,
    gb2,
    g2,
    gs2,
    ab2,
    a2,
    as2,
    bb2,
    b2,
    bs2,
    cb3,
    c3,
    cs3,
    db3,
    d3,
    ds3,
    eb3,
    e3,
    es3,
    fb3,
    f3,
    fs3,
    gb3,
    g3,
    gs3,
    ab3,
    a3,
    as3,
    bb3,
    b3,
    bs3,
    cb4,
    c4,
    cs4,
    db4,
    d4,
    ds4,
    eb4,
    e4,
    es4,
    fb4,
    f4,
    fs4,
    gb4,
    g4,
    gs4,
    ab4,
    a4,
    as4,
    bb4,
    b4,
    bs4,
    cb5,
    c5,
    cs5,
    db5,
    d5,
    ds5,
    eb5,
    e5,
    es5,
    fb5,
    f5,
    fs5,
    gb5,
    g5,
    gs5,
    ab5,
    a5,
    as5,
    bb5,
    b5,
    bs5,
    cb6,
    c6,
    cs6,
    db6,
    d6,
    ds6,
    eb6,
    e6,
    es6,
    fb6,
    f6,
    fs6,
    gb6,
    g6,
    gs6,
    ab6,
    a6,
    as6,
    bb6,
    b6,
    bs6,
    cb7,
    c7,
    cs7,
    db7,
    d7,
    ds7,
    eb7,
    e7,
    es7,
    fb7,
    f7,
    fs7,
    gb7,
    g7,
    gs7,
    ab7,
    a7,
    as7,
    bb7,
    b7,
    bs7,
    cb8,
    c8,
    cs8,
    db8,
    d8,
    ds8,
    eb8,
    e8,
    es8,
    fb8,
    f8,
    fs8,
    gb8,
    g8,
    gs8,
    ab8,
    a8,
    as8,
    bb8,
    b8,
    bs8,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe

-- | A distance in semitones, relative to A4 when used as an absolute pitch.
type Semitones = Int

-- | An octave in scientific pitch notation; octave 4 starts at middle C.
type Octave = Int

-- | The letter name of a pitch.
data Letter = C | D | E | F | G | A | B
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | An accidental, written or implied by a key signature.
data Accidental = Flat | Natural | Sharp
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | A written pitch in scientific notation (A4 = 440 Hz).
data Pitch = Pitch
  { letter :: Letter,
    octave :: Octave,
    -- | 'Nothing' when no accidental is written, so the key decides.
    accidental :: Maybe Accidental
  }
  deriving (Show, Eq)

-- | Write a flat on a pitch.
flat :: Pitch -> Pitch
flat p = p {accidental = Just Flat}

-- | Write a sharp on a pitch.
sharp :: Pitch -> Pitch
sharp p = p {accidental = Just Sharp}

-- | Write a natural sign on a pitch, cancelling the key signature.
natural :: Pitch -> Pitch
natural p = p {accidental = Just Natural}

-- | A key signature: the accidental each letter takes when none is written.
newtype Key = Key (Map Letter Accidental)
  deriving (Show, Eq)

-- | The accidental a key signature gives to a letter.
keyAccidental :: Key -> Letter -> Accidental
keyAccidental (Key as) l = Map.findWithDefault Natural l as

-- | Key signature with @n@ sharps, added in the order F C G D A E B.
sharps :: Int -> Key
sharps n = Key $ Map.fromList [(l, Sharp) | l <- take n [F, C, G, D, A, E, B]]

-- | Key signature with @n@ flats, added in the order B E A D G C F.
flats :: Int -> Key
flats n = Key $ Map.fromList [(l, Flat) | l <- take n [B, E, A, D, G, C, F]]

-- | C major (or A minor): no sharps or flats.
cMajor :: Key
cMajor = Key Map.empty

-- | G minor (or B flat major): two flats.
gMinor :: Key
gMinor = flats 2

-- | Semitones relative to A4, taking unwritten accidentals from the key.
--
-- >>> pitchSemitones cMajor c4
-- -9
-- >>> pitchSemitones gMinor b4
-- 1
pitchSemitones :: Key -> Pitch -> Semitones
pitchSemitones key (Pitch l o acc) = base l + offset (fromMaybe (keyAccidental key l) acc) + 12 * (o - 4) - 9
  where
    base C = 0
    base D = 2
    base E = 4
    base F = 5
    base G = 7
    base A = 9
    base B = 11
    offset Flat = -1
    offset Natural = 0
    offset Sharp = 1

-- $names
--
-- Names are a letter, then @b@ for flat or @s@ for sharp, then the octave:
-- @bb4@ is B flat 4, @fs5@ is F sharp 5 and @e4@ is E 4 with no written
-- accidental. Natural names leave the accidental unwritten, so they follow
-- the key signature.

-- | Octave 0.
cb0, c0, cs0, db0, d0, ds0, eb0, e0, es0, fb0, f0, fs0, gb0, g0, gs0, ab0, a0, as0, bb0, b0, bs0 :: Pitch
c0 = Pitch C 0 Nothing
d0 = Pitch D 0 Nothing
e0 = Pitch E 0 Nothing
f0 = Pitch F 0 Nothing
g0 = Pitch G 0 Nothing
a0 = Pitch A 0 Nothing
b0 = Pitch B 0 Nothing
cb0 = flat c0
db0 = flat d0
eb0 = flat e0
fb0 = flat f0
gb0 = flat g0
ab0 = flat a0
bb0 = flat b0
cs0 = sharp c0
ds0 = sharp d0
es0 = sharp e0
fs0 = sharp f0
gs0 = sharp g0
as0 = sharp a0
bs0 = sharp b0

-- | Octave 1.
cb1, c1, cs1, db1, d1, ds1, eb1, e1, es1, fb1, f1, fs1, gb1, g1, gs1, ab1, a1, as1, bb1, b1, bs1 :: Pitch
c1 = Pitch C 1 Nothing
d1 = Pitch D 1 Nothing
e1 = Pitch E 1 Nothing
f1 = Pitch F 1 Nothing
g1 = Pitch G 1 Nothing
a1 = Pitch A 1 Nothing
b1 = Pitch B 1 Nothing
cb1 = flat c1
db1 = flat d1
eb1 = flat e1
fb1 = flat f1
gb1 = flat g1
ab1 = flat a1
bb1 = flat b1
cs1 = sharp c1
ds1 = sharp d1
es1 = sharp e1
fs1 = sharp f1
gs1 = sharp g1
as1 = sharp a1
bs1 = sharp b1

-- | Octave 2.
cb2, c2, cs2, db2, d2, ds2, eb2, e2, es2, fb2, f2, fs2, gb2, g2, gs2, ab2, a2, as2, bb2, b2, bs2 :: Pitch
c2 = Pitch C 2 Nothing
d2 = Pitch D 2 Nothing
e2 = Pitch E 2 Nothing
f2 = Pitch F 2 Nothing
g2 = Pitch G 2 Nothing
a2 = Pitch A 2 Nothing
b2 = Pitch B 2 Nothing
cb2 = flat c2
db2 = flat d2
eb2 = flat e2
fb2 = flat f2
gb2 = flat g2
ab2 = flat a2
bb2 = flat b2
cs2 = sharp c2
ds2 = sharp d2
es2 = sharp e2
fs2 = sharp f2
gs2 = sharp g2
as2 = sharp a2
bs2 = sharp b2

-- | Octave 3.
cb3, c3, cs3, db3, d3, ds3, eb3, e3, es3, fb3, f3, fs3, gb3, g3, gs3, ab3, a3, as3, bb3, b3, bs3 :: Pitch
c3 = Pitch C 3 Nothing
d3 = Pitch D 3 Nothing
e3 = Pitch E 3 Nothing
f3 = Pitch F 3 Nothing
g3 = Pitch G 3 Nothing
a3 = Pitch A 3 Nothing
b3 = Pitch B 3 Nothing
cb3 = flat c3
db3 = flat d3
eb3 = flat e3
fb3 = flat f3
gb3 = flat g3
ab3 = flat a3
bb3 = flat b3
cs3 = sharp c3
ds3 = sharp d3
es3 = sharp e3
fs3 = sharp f3
gs3 = sharp g3
as3 = sharp a3
bs3 = sharp b3

-- | Octave 4 (middle C is 'c4', concert A is 'a4').
cb4, c4, cs4, db4, d4, ds4, eb4, e4, es4, fb4, f4, fs4, gb4, g4, gs4, ab4, a4, as4, bb4, b4, bs4 :: Pitch
c4 = Pitch C 4 Nothing
d4 = Pitch D 4 Nothing
e4 = Pitch E 4 Nothing
f4 = Pitch F 4 Nothing
g4 = Pitch G 4 Nothing
a4 = Pitch A 4 Nothing
b4 = Pitch B 4 Nothing
cb4 = flat c4
db4 = flat d4
eb4 = flat e4
fb4 = flat f4
gb4 = flat g4
ab4 = flat a4
bb4 = flat b4
cs4 = sharp c4
ds4 = sharp d4
es4 = sharp e4
fs4 = sharp f4
gs4 = sharp g4
as4 = sharp a4
bs4 = sharp b4

-- | Octave 5.
cb5, c5, cs5, db5, d5, ds5, eb5, e5, es5, fb5, f5, fs5, gb5, g5, gs5, ab5, a5, as5, bb5, b5, bs5 :: Pitch
c5 = Pitch C 5 Nothing
d5 = Pitch D 5 Nothing
e5 = Pitch E 5 Nothing
f5 = Pitch F 5 Nothing
g5 = Pitch G 5 Nothing
a5 = Pitch A 5 Nothing
b5 = Pitch B 5 Nothing
cb5 = flat c5
db5 = flat d5
eb5 = flat e5
fb5 = flat f5
gb5 = flat g5
ab5 = flat a5
bb5 = flat b5
cs5 = sharp c5
ds5 = sharp d5
es5 = sharp e5
fs5 = sharp f5
gs5 = sharp g5
as5 = sharp a5
bs5 = sharp b5

-- | Octave 6.
cb6, c6, cs6, db6, d6, ds6, eb6, e6, es6, fb6, f6, fs6, gb6, g6, gs6, ab6, a6, as6, bb6, b6, bs6 :: Pitch
c6 = Pitch C 6 Nothing
d6 = Pitch D 6 Nothing
e6 = Pitch E 6 Nothing
f6 = Pitch F 6 Nothing
g6 = Pitch G 6 Nothing
a6 = Pitch A 6 Nothing
b6 = Pitch B 6 Nothing
cb6 = flat c6
db6 = flat d6
eb6 = flat e6
fb6 = flat f6
gb6 = flat g6
ab6 = flat a6
bb6 = flat b6
cs6 = sharp c6
ds6 = sharp d6
es6 = sharp e6
fs6 = sharp f6
gs6 = sharp g6
as6 = sharp a6
bs6 = sharp b6

-- | Octave 7.
cb7, c7, cs7, db7, d7, ds7, eb7, e7, es7, fb7, f7, fs7, gb7, g7, gs7, ab7, a7, as7, bb7, b7, bs7 :: Pitch
c7 = Pitch C 7 Nothing
d7 = Pitch D 7 Nothing
e7 = Pitch E 7 Nothing
f7 = Pitch F 7 Nothing
g7 = Pitch G 7 Nothing
a7 = Pitch A 7 Nothing
b7 = Pitch B 7 Nothing
cb7 = flat c7
db7 = flat d7
eb7 = flat e7
fb7 = flat f7
gb7 = flat g7
ab7 = flat a7
bb7 = flat b7
cs7 = sharp c7
ds7 = sharp d7
es7 = sharp e7
fs7 = sharp f7
gs7 = sharp g7
as7 = sharp a7
bs7 = sharp b7

-- | Octave 8.
cb8, c8, cs8, db8, d8, ds8, eb8, e8, es8, fb8, f8, fs8, gb8, g8, gs8, ab8, a8, as8, bb8, b8, bs8 :: Pitch
c8 = Pitch C 8 Nothing
d8 = Pitch D 8 Nothing
e8 = Pitch E 8 Nothing
f8 = Pitch F 8 Nothing
g8 = Pitch G 8 Nothing
a8 = Pitch A 8 Nothing
b8 = Pitch B 8 Nothing
cb8 = flat c8
db8 = flat d8
eb8 = flat e8
fb8 = flat f8
gb8 = flat g8
ab8 = flat a8
bb8 = flat b8
cs8 = sharp c8
ds8 = sharp d8
es8 = sharp e8
fs8 = sharp f8
gs8 = sharp g8
as8 = sharp a8
bs8 = sharp b8
