module Main (main) where

import Data.ByteString.Lazy qualified as B
import Data.ByteString.Lazy.Char8 qualified as BC
import Data.Vector.Unboxed qualified as V
import Herald
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "herald" [pitchTests, soundTests, instrumentTests, scoreTests, wavTests]

pitchTests :: TestTree
pitchTests =
  testGroup
    "Pitch"
    [ testCase "A4 is the reference" $ pitchSemitones cMajor a4 @?= 0,
      testCase "middle C is 9 semitones below A4" $ pitchSemitones cMajor c4 @?= -9,
      testCase "octaves are 12 semitones apart" $ pitchSemitones cMajor a5 @?= 12,
      testCase "names carry accidentals" $ do
        pitchSemitones cMajor fs4 @?= pitchSemitones cMajor (sharp f4)
        pitchSemitones cMajor bb4 @?= pitchSemitones cMajor a4 + 1,
      testCase "unwritten accidentals come from the key" $
        pitchSemitones gMinor b4 @?= pitchSemitones cMajor bb4,
      testCase "a natural sign cancels the key" $
        pitchSemitones gMinor (natural b4) @?= pitchSemitones cMajor b4,
      testCase "key signatures add accidentals in order" $ do
        map (keyAccidental (sharps 2)) [F, C, G] @?= [Sharp, Sharp, Natural]
        map (keyAccidental (flats 2)) [B, E, A] @?= [Flat, Flat, Natural],
      testCase "A4 is 440 Hz" $ noteFreq 0 @?= 440,
      testCase "an octave doubles the frequency" $ noteFreq 12 @?= 880
    ]

soundTests :: TestTree
soundTests =
  testGroup
    "Sound"
    [ testCase "durations round to whole samples" $ do
        V.length (silence 1) @?= 48000
        V.length (sine 440 0.5) @?= 24000,
      testCase "mixing pads shorter sounds with silence" $
        mixSound [V.fromList [1, 2, 3], V.fromList [10]] @?= V.fromList [11, 2, 3],
      testCase "balanced mixing divides by the square root of the count" $
        balanceSound [V.fromList [1], V.fromList [1], V.fromList [1], V.fromList [1]] @?= V.fromList [2],
      testCase "a pluck starts at a peak of 1 and decays" $ do
        let s = pluck 0.996 220 1
        V.length s @?= 48000
        peak s @?= 1
        assertBool "doesn't decay" (peak (V.drop 40000 s) < 0.5)
    ]

instrumentTests :: TestTree
instrumentTests =
  testGroup "Instrument" . concat $
    [ [ testCase ("notes last exactly their duration: " ++ name) $
          V.length (note i 0 0.3) @?= 14400
      | (name, i) <- instruments
      ],
      [ testCase ("notes peak at most 1: " ++ name ++ " at " ++ show n) $
          assertBool "too loud" (peak (note i n 0.5) <= 1)
      | (name, i) <- instruments,
        n <- [-24, 0, 24]
      ],
      [ testCase "a trill fills its duration" $
          V.length (trill additive 6 0 2 0.75) @?= 36000,
        testCase "a chord is as long as its notes" $
          V.length (chord organ [0, 4, 7] 0.5) @?= 24000
      ]
    ]
  where
    instruments = [("pure tone", pureTone), ("additive", additive), ("organ", organ), ("harpsichord", harpsichord)]

scoreTests :: TestTree
scoreTests =
  testGroup
    "Score"
    [ testCase "a quarter note at 120 bpm lasts half a second" $
        V.length (render (tellNotes quarter [c4, d4])) @?= 48000,
      testCase "rests and notes share the timeline" $
        V.length (render (tellRest half >> tellNote eighth c4)) @?= 60000,
      testCase "tempo changes apply from then on" $
        V.length (render (tellNote quarter c4 >> setTempo 60 >> tellNote quarter c4)) @?= 72000,
      testCase "voices start in C major and setKey applies from then on" $
        render (tellNote quarter b4 >> setKey gMinor >> tellNote quarter b4)
          @?= render (tellNotes quarter [natural b4, bb4]),
      testCase "an accidental lasts until the end of the bar" $
        render (tellNote quarter (sharp f4) >> tellNote quarter f4)
          @?= render (tellNotes quarter [fs4, fs4]),
      testCase "a bar line cancels earlier accidentals" $
        render (tellNote quarter (sharp f4) >> tellBar >> tellNote quarter f4)
          @?= render (tellNotes quarter [fs4, natural f4]),
      testCase "a trill's auxiliary accidental doesn't carry on" $
        render (tellTrill quarter 4 e4 (sharp f4) >> tellNote quarter f4)
          @?= render (tellTrill quarter 4 e4 fs4 >> tellNote quarter (natural f4)),
      testCase "voices are rendered independently" $
        playScore defaultConfig [tellNote quarter (sharp f4), tellNote quarter f4]
          @?= playScore defaultConfig [tellNote quarter fs4, tellNote quarter (natural f4)],
      testCase "withInstrument applies only to its block" $
        render (withInstrument organ (tellNote quarter c4) >> tellNote quarter c4)
          @?= V.concat [note organ (-9) 0.5, note additive (-9) 0.5]
    ]
  where
    render v = playScore defaultConfig [v]

wavTests :: TestTree
wavTests =
  testGroup
    "WAV"
    [ testCase "header and 16-bit samples" $ do
        let wav = encodeWav (V.fromList [0, 1, -1])
        B.length wav @?= 44 + 6
        BC.take 4 wav @?= BC.pack "RIFF"
        BC.take 4 (B.drop 8 wav) @?= BC.pack "WAVE"
        B.unpack (B.drop 44 wav) @?= [0, 0, 0xff, 0x7f, 0x01, 0x80],
      testCase "out-of-range samples are clipped" $
        B.drop 44 (encodeWav (V.fromList [2, -2])) @?= B.drop 44 (encodeWav (V.fromList [1, -1]))
    ]

peak :: Sound -> Double
peak = V.maximum . V.map abs
