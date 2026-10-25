module Main (main) where

import Data.ByteString.Lazy qualified as B
import Data.ByteString.Lazy.Char8 qualified as BC
import Data.Vector.Unboxed qualified as V
import Herald
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup
      "herald"
      [ pitchTests,
        tuningTests,
        soundTests,
        instrumentTests,
        scoreTests,
        tieTests,
        meterTests,
        stereoTests,
        wavTests
      ]

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
        map (keyAccidental (flats 2)) [B, E, A] @?= [Flat, Flat, Natural]
    ]

tuningTests :: TestTree
tuningTests =
  testGroup
    "Tuning"
    [ testCase "A4 is 440 Hz" $ tune a440 0 @?= 440,
      testCase "an octave doubles the frequency" $ tune a440 12 @?= 880,
      testCase "equal temperament takes a reference pitch" $ tune (equalTemperament 415) 0 @?= 415,
      testCase "temperaments are anchored at A4" $ do
        tune (werckmeisterIII 440) 0 @?= 440
        tune (quarterCommaMeantone 440) 0 @?= 440,
      testCase "Werckmeister III raises C4 by its offset from A" $
        assertApprox "cents" 11.73 (cents (tune (werckmeisterIII 440) (-9)) (tune a440 (-9))),
      testCase "quarter-comma meantone has a pure major third on C" $
        assertApprox "cents" (cents 5 4) (cents (tune meantone (-5)) (tune meantone (-9))),
      testCase "the score uses the configured tuning" $
        playScoreMono defaultConfig {tuning = equalTemperament 220} [(defaultVoice, tellNote quarter a5)]
          @?= Right (render (tellNote quarter a4))
    ]
  where
    meantone = quarterCommaMeantone 440
    cents x y = 1200 * logBase 2 (x / y)
    assertApprox msg expected actual =
      assertBool (msg ++ ": expected " ++ show expected ++ ", got " ++ show actual) (abs (expected - actual) < 0.1)

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
          V.length (renderNote i 440 0.3) @?= 14400
      | (name, i) <- instruments
      ],
      [ testCase ("notes peak at most 1: " ++ name ++ " at " ++ show f ++ " Hz") $
          assertBool "too loud" (peak (renderNote i f 0.5) <= 1)
      | (name, i) <- instruments,
        f <- [110, 440, 1760]
      ],
      [ testCase "a trill fills its duration" $
          V.length (trill additive 6 440 494 0.75) @?= 36000,
        testCase "a chord is as long as its notes" $
          V.length (chord organ [262, 330, 392] 0.5) @?= 24000
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
      testCase "the tempo comes from the configuration" $
        V.length <$> playScoreMono defaultConfig {tempo = 60} [(defaultVoice, tellNote quarter c4)] @?= Right 48000,
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
        playScoreMono defaultConfig [(defaultVoice, tellNote quarter (sharp f4)), (defaultVoice, tellNote quarter f4)]
          @?= playScoreMono defaultConfig [(defaultVoice, tellNote quarter fs4), (defaultVoice, tellNote quarter (natural f4))],
      testCase "each voice plays its own instrument" $
        playScoreMono defaultConfig [(defaultVoice {instrument = organ}, tellNote quarter c4), (defaultVoice, tellNote quarter c4)]
          @?= Right (balanceSound [renderNote organ (tune a440 (-9)) 0.5, renderNote additive (tune a440 (-9)) 0.5]),
      testCase "dynamics scale notes from then on" $
        render (setDynamic forte >> tellNote quarter c4)
          @?= gain (dynamicGain forte) (render (tellNote quarter c4)),
      testCase "dynamics go from soft to loud"
        $ assertBool "out of order"
        $ and (zipWith (<) dynamics (drop 1 dynamics))
    ]
  where
    dynamics = [pianissimo, piano, mezzoPiano, mezzoForte, forte, fortissimo]

tieTests :: TestTree
tieTests =
  testGroup
    "Tie"
    [ testCase "tied notes sound as one" $
        render (tellTie quarter c4 >> tellNote quarter c4) @?= render (tellNote half c4),
      testCase "ties can be chained" $
        render (tellTie eighth c4 >> tellTie eighth c4 >> tellNote quarter c4)
          @?= render (tellNote half c4),
      testCase "a tied note keeps its accidental across the bar line" $
        render (tellTie quarter (sharp f4) >> tellBar >> tellNote quarter f4)
          @?= render (tellNote half fs4),
      testCase "a tie into a different pitch is an error" $
        result [tellTie quarter c4 >> tellNote quarter d4]
          @?= Left (ScoreError 1 1 TieToDifferentPitch),
      testCase "a tie followed by a rest is an error" $
        result [tellTie quarter c4 >> tellBar >> tellRest quarter]
          @?= Left (ScoreError 1 2 UnfinishedTie),
      testCase "a voice can't end in a tie" $
        result [tellTie quarter c4] @?= Left (ScoreError 1 1 UnfinishedTie)
    ]

meterTests :: TestTree
meterTests =
  testGroup
    "Meter"
    [ testCase "a bar lasts its time signature in beats" $ do
        meterBeats (Meter 4 4) @?= 4
        meterBeats (Meter 6 8) @?= 3,
      testCase "bars that add up are accepted" $
        result [setMeter (Meter 3 4) >> tellNotes quarter [c4, d4, e4] >> tellBar >> tellNote (dotted half) c4]
          @?= Right (),
      testCase "a short bar is an error" $
        result [setMeter (Meter 4 4) >> tellNotes quarter [c4, d4, e4] >> tellBar]
          @?= Left (ScoreError 1 1 (WrongBarLength 3 4)),
      testCase "the last bar is checked too" $
        result [setMeter (Meter 2 4) >> tellNotes quarter [c4, d4] >> tellBar >> tellNote quarter c4]
          @?= Left (ScoreError 1 2 (WrongBarLength 1 2)),
      testCase "a tie counts in each bar it crosses" $
        result [setMeter (Meter 2 4) >> tellNote quarter c4 >> tellTie quarter d4 >> tellBar >> tellNotes quarter [d4, e4]]
          @?= Right (),
      testCase "errors name the voice" $
        result [tellNote quarter c4, setMeter (Meter 2 4) >> tellNote quarter c4]
          @?= Left (ScoreError 2 1 (WrongBarLength 1 2))
    ]

stereoTests :: TestTree
stereoTests =
  testGroup
    "Stereo"
    [ testCase "the centre splits a sound at equal power" $ do
        let Stereo l r = panSound 0 (V.fromList [1])
        assertClose l (V.fromList [sqrt 0.5])
        assertClose r (V.fromList [sqrt 0.5]),
      testCase "hard left and right use a single channel" $ do
        assertClose (rightChannel (panSound (-1) (V.fromList [1]))) (V.fromList [0])
        assertClose (leftChannel (panSound 1 (V.fromList [1]))) (V.fromList [0]),
      testCase "voices go to their side" $ do
        let left = tellNote quarter c4
            right = tellNote quarter e4
        case playScoreStereo defaultConfig [(defaultVoice {pan = -1}, left), (defaultVoice {pan = 1}, right)] of
          Left err -> assertFailure (displayScoreError err)
          Right (Stereo l r) -> do
            assertClose l (gain (1 / sqrt 2) (render left))
            assertClose r (gain (1 / sqrt 2) (render right))
    ]
  where
    assertClose x y = do
      V.length x @?= V.length y
      assertBool "channels differ" (V.and (V.zipWith (\a b -> abs (a - b) < 1e-12) x y))

wavTests :: TestTree
wavTests =
  testGroup
    "WAV"
    [ testCase "header and 16-bit samples" $ do
        let wav = encodeWav (mono [0, 1, -1])
        B.length wav @?= 44 + 6
        BC.take 4 wav @?= BC.pack "RIFF"
        BC.take 4 (B.drop 8 wav) @?= BC.pack "WAVE"
        B.unpack (B.drop 44 wav) @?= [0, 0, 0xff, 0x7f, 0x01, 0x80],
      testCase "out-of-range samples are clipped" $
        B.drop 44 (encodeWav (mono [2, -2])) @?= B.drop 44 (encodeWav (mono [1, -1])),
      testCase "stereo interleaves left and right" $ do
        let wav = encodeWav (Stereo (V.fromList [1, 0]) (V.fromList [-1]))
        B.length wav @?= 44 + 8
        B.unpack (B.take 2 (B.drop 22 wav)) @?= [2, 0]
        B.unpack (B.drop 44 wav) @?= [0xff, 0x7f, 0x01, 0x80, 0, 0, 0, 0]
    ]

-- | Render a single voice with the default configuration, which the tests
-- expect to succeed.
render :: ScoreT () -> Sound
render v = either (error . displayScoreError) id (playScoreMono defaultConfig [(defaultVoice, v)])

-- | Whether voices with the default configuration render.
result :: [ScoreT ()] -> Either ScoreError ()
result vs = () <$ playScoreMono defaultConfig [(defaultVoice, v) | v <- vs]

peak :: Sound -> Double
peak = V.maximum . V.map abs

-- | A mono sound from samples.
mono :: [Double] -> Sound
mono = V.fromList
