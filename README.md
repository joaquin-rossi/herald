# herald: sheet music in Haskell

[![CI](https://github.com/joaquin-rossi/herald/actions/workflows/ci.yml/badge.svg)](https://github.com/joaquin-rossi/herald/actions/workflows/ci.yml)

Haskell library for writing musical scores as code and synthesizing them into audio.

Original idea: https://www.youtube.com/watch?v=FYTZkE5BZ-0

## Features

- **Scores as code**: each voice is a sequence of notes, rests and bar lines, read like a staff.
- **Notation rules**: named pitches (`c4`, `fs4`, `bb4`, …), key signatures, and accidentals.
- **Rhythm and tempo**: note values from whole to thirty-second notes, dotted notes, ties across bar lines, and tempo changes mid-score.
- **Checked bars**: with a time signature, every bar must add up to it; mistakes are reported with the voice and bar number.
- **Expression**: dynamics from pianissimo to fortissimo, chords and trills.
- **Tuning**: equal temperament at any reference pitch, or historical temperaments such as Werckmeister III and quarter-comma meantone.
- **Several voices**, mixed at balanced levels.
- **Instruments**: pure tones, additive synthesis, a pipe organ and a Karplus-Strong harpsichord, or your own as a function from frequency and duration to sound.
- **Output**: 16-bit 48 kHz WAV files, or direct playback.

## Requirements

- GHC 9.10, 9.12 or 9.14 and cabal 3.
- [FFmpeg](https://ffmpeg.org)'s `ffplay` on the `PATH`, only for `play`.

## Example

```haskell
import Herald

melody :: Score m => m ()
melody = do
  setKey gMinor
  setMeter (Meter 4 4)
  tellNotes quarter [g4, b4, a4, g4] -- b4 is B flat in G minor
  tellBar
  setDynamic forte
  tellNote half (sharp f4)
  tellNote half g4

bass :: Score m => m ()
bass = do
  setKey gMinor
  setMeter (Meter 4 4)
  tellTie half g3
  tellNote half g3
  tellBar
  tellNote half d3
  tellNote half g3

main :: IO ()
main = do
  let config = defaultConfig {tempo = 100, tuning = werckmeisterIII 415}
      song = playScore config [melody, withInstrument organ bass]
  saveWav "song.wav" song
  play song
```

## Demo

The [`demo`](https://github.com/joaquin-rossi/herald/tree/master/demo) package in the repository renders a couple of songs. Videos of them play on [GitHub](https://github.com/joaquin-rossi/herald).

```sh
cabal run herald-demo -- play little-fugue                   # requires ffplay
cabal run herald-demo -- wav little-fugue fugue.wav
cabal run herald-demo -- play little-fugue pure harpsichord  # one instrument per voice
```

| Song | Default instruments |
|---|---|
| `happy-birthday` | `additive` |
| `little-fugue` | `organ` `harpsichord` |

Instruments: `pure`, `additive`, `organ`, `harpsichord`.

### Little Fugue in G Minor

https://github.com/user-attachments/assets/1e45c481-e9d9-4878-850b-507c6e0b4602

Source: https://www.mit.edu/~jcb/little-fugue-two-violins.abc

### Happy Birthday

https://github.com/user-attachments/assets/ae7f5c15-e213-4c3a-88cf-05e7b19d2d02
