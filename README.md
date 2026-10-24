# herald: sheet music in haskell

[![CI](https://github.com/joaquin-rossi/herald/actions/workflows/ci.yml/badge.svg)](https://github.com/joaquin-rossi/herald/actions/workflows/ci.yml)

Haskell library for writing musical scores as code and synthesizing them into audio.

Original idea: https://www.youtube.com/watch?v=FYTZkE5BZ-0

## Features

- **Scores as code**: each voice is a sequence of notes, rests and bar lines, read like a staff.
- **Notation rules**: named pitches (`c4`, `fs4`, `bb4`, …), key signatures, and accidentals that last until the end of the bar.
- **Rhythm and tempo**: note values from whole to thirty-second notes, dotted notes, and tempo changes mid-score.
- **Several voices**, mixed at balanced levels, plus chords and trills.
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
  tellNotes quarter [g4, b4, a4, g4] -- b4 is B flat: the key is G minor
  tellBar
  tellNote half (sharp f4)
  tellNote half g4

bass :: Score m => m ()
bass = do
  tellNote whole g3
  tellBar
  tellNote half d3
  tellNote half g3

main :: IO ()
main = do
  let song = playScore defaultConfig {tempo = 100, key = gMinor} [melody, withInstrument organ bass]
  saveWav "song.wav" song
  play song
```

## Demo

The [`demo`](demo) package renders a couple of songs:

```sh
cabal run herald-demo -- play little-fugue                   # requires ffplay
cabal run herald-demo -- wav little-fugue fugue.wav
cabal run herald-demo -- play little-fugue pure harpsichord  # one instrument per voice
```

| Song | Default instruments |
|---|---|
| `happy-birthday` | `additive` |
| `little-fugue` | `organ` `harpsichord` |

Instruments: `pure`, `additive`, `organ`, `harpsichord`. A song takes either none (its defaults) or one per voice.

### Little Fugue in G Minor

https://github.com/user-attachments/assets/1e45c481-e9d9-4878-850b-507c6e0b4602

Source: https://www.mit.edu/~jcb/little-fugue-two-violins.abc

### Happy Birthday

https://github.com/user-attachments/assets/ae7f5c15-e213-4c3a-88cf-05e7b19d2d02

## Development

```sh
cabal build all
cabal test all
ormolu --mode inplace $(git ls-files '*.hs')
```

## License

AGPL-3.0-or-later. The Little Fugue transcription in the demo is © 2003 Jeff Bigler, used under the terms in its source file.
