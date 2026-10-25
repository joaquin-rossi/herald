# herald: sheet music in Haskell

[![CI](https://github.com/joaquin-rossi/herald/actions/workflows/ci.yml/badge.svg)](https://github.com/joaquin-rossi/herald/actions/workflows/ci.yml)

Haskell library for writing musical scores as code and synthesizing them into audio.

Original idea: https://www.youtube.com/watch?v=FYTZkE5BZ-0

## Features

- Named pitches, key signatures and accidentals that last until the bar line
- Note values, dots, ties, chords and trills
- Time signatures, with every bar checked
- Dynamics
- Equal temperament and historical tunings
- Several voices, each with its own instrument and stereo position
- Mono or stereo WAV output and playback

Instruments: `pureTone`, `additive`, `organ`, `harpsichord`.

## Example

```haskell
import Herald

melody :: Score m => m ()
melody = do
  setKey gMinor
  tellNotes quarter [g4, b4, a4, g4]
  tellBar
  tellNote whole g4

main :: IO ()
main =
  either (putStrLn . displayScoreError) (saveWav "melody.wav") $
    playScoreMono defaultConfig [(defaultVoice, melody)]
```

## Demo

```sh
cabal run herald-demo -- play little-fugue  # requires ffplay
```

Little Fugue in G minor ([source](https://www.mit.edu/~jcb/little-fugue-two-violins.abc)):

https://github.com/user-attachments/assets/ad373e14-ba8c-4924-a4e0-e839ccfaa71f

Happy Birthday:

https://github.com/user-attachments/assets/da93b721-d94f-4736-bea8-e08e566b3189
