#!/usr/bin/env bash
# Renders the demo songs as MP4 videos under GitHub's 10 MB attachment limit,
# to upload through the README editor.
# Usage: scripts/render-videos.sh [output-dir]. Set CABAL to override cabal.

set -euo pipefail

out=${1:-${TMPDIR:-/tmp}/herald-videos}
cabal=${CABAL:-cabal}
max_bytes=$((9000 * 1024)) # x264 overshoots the target bitrate a little
audio_kbps=128

songs=(
  "happy-birthday|Happy Birthday"
  "little-fugue|Little Fugue in G Minor — J. S. Bach"
)

cd "$(dirname "$0")/.."
mkdir -p "$out"
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

$cabal build -v0 herald-demo
demo=$($cabal list-bin -v0 herald-demo)
font=$(fc-match -f '%{file}' sans)

for entry in "${songs[@]}"; do
  song=${entry%%|*}
  title=${entry#*|}
  wav="$tmp/$song.wav"
  mp4="$out/$song.mp4"

  "$demo" wav "$song" "$wav"
  seconds=$(ffprobe -v error -show_entries format=duration -of csv=p=0 "$wav")

  video_kbps=$(awk -v s="$seconds" -v max="$max_bytes" -v a="$audio_kbps" \
    'BEGIN { k = int(max * 8 / 1000 / s - a); print (k > 2000 ? 2000 : k) }')

  filter="[0:a]showspectrum=s=1280x400:slide=scroll:mode=combined:scale=log:fscale=log:color=intensity:legend=0,"
  filter+="drawtext=fontfile=$font:text='$title':x=24:y=20:fontsize=36:fontcolor=white:box=1:boxcolor=black@0.5:boxborderw=10[spec];"
  filter+="[0:a]showwaves=s=1280x160:mode=cline:colors=0x9ecbff:scale=sqrt[wave];"
  filter+="[spec][wave]vstack,format=yuv420p[v]"

  common=(-hide_banner -loglevel error -y -i "$wav" -filter_complex "$filter" -map "[v]"
    -c:v libx264 -preset slow -b:v "${video_kbps}k" -r 30 -passlogfile "$tmp/x264")
  ffmpeg "${common[@]}" -pass 1 -an -f mp4 /dev/null
  ffmpeg "${common[@]}" -pass 2 -map 0:a -c:a aac -b:a "${audio_kbps}k" -movflags +faststart -shortest "$mp4"

  echo "$mp4: $(du -h "$mp4" | cut -f1), ${seconds%.*} s"
done
