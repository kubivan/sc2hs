#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 2 ]]; then
  echo "Usage: $0 <input_folder_with_SC2Map_files> <output_folder_for_txt_grids>" >&2
  exit 1
fi

INPUT_DIR="$1"
OUTPUT_DIR="$2"

if [[ ! -d "$INPUT_DIR" ]]; then
  echo "Input folder does not exist: $INPUT_DIR" >&2
  exit 1
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

mkdir -p "$OUTPUT_DIR"

map_count=0
while IFS= read -r -d '' map_file; do
  map_count=$((map_count + 1))
  map_name="$(basename "$map_file")"
  map_stem="${map_name%.*}"
  out_file="$OUTPUT_DIR/${map_stem}.txt"

  echo "[$map_count] Dumping $map_name -> $out_file"
  (
    cd "$REPO_ROOT"
    stack run sc2mapdump -- --map-path "$map_file" --output "$out_file"
  )
done < <(find "$INPUT_DIR" -maxdepth 1 -type f \( -name "*.SC2Map" -o -name "*.sc2map" \) -print0 | sort -z)

if [[ $map_count -eq 0 ]]; then
  echo "No .SC2Map files found in: $INPUT_DIR" >&2
  exit 2
fi

echo "Done. Generated $map_count grid file(s) in $OUTPUT_DIR"
