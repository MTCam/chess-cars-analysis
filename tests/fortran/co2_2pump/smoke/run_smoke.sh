#!/usr/bin/env bash
set -euo pipefail

# Resolve paths
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../../.." && pwd)"
PKG_DIR="$ROOT/src/fortran/co2_2pump"
BIN_DIR="$PKG_DIR/bin"
DATA_DIR="$PKG_DIR/data"

echo "[co2-smoke] Building co2_2pump..."
make -C "$PKG_DIR" -j

# Find the carsfit executable
EXE_PATH="$(ls -1 "$BIN_DIR"/carsfit_* 2>/dev/null | head -n1 || true)"
if [[ -z "$EXE_PATH" ]]; then
  echo "[co2-smoke] FAIL: no carsfit_* executable in $BIN_DIR"
  exit 1
fi
EXE_NAME="$(basename "$EXE_PATH")"

# Stage clean run dir with data
RUN_DIR="$BIN_DIR/_smoke_run"
rm -rf "$RUN_DIR"; mkdir -p "$RUN_DIR"
cp -a "$EXE_PATH" "$RUN_DIR/$EXE_NAME"
cp -a "$DATA_DIR"/* "$RUN_DIR"/

# Pick a timeout command if available (optional)
TIMEOUT_CMD=""
if command -v timeout >/dev/null 2>&1; then
  TIMEOUT_CMD="timeout"
elif command -v gtimeout >/dev/null 2>&1; then
  TIMEOUT_CMD="gtimeout"
fi

echo "[co2-smoke] Running in $RUN_DIR ..."
set +e
if [[ -n "$TIMEOUT_CMD" ]]; then
  ( cd "$RUN_DIR" && "$TIMEOUT_CMD" 60s "./$EXE_NAME" > stdout.log 2> stderr.log <<'EOF'








N
N
EOF
  )
else
  ( cd "$RUN_DIR" && "./$EXE_NAME" > stdout.log 2> stderr.log <<'EOF'








N
N
EOF
  )
fi
rc=$?
set -e

if [[ $rc -ne 0 ]]; then
  echo "[co2-smoke] FAIL: exit code $rc"
  if [[ -f "$RUN_DIR/stderr.log" ]]; then
    echo "---- stderr ----"
    sed -n '1,200p' "$RUN_DIR/stderr.log" || true
  else
    echo "(no stderr.log produced)"
  fi
  exit 1
else
    echo "[co2-smoke] Run finished."
fi

# Basic sanity
if ! [[ -s "$RUN_DIR/stdout.log" ]]; then
  echo "[co2-smoke] FAIL: Didn't make any output (i.e. empty stdout)."
  exit 1
else
    echo "[co2-smoke] STDOUT: "
    cat ${RUN_DIR}/stdout.log
fi
if [[ -f "$RUN_DIR/stderr.log" ]] && grep -Eiq 'segmentation|floating|stack|abort|error' "$RUN_DIR/stderr.log"; then
  echo "[co2-smoke] FAIL: runtime errors. Error output from stderr:"
  sed -n '1,200p' "$RUN_DIR/stderr.log"
  exit 1
fi
if [[ -f "$RUN_DIR/pltchi_0001.csv" ]] && [[ -f "$RUN_DIR/pltchi_0001.meta" ]]; then
    echo "[co2-smoke] Found expected outputs in pltchi_0001.{csv,meta}."
    echo "[co2-smoke] OK"
    exit 0
else
    echo "[co2-smoke] FAIL: did not find expected output."
    exit 1
fi

