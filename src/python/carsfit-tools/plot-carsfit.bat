@echo off

REM Directory containing this script
set HERE=%~dp0

where python >nul 2>&1
if errorlevel 1 (
  echo plot-carsfit: python not found on PATH 1>&2
  exit /b 1
)

python - <<EOF >nul 2>&1
import matplotlib
EOF
if errorlevel 1 (
  echo plot-carsfit: matplotlib not available 1>&2
  exit /b 1
)

REM Prefer Python launcher if available
where py >nul 2>&1
if %errorlevel%==0 (
  py -3 "%HERE%plot_carsfit_csv.py" %*
) else (
  python "%HERE%plot_carsfit_csv.py" %*
)
