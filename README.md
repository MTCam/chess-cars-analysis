# CARS Analysis Tools

[![MATLAB CI](https://github.com/chess-uiuc/cars-analysis/actions/workflows/matlab-ci.yml/badge.svg)](https://github.com/chess-uiuc/cars-analysis/actions/workflows/matlab-ci.yml)
[![CO2 Dual Pump CI](https://github.com/chess-uiuc/cars-analysis/actions/workflows/co2-ci.yml/badge.svg)](https://github.com/chess-uiuc/cars-analysis/actions/workflows/co2-ci.yml)

MATLAB & Fortran analysis tooling for cars data.

## Layout

### Main tools
- `src/matlab/carsft`  – Matlab source for CARSFT functions
- `src/matlab/+cfx` - Matlab interface for Fortran carsfit
- `src/fortran/co2_2pump` - Fortran-based carsfit (CO2/2pump)
- `src/fortran/co2_2pump/data` - Data files for the fortran-based exe
- `src/python/carsfit-tools` - Python utilities for carsfit (e.g. plotting)

### Tests
- `tests/matlab/carsft` – Tests of CARSFT Matlab codes
- `tests/matlab/cfx` - Tests of the interface to Fortran carsfit (CO2/2pump)
- `tests/fortran/co2_2pump` - Tests for the CO2/dual pump code

------------------

## Get the code:

Obtain the suite of CARS analysis tools with the following command
from your terminal:

```bash
mkdir ~/CHESS-CARS-ANALYSIS
cd ~/CHESS-CARS-ANALYSIS
git clone https://github.com/chess-uiuc/cars-analysis
cd cars-analysis
```

-----------------------

## Set up the MATLAB Utilities (`cfx` toolkit)
----------------------------------------------

This repository includes a small set of MATLAB utilities under `src/matlab/+cfx/` to make it easy to run `carsfit`, ingest its outputs, and plot results in MATLAB.  To use the `cfx` toolkit, it needs to be added to the MATLAB path. If you want to use the `cfx` toolkit, then after getting the code as above, you can set up the `cfx` toolkit by doing the following inside MATLAB:

```matlab
cd ~/CHESS-CARS-ANALYSIS/cars-analysis/src/matlab
addpath(pwd)
cd ../../

% --- make sure everything works by running the test suite ---
% Test matlab carsft functions
runtests('tests/matlab/carsft')
% Run the CFX tests
runtests('tests/matlab/cfx')
```

You can inspect, and change the `carsft` tests by looking in:
`tests/matlab/carsft/*.m`

You can inspect, and change `carsft` code by looking in:
`src/matlab/carsft/src/*.m`

--------------------

### Running `carsfit` from within MATLAB
----------------------------------------

#### Option 1 - Directly with your own executable

If you have your own executable and inputs, then you can run it directly in MATLAB. If your executable is named `carsfit-3`, for example, and you're in the directory where it is located along with all the required inputs, then you can run it like this:

```matlab
system("./carsfit-3")
```

This should run `carsfit` interactively with the usual menu interface. Depending on what version of the code runs, it will produce outputs similar to `spec.out` and possibly some plotting data in `pltchi_000X.csv`.  Information on how to use these results using the `cfx` toolkit can be found below.

--------------

#### Option 2 - In an interactive loop with your own executable

The `cfx` toolkit includes an interactive loop runner which will run your `carsfit` executable and plot the resulting spectra in a loop.  If you want to run in this loop mode, then run it like this:

```matlab
cfx.run_carsfit_exe("./carsfit-3")
```

This mode will be interactive and prompt the user for inputs, filenames, and options to keep going or exit. Plots and outputs will optionally be generated interactively.  Outputs can be further processed inside MATLAB with options explained below.

----------------

#### Option 3 - Using the built-in MATLAB interface to FORTRAN `carsfit_co2`

To use this, you must first build
or obtain the binary `carsfit_co2` executable for your particular platform. If you have a
FORTRAN compiler, the instructions above can help you build it.  Otherwise, you must obtain
an executable from someone that already has one.

The following steps should get you up and running with the Matlab interface to the FORTRAN
code.

1) Build or obtain the binary (See HOWTO build FORTRAN `carsfit_co2` below):

The build process (or an executable you otherwise obtain) must be located at and named:

`src/fortran/co2_2pump/bin/carsfit_co2`

2) In MATLAB:

```matlab
cd ~/CHESS-CARS-ANALYSIS/cars-analysis
% If you didnt already do this, add the matlab path
addpath(genpath('src/matlab'));

% Optional: run the cfx+ tests
runtests('tests/matlab/cfx')

% Run carsfit_co2 from MATLAB (8 Enters, then N, N)
seq = [repmat("",8,1); "N"; "N"];
out = cfx.run_carsfit_script(seq, struct('workdir',"runs/demo1"));

% Load and plot the primary CSV
T = out.tables(out.primary_csv);
figure; plot(T{:,1}, T{:,2}, 'LineWidth', 1.2); grid on; xlabel('X'); ylabel('Y');

% Optional: generate a PNG plot via the Python tool
R = cfx.plot_csv_with_python(fullfile(out.workdir, out.primary_csv));
disp("PNG saved as " + R.png_out);
```

----------------

### Spectrum reader: `cfx.read_cars_spectrum`
---------------------------------------------

This utility is useful for reading the spectra output from the `carsfit` executable which should be found in the working directory where `carsfit` is running.  The user generally specifies the filename interactively, and in this README example we use `spec.out` as an example.

Reads a carsfit spectrum output file (e.g., `spec.out`) into a MATLAB struct with both:
- named vectors for direct plotting (`wavenumber`, `data`, `theory`, `residual`)
- a table (`T`) for convenient inspection and downstream processing
- optional header metadata such as a scalar `baseline` (if present in the comment header)

**Usage**

```matlab
S = cfx.read_cars_spectrum("spec.out");

% Plot theory vs wavenumber
figure;
plot(S.wavenumber, S.theory, "LineWidth", 1.2);
grid on;
xlabel("Wavenumber");
ylabel("Theory");
title("carsfit spectrum");
```

----------

### CSV plot data reader: `cfx.import_plot_csv`
----------------------------------------------

This utility is useful if your version of `carsfit` writes plotting data into CSV files.

Imports a carsfit-generated CSV and (if present) a same-base “sidecar” metadata file describing the CSV content (labels, title, units, etc.). This function does not plot by default; it returns a struct that makes plotting straightforward.

**Usage**:
```matlab
R = cfx.import_plot_csv("runs/demo1/pltchi_0001.csv");

% Minimal plot (first two numeric columns)
figure;
plot(R.x, R.y, "LineWidth", 1.2);
grid on;
xlabel("X");
ylabel("Y");
title("carsfit CSV");
```

Versions of `carsfit` which write CSV plotting data also write a metadata sidecar to describe the data inside each CSV file. The `cfx.import_plot_csv` utility should automatically find and read that metadata.  If it was successful, then you can use the metadata like so:

```matlab
figure;
plot(R.x, R.y, "LineWidth", 1.2);
grid on;

if isfield(R.meta, "xlabel"), xlabel(R.meta.xlabel); end
if isfield(R.meta, "ylabel"), ylabel(R.meta.ylabel); end
if isfield(R.meta, "title"),  title(R.meta.title);   end
```

--------------------------

### How to use the FORTRAN `carsfit_co2`
----------------------------------------

0) You need a FORTRAN compiler to build `carsfit_co2`

If you do not have a FORTRAN compiler, you'll need to install
one.  If you already have one, then one of these commands should
tell you if you have a common one:

```bash
which gfortran
which f90
which f77
which ifort
echo $FC
```

If none of those commands indicate you have a FORTRAN compiler
then try the following:

  - MAC: `brew install gcc` ([More details](https://www.google.com/search?q=install+gfortran+on+mac))
  - Ubuntu Linux: `aptget install gcc` ([Other Linux](https://www.google.com/search?q=install+gfortran+on+linux))
  - Windows: ([Try this](https://www.google.com/search?q=how+to+install+gcc+on+windows&oq=how+to+install+gcc+on+windows))

[Here's some more information about installing a FORTRAN compiler on different systems.](https://fortran-lang.org/learn/os_setup/install_gfortran/)

1) Build the `carsfit_co2` executable from FORTRAN code

This command **builds** the executable:

```bash
make -C src/fortran/co2_2pump
```

Then this command runs some tests to make sure it "works":

```bash
tests/fortran/co2_2pump/smoke/run_smoke.sh
```

2) Run `carsft`:

Examples of the required input files `cars.mol`, `cars.par`, `co2.mol`
can be found in the `data` directory.  In this example, we copy that
directory to the `testrun` directory as a demo.

```bash
cd src/fortran/co2_2pump
cp -r data testrun
cd testrun
../bin/carsfit_co2
```

If the run is successful, at least one dataset will be created:

```bash
ls -l pltchi_*.*
```

You should see something like this:

```
-rw-r--r--  1 mtcampbe  staff   220006 Dec 16 10:18 pltchi_0001.csv
-rw-r--r--  1 mtcampbe  staff       89 Dec 16 10:18 pltchi_0001.meta
```

The data to plot is in the CSV file, and a human-readable meta data
file explains what data can be found in the CSV.

3) Plot results:

A python utility is provided to plot the resulting data. It automatically
reads the meta data file for the axis labels, if it exists.

```bash
python ../../../python/carsfit-tools/plot_carsfit_csv.py pltchi_0001.csv
```

If you do not have `python` on your system, here is some info about how
to install python on [Mac](https://www.google.com/search?q=install+python+on+mac), [Linux](https://www.google.com/search?q=install+python+on+linux), and [Windows](https://www.google.com/search?q=install+python+on+windows).

----------------------------------

## Inline Plotting Support (carsfit)

The carsfit code includes optional **inline plotting** support that opens spectrum plots automatically during execution. This feature is implemented by spawning an external Python plotting utility from the Fortran code.

### Important requirement
**Inline plotting is only available when using the carsfit executable built from this package.**

If you run a carsfit executable obtained elsewhere (e.g., prebuilt, legacy, or user-supplied), inline plotting will not be available, even if the plotting utilities are installed.

---

## Plotting utilities

The plotting utilities live in the repository at:

`src/python/carsfit-tools/`


This directory contains:
- `plot_carsfit_csv.py` — the Python script that reads CSV output and displays plots
- `plot-carsfit`        — macOS/Linux wrapper
- `plot-carsfit.bat`    — Windows wrapper

The wrappers are responsible for invoking Python and locating the plotting script.

---

## Installation / setup (all platforms)

To enable inline plotting:

1. **Build and use the carsfit executable from this repository**
   - The Fortran code in this package is required for inline plotting support.

2. **Install the plotting utilities on your PATH**
   - Copy (or symlink) the following files into a directory on your PATH:
     - macOS / Linux:
       ```
       plot-carsfit
       plot_carsfit_csv.py
       ```
     - Windows:
       ```
       plot-carsfit.bat
       plot_carsfit_csv.py
       ```
   The wrapper and the Python script **must reside in the same directory**.
   If you are not sure about how to add a PATH to your executable PATH, then have a look at:
   [How to add a filesystem path to your executable path](https://www.google.com/search?q=add+executable+path+linux%2C+windows%2C+mac)

3. **Ensure Python and matplotlib are available**
   - Python 3 must be accessible as `python3` (macOS/Linux) or via the Python launcher on Windows.
   - The Python environment must have `matplotlib` installed.
   
   If you do not have `python` on your system, here is some info about how
   to install python on [Mac](https://www.google.com/search?q=install+python+on+mac),
   [Linux](https://www.google.com/search?q=install+python+on+linux), and
   [Windows](https://www.google.com/search?q=install+python+on+windows).

---

## Runtime behavior

When inline plotting is enabled:

- Selecting the plotting option in the carsfit menu will:
  1. Write CSV (and metadata) files for the plot
  2. Launch `plot-carsfit` in a **nonblocking** way
  3. Open a new plot window while carsfit continues running

- If the plotting utilities are **not** installed or not on PATH:
  - carsfit will silently skip plotting
  - no errors or warnings are emitted

This allows plotting support to be optional without affecting core simulation workflows.

---

## Notes

- On macOS, plot windows may come to the foreground when opened. This is normal OS behavior.
- Plotting behavior is intentionally isolated from the Fortran code; all Python and GUI logic lives in the external plotter.
- CSV and metadata files can always be post-processed manually using MATLAB or Python, even if inline plotting is disabled.

---

If you encounter issues with plotting, verify:
- you are using the carsfit executable built from this repository
- `plot-carsfit` is on your PATH
- Python and matplotlib are available
