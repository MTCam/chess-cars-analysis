# CARS Analysis Tools

[![MATLAB CI](https://github.com/chess-uiuc/cars-analysis/actions/workflows/matlab-ci.yml/badge.svg)](https://github.com/chess-uiuc/cars-analysis/actions/workflows/matlab-ci.yml)
[![CO2 Dual Pump CI](https://github.com/chess-uiuc/cars-analysis/actions/workflows/co2-ci.yml/badge.svg)](https://github.com/chess-uiuc/cars-analysis/actions/workflows/co2-ci.yml)

MATLAB & Fortran analysis tooling for cars data.

## Layout

### Main tools
- `src/matlab/carsft`  – Matlab source for CARSFT functions
- `src/matlab/+cfx` - Matlab interface for Fortran carsfit (CO2/2pump)
- `src/fortran/co2_2pump` - Fortran-based carsfit (CO2/2pump)
- `src/fortran/co2_2pump/data` - Data files for the fortran-based exe
- `src/python/carsfit-tools` - Python utilities for carsfit (e.g. plotting)

### Tests
- `tests/matlab/carsft` – Tests of CARSFT Matlab codes
- `tests/matlab/cfx` - Tests of the interface to Fortran carsfit (CO2/2pump)
- `tests/fortran/co2_2pump` - Tests for the CO2/dual pump code

------------------

## Using the CARS Analysis Suite


### Get the code:

Obtain the suite of CARS analysis tools with the following command
from your terminal:

```bash
mkdir ~/CHESS-CARS-ANALYSIS
cd ~/CHESS-CARS-ANALYSIS
git clone https://github.com/chess-uiuc/cars-analysis
cd cars-analysis
```

-----------------------

### How to use the Matlab code

Do the following inside of `Matlab`:

```matlab
% Navigate to the top level directory of the cars-analysis suite:
cd ~/CHESS-CARS-ANALYSIS/cars-analysis

% Add all MATLAB source folders to the path:
addpath(genpath('src/matlab'))

% Run the full MATLAB test suite
runtests('tests/matlab/carsft')
```

You can inspect, and change the `carsft` tests by looking in:
`tests/matlab/carsft/*.m`

You can inspect, and change `carsft` code by looking in:
`src/matlab/carsft/src/*.m`

--------------------------

### How to use the FORTRAN `carsfit_co2`

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

### How to use the Matlab interface to FORTRAN `carsfit_co2`

The Matlab interface to the FORTRAN code is called `cfx`. To use it, you must first build
or obtain the binary `carsfit_co2` executable for your particular platform. If you have a
FORTRAN compiler, the instructions above can help you build it.  Otherwise, you must obtain
an executable from someone that already has one.

The following steps should get you up and running with the Matlab interface to the FORTRAN
code.

1) Build or obtain the binary (See HOWTO build FORTRAN `carsfit_co2` above):

The build process (or an executable you otherwise obtain) must be located at and named:

`src/fortran/co2_2pump/bin/carsfit_co2`

2) In MATLAB:

```matlab
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

