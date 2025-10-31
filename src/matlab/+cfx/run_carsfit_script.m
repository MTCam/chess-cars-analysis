function out = run_carsfit_script(script_or_lines, opts)
% Run the interactive Fortran carsfit_co2 using a scripted stdin sequence.
% - Copies required input data dir into the workdir before running.
% - Accepts either a path to a script file OR a string array of lines.
%
% Usage:
%   out = carskit.run_carsfit_script("path/to/script.txt");
%   out = carskit.run_carsfit_script(["", "", ... , "N", "N"]);  % 8 blanks, then N, N

arguments
  script_or_lines
  opts.exe (1,1) string = fullfile( ...
      fileparts(mfilename('fullpath')), "..","..", ...
      "fortran","co2_2pump","bin","carsfit_co2")
  opts.data_dir (1,1) string = fullfile( ...
      fileparts(mfilename('fullpath')), "..","..", ...
      "fortran","co2_2pump","data")
  opts.workdir (1,1) string = string(tempname)
  opts.timeout_sec (1,1) double = 300
end

% Prepare workdir and stage required input data
wd = opts.workdir; if ~isfolder(wd), mkdir(wd); end
if isfolder(opts.data_dir)
    % copy data contents into workdir (shallow copy)
    copyfile(fullfile(opts.data_dir, '*'), wd);
end

% Prepare stdin script file
if isstring(script_or_lines) && isscalar(script_or_lines) && isfile(script_or_lines)
    script_path = string(script_or_lines);
else
    % Accept a string array: each element is one line; "" produces a blank line.
    lines = string(script_or_lines(:));
    script_path = fullfile(wd, "carsfit_menu_script.txt");
    writelines(lines, script_path);
end

% Build command (platform-safe)
exe = string(opts.exe);
if ispc
    cmd = sprintf('cmd /c "(cd /d %s) & \"%s\" < \"%s\""', wd, exe, script_path);
else
    cmd = sprintf('bash -lc "cd %s && \"%s\" < \"%s\""', wd, exe, script_path);
end

t0 = datetime('now');
[st, msg] = system(cmd);
if st ~= 0
    warning('carsfit_co2 exit status %d: %s', st, msg);
end

% Collect CSVs produced during the run
d = dir(fullfile(wd, "*.csv"));
csv_files = strings(0,1);
for k = 1:numel(d)
    if d(k).datenum >= datenum(t0) - 1/24/60
        csv_files(end+1,1) = string(d(k).name); %#ok<AGROW>
    end
end

% Read CSVs (if you want tables immediately in MATLAB)
tables = containers.Map('KeyType','char','ValueType','any');
for k = 1:numel(csv_files)
    f = fullfile(wd, csv_files(k));
    try, tables(char(csv_files(k))) = readtable(f); catch, end
end

out = struct('status', double(~isempty(csv_files))-0, ...
             'workdir', wd, ...
             'csv_files', cellstr(csv_files), ...
             'tables', tables);
end
