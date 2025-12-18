function out = run_carsfit_script(script_or_lines, opts)
% Allow optional 'opts' struct
if nargin < 2 || isempty(opts)
     opts = struct();
end

% Run the interactive Fortran carsfit_co2 using a scripted stdin sequence.
% - Copies required input data dir into the workdir before running.
% - Accepts either a path to a script file OR a string array of lines.
%
% Usage:
%   out = carskit.run_carsfit_script("path/to/script.txt");
%   out = carskit.run_carsfit_script(["", "", ... , "N", "N"]);  % 8 blanks, then N, N
if ~isfield(opts,'exe')
    opts.exe = fullfile(fileparts(mfilename('fullpath')),"..","..","fortran","co2_2pump","bin","carsfit_co2");
end
if ~isfield(opts,'data_dir')
    opts.data_dir = fullfile(fileparts(mfilename('fullpath')),"..","..","fortran","co2_2pump","data");
end
if ~isfield(opts,'workdir')
    opts.workdir = string(tempname);
end
if ~isfield(opts,'timeout_sec')
    opts.timeout_sec = 300;
end

% Prepare workdir and stage required input data
wd = opts.workdir;
if ~isfolder(wd), mkdir(wd); end
wd = string(java.io.File(wd).getCanonicalPath());

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

% Force absolute, canonical path for redirection
script_path = string(java.io.File(script_path).getCanonicalPath());

% Build command (platform-safe)
exe = string(opts.exe);

if ispc
    cmd = sprintf('cmd /c "cd /d "%s" && "%s" < "%s""', wd, exe, script_path);
else
    cmd = sprintf('bash -c ''cd "%s" && "%s" < "%s"''', wd, exe, script_path);
end

t0 = datetime('now');
[st, msg] = system(cmd);
if st ~= 0
    warning('carsfit_co2 exit status %d: %s', st, msg);
end

% Collect all CSVs, newest first, as a cell array of char
d = dir(fullfile(wd, "*.csv"));
% keep only files created near this run (same filter you had)
d = d([d.datenum] >= datenum(t0) - 1/24/60);
% newest first (just in case multiple are created)
[~, idx] = sort([d.datenum], 'descend');
d = d(idx);

csv_files = cell(numel(d),1);
for k = 1:numel(d)
    csv_files{k,1} = char(d(k).name);
end

% Build map for every CSV
tables = containers.Map('KeyType','char','ValueType','any');
for k = 1:numel(csv_files)
    f = fullfile(wd, csv_files{k});
    try
        T = readtable(f);
        tables(csv_files{k}) = T;
    catch ME
        warning('Failed to read %s: %s', f, ME.message);
    end
end

% Primary = newest (first) if any
primary_csv = '';
if ~isempty(csv_files)
    primary_csv = csv_files{1};
end

out = struct( ...
    'status', double(~isempty(csv_files)), ...
    'workdir', wd, ...
    'csv_files', csv_files, ...   % cell array of char filenames (maybe many)
    'primary_csv', primary_csv, ... % convenient single pick (newest)
    'tables', tables);

end
