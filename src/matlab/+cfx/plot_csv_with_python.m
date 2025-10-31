function png_out = plot_csv_with_python(csv_file, varargin)
% Call your existing Python plotter and return the output PNG path.
% Assumes: plot_cars_csv_output.py writes a PNG next to the CSV (or accept -o).

arguments
  csv_file (1,1) string
end
arguments (Repeating)
  varargin
end

pyplot = fullfile(fileparts(mfilename('fullpath')), ...
    "..","..","python","carsfit-tools", "plot_cars_csv.py"); % adjust if needed

% If your plotter supports -o, use it; otherwise let it drop next to CSV
[folder, base, ~] = fileparts(csv_file);
png_out = fullfile(folder, base + ".png");

% Try: python -V to pick interpreter if needed; here rely on PATH
cmd = sprintf('python "%s" "%s" -o "%s"', pyplot, csv_file, png_out);
[st, msg] = system(cmd);
if st ~= 0
    warning('Python plotter failed: %s', msg);
end

if isfile(png_out)
    % Preview in MATLAB
    I = imread(png_out);
    figure('Name','carsfit plot'); imshow(I);
else
    warning('Expected plot not found: %s', png_out);
end
end
