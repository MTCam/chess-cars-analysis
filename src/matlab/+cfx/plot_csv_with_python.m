function R = plot_csv_with_python(csv_file, opts)
    if nargin < 2 || isempty(opts), opts = struct(); end
    if ~isfield(opts,'python'), opts.python = 'python3'; end

    % --- normalize types to char ---
    if isstring(csv_file), csv_file = char(csv_file); end
    if isfield(opts,'python') && isstring(opts.python), opts.python = char(opts.python); end
    % --------------------------------

    if ~isfile(csv_file), error('CSV not found: %s', csv_file); end
    setenv('MPLBACKEND','Agg');

    thisdir = fileparts(mfilename('fullpath'));
    cand = {
        fullfile(thisdir,'..','..','python','carsfit-tools','plot_carsfit_csv_output.py')
        fullfile(thisdir,'..','..','python','carsfit-tools','plot_carsfit_csv.py')
    };
    plotter = '';
    for i = 1:numel(cand)
        if isfile(cand{i}), plotter = cand{i}; break; end
    end
    if isempty(plotter), error('Could not find plotter script in src/python/carsfit-tools/.'); end
    if isstring(plotter), plotter = char(plotter); end
    plotter = char(java.io.File(plotter).getCanonicalPath());

    [csv_dir, csv_base, ~] = fileparts(csv_file);
    if isstring(csv_dir),  csv_dir  = char(csv_dir);  end
    if isstring(csv_base), csv_base = char(csv_base); end

    png_out  = fullfile(csv_dir, [csv_base '.png']);
    if isstring(png_out),  png_out  = char(png_out);  end

    % exact CLI: SCRIPT INPUT.csv --out OUTPUT.png
    cmd = sprintf('%s "%s" "%s" --out "%s"', opts.python, plotter, csv_file, png_out);

    % use char concatenation to keep 'cmd' a char
    [st, outtxt] = system([cmd ' 2>&1']);

    % write debug log (ensure char)
    log_path = fullfile(csv_dir, [csv_base '_plotter.log']);
    if isstring(log_path), log_path = char(log_path); end
    fid = fopen(log_path,'w');
    if fid > 0
        fprintf(fid, "$ %s\n\n%s\n", cmd, outtxt);
        fclose(fid);
    end

    R = struct('status', st, 'png_out', png_out, 'cmd_used', cmd, ...
               'stdout', outtxt, 'log', log_path);
end
