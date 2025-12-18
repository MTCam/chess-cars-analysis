function R = import_plot_csv(csv_path)
%CFX.IMPORT_PLOT_CSV Import a carsfit-generated CSV and optional metadata.
%
% R = cfx.import_plot_csv(csv_path)
%
% Inputs
%   csv_path : string/char path to CSV file
%
% Outputs (struct R)
%   R.csv_path        : absolute path to CSV
%   R.meta_path       : metadata file path if found, else ""
%   R.meta            : struct with metadata (may be empty struct)
%   R.T               : table from CSV
%   R.x               : first column as Nx1 double (if numeric)
%   R.y               : second column as Nx1 double (if numeric)
%   R.varnames        : table variable names
%
% Metadata sidecar search order (same base name as CSV):
%   <base>.json
%   <base>.meta.json
%   <base>.metadata.json
%   <base>.txt
%   <base>.meta
%
% JSON metadata is preferred. TXT/META expects "key: value" or "key=value".

    arguments
        csv_path (1,1) string
    end

    csv_path = string(csv_path);
    if ~isfile(csv_path)
        error("CSV not found: %s", csv_path);
    end

    % Canonicalize path
    csv_path_abs = string(java.io.File(csv_path).getCanonicalPath());

    % Read CSV as table (robust for headers/no headers)
    % - If no header line exists, MATLAB will generate Var1, Var2, ...
    T = readtable(csv_path_abs, "FileType","text");

    R = struct();
    R.csv_path  = csv_path_abs;
    R.meta_path = "";
    R.meta      = struct();
    R.T         = T;
    R.varnames  = string(T.Properties.VariableNames);

    % Try to infer x/y vectors if first two cols are numeric
    R.x = [];
    R.y = [];
    if width(T) >= 2
        c1 = T{:,1};
        c2 = T{:,2};
        if isnumeric(c1) && isnumeric(c2)
            R.x = c1;
            R.y = c2;
        end
    end

    % Find and parse metadata if present
    meta_path = local_find_sidecar(csv_path_abs);
    if meta_path ~= ""
        R.meta_path = meta_path;
        R.meta = local_parse_metadata(meta_path);
    end
end

function meta_path = local_find_sidecar(csv_path_abs)
    [p, base, ~] = fileparts(csv_path_abs);

    candidates = [
        fullfile(p, base + ".json")
        fullfile(p, base + ".meta.json")
        fullfile(p, base + ".metadata.json")
        fullfile(p, base + ".txt")
        fullfile(p, base + ".meta")
    ];

    meta_path = "";
    for i = 1:numel(candidates)
        if isfile(candidates(i))
            meta_path = string(candidates(i));
            return;
        end
    end
end

function meta = local_parse_metadata(meta_path)
    meta = struct();
    meta_path = string(meta_path);

    [~,~,ext] = fileparts(meta_path);
    ext = lower(string(ext));

    if ext == ".json"
        txt = fileread(meta_path);
        try
            meta = jsondecode(txt);
            if ~isstruct(meta)
                % jsondecode can return non-struct (e.g., cell/array); wrap it
                meta = struct("value", meta);
            end
        catch ME
            warning("Failed to parse JSON metadata (%s): %s", meta_path, ME.message);
            meta = struct();
        end
        return;
    end

    % Text "key: value" or "key=value" parser
    try
        lines = splitlines(string(fileread(meta_path)));
        for k = 1:numel(lines)
            s = strtrim(lines(k));
            if s == "" || startsWith(s,"#") || startsWith(s,"%")
                continue;
            end

            % Split on first ':' or '='
            idx = regexp(s, '[:=]', 'once');
            if isempty(idx), continue; end

            key = strtrim(extractBefore(s, idx));
            val = strtrim(extractAfter(s, idx));

            if key == "", continue; end

            % Make a valid MATLAB field name
            f = matlab.lang.makeValidName(char(key));

            % Try numeric coercion
            vnum = str2double(val);
            if ~isnan(vnum) && ~contains(lower(val), lettersPattern)
                meta.(f) = vnum;
            else
                meta.(f) = char(val);
            end
        end
    catch ME
        warning("Failed to parse text metadata (%s): %s", meta_path, ME.message);
        meta = struct();
    end
end
