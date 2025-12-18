function S = read_cars_spectrum(fname)
% CARSFIT_READ_SPEC_OUT Read carsfit spectrum output (spec.out) into MATLAB.
%
% Input:
%   fname : path to spec.out-like file
%
% Output struct S:
%   S.wavenumber : Nx1 double
%   S.data       : Nx1 double
%   S.theory     : Nx1 double
%   S.residual   : Nx1 double   (data-theory)
%   S.T          : table with the above columns

    arguments
        fname (1,1) string
    end

    % --- Etract scalar metadata from comment block
    meta = local_extract_comment_scalar(fname);

    % Find first numeric line (skip 'C' comment header and separators)
    firstDataLine = local_find_first_numeric_line(fname);

    % Configure import options explicitly
    opts = delimitedTextImportOptions("NumVariables", 4);
    opts.DataLines = [firstDataLine Inf];
    opts.Delimiter = {' ', '\t'};
    opts.ConsecutiveDelimitersRule = "join";
    opts.LeadingDelimitersRule = "ignore";

    opts.VariableNames = ["Wavenumber","Data","Theory","DataMinusTheory"];
    opts.VariableTypes = ["double","double","double","double"];
    opts.ExtraColumnsRule = "ignore";
    opts.EmptyLineRule = "read";

    T = readtable(fname, opts);

    S = struct();
    S.wavenumber = T.Wavenumber;
    S.data       = T.Data;
    S.theory     = T.Theory;
    S.residual   = T.DataMinusTheory;
    S.baseline   = meta; % mean offset
    S.T          = T;

end

function k = local_find_first_numeric_line(fname)
    fid = fopen(fname,'r');
    assert(fid>0, "Cannot open file: %s", fname);

    k = 1;
    while true
        t = fgetl(fid);
        if ~ischar(t)
            fclose(fid);
            error("No numeric data found in %s", fname);
        end

        tt = strtrim(t);
        if tt=="" || startsWith(tt,"C")
            k = k + 1;
            continue;
        end

        % numeric line begins with something parseable as a number
        if ~isempty(regexp(tt, '^[+-]?\d', 'once'))
            fclose(fid);
            return;
        end

        k = k + 1;
    end
end

function val = local_extract_comment_scalar(fname)
% Extract first numeric value found in comment lines beginning with 'C'

    fid = fopen(fname,'r');
    assert(fid>0, "Cannot open %s", fname);

    val = NaN;

    while true
        t = fgetl(fid);
        if ~ischar(t)
            break;
        end

        tt = strtrim(t);

        % Stop once we hit non-comment lines
        if ~startsWith(tt,"C")
            break;
        end

        % Replace Fortran D exponent with E
        tt = strrep(tt, 'D', 'E');
        tt = strrep(tt, 'd', 'e');

        % Look for a floating-point number anywhere in the line
        tok = regexp(tt, ...
            '([+-]?\d+(\.\d+)?([Ee][+-]?\d+)?)', ...
            'match', 'once');

        if ~isempty(tok)
            val = str2double(tok);
            break;
        end
    end

    fclose(fid);
end
