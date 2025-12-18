function run_carsfit_exe(exe)
% CARSFIT_INTERACTIVE_LOOP Run carsfit interactively and plot spectrum output.
%
% Usage:
%   carsfit_interactive_loop("./carsfit-3")
%   carsfit_interactive_loop("/full/path/to/carsfit_co2")

    arguments
        exe (1,1) string
    end

    exe = string(exe);

    % Basic validation
    if ~isfile(exe) && ~isfile(which(exe))
        error("Executable not found: %s", exe);
    end

    fprintf("Using carsfit executable: %s\n", exe);

    while true
        % Run interactively
        status = system(quote_cmd(exe));
        if status ~= 0
            fprintf(2, "carsfit exited with status %d\n", status);
        end

        % User just specified this inside carsfit
        fname = string(input("Enter spectrum output filename (default: spec.out): ", "s"));
        if fname == ""
          % error("No output filename provided.");
          fname = "spec.out";
        end

        S = cfx.read_cars_spectrum(fname);

        figure;
        plot(S.wavenumber, S.theory, "LineWidth", 1.2);
        grid on;
        xlabel("Wavenumber");
        ylabel("Theory");
        if isfield(S,"baseline") && ~isnan(S.baseline)
            title(sprintf("carsfit spectrum (baseline = %g)", S.baseline));
        else
            title("carsfit spectrum");
        end

        again = lower(strtrim(input("Run another calculation? (y/n): ", "s")));
        if again ~= "y"
            break;
        end
    end
end

function s = quote_cmd(cmd)
    cmd = string(cmd);
    if contains(cmd, " ")
        s = """" + cmd + """";
    else
        s = cmd;
    end
end
