classdef TestCarsfitIntegration < matlab.unittest.TestCase
    methods (Test)
        function test_run_and_csv(test)
            addpath(genpath(fullfile(pwd,'src','matlab')));

            % Build the menu sequence: 8 blanks, then N, N
            seq = [repmat("",8,1); "N"; "N"];
            wd = string(tempname); mkdir(wd);

            out = cfx.run_carsfit_script(seq, struct('workdir',wd));

            test.verifyEqual(out.status, 1, 'carsfit produced no CSV');
            test.assertNotEmpty(out.csv_files, 'No CSV files found');
            % Verify we can read at least one table and it has >= 2 columns
            T = out.tables(out.csv_files{1});
            test.assertGreaterThanEqual(width(T), 2);
        end

        function test_python_plotter(test)
            % --- Python availability guard ---
            [st,~] = system('python -V');
            test.assumeEqual(st, 0, ...
                'Python not available on PATH; skipping plotter test.');
            % --------------------------------
            addpath(genpath(fullfile(pwd,'src','matlab')));
            seq = [repmat("",8,1); "N"; "N"];
            wd = string(tempname); mkdir(wd);

            out = cfx.run_carsfit_script(seq, struct('workdir',wd));
            csvf = fullfile(out.workdir, out.csv_files{1});

            png_out = cfx.plot_csv_with_python(csvf);
            test.assertTrue(isfile(png_out), 'Expected PNG not produced by Python plotter');
        end
    end
end
