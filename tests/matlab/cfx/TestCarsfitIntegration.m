classdef TestCarsfitIntegration < matlab.unittest.TestCase
    methods (Test)
        function test_run_and_csv(test)
            addpath(genpath(fullfile(pwd,'src','matlab')));

            % Build the menu sequence: 8 blanks, then N, N
            seq = [repmat("",8,1); "N"; "N"];
            wd = string(tempname); mkdir(wd);

            out = cfx.run_carsfit_script(seq, struct('workdir',wd));

            % Existence checks
            test.assertEqual(out.status, 1, 'carsfit did not produce CSVs');
            test.assertGreaterThanOrEqual(numel(out.csv_files), 1, 'No CSV files found');
            test.assertNotEmpty(out.primary_csv, 'primary_csv was not set');

            % Load the primary CSV table and sanity-check it
            T = out.tables(out.primary_csv);
            test.assertGreaterThanOrEqual(width(T), 2, 'Expected at least two columns in CSV table');

            disp(struct('csv_files_class',class(out.csv_files), ...
                        'is_cell',      iscell(out.csv_files), ...
                        'is_string',    isstring(out.csv_files), ...
                        'is_char',      ischar(out.csv_files)));
            
            % k = keys(out.tables);           % always returns a cell array
            % T = out.tables(k{1});           % safe brace indexing into a cell
            % T = out.tables(out.csv_files{1})
        end

        function test_python_plotter(test)
            % Guard: python + deps
            [st3,~] = system('python3 -c "import numpy, matplotlib"');
            if st3 ~= 0
                [st,~] = system('python -c "import numpy, matplotlib"');
                test.assumeEqual(st, 0, 'Python with numpy+matplotlib not available; skipping.');
            end

            addpath(genpath(fullfile(pwd,'src','matlab')));

            % Run the minimal menu sequence (8 Enters, then N, N)
            seq = [repmat("",8,1); "N"; "N"];
            wd = string(tempname); mkdir(wd);
            out = cfx.run_carsfit_script(seq, struct('workdir',wd));

            test.assertGreaterThanOrEqual(numel(out.csv_files), 1, 'No CSV files found');
            test.assertNotEmpty(out.primary_csv, 'primary_csv not set');

            % Pass the CSV *file path* to the plotter
            csv_path = fullfile(out.workdir, out.primary_csv);

            R = cfx.plot_csv_with_python(csv_path);
            if ~(R.status==0 && isfile(R.png_out))
                if isfile(R.log)
                    fprintf(1, '\n--- plotter log ---\n%s\n--------------------\n', fileread(R.log));
                end
            end
            test.assertTrue(R.status==0 && isfile(R.png_out), ...
            sprintf('PNG not produced. cmd=\"%s\" log=%s', R.cmd_used, R.log));
        end
    end
end
