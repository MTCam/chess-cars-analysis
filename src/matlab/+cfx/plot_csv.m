function h = plot_csv(tbl, xcol, ycol)
if nargin < 2 || isempty(xcol) || isempty(ycol)
    vn = tbl.Properties.VariableNames;
    num_mask = varfun(@isnumeric, tbl, 'OutputFormat','uniform');
    vn = vn(num_mask);
    assert(numel(vn)>=2,'No two numeric columns to plot.');
    xcol = vn{1}; ycol = vn{2};
end
h = plot(tbl.(xcol), tbl.(ycol), '-','LineWidth',1.5); grid on;
xlabel(strrep(xcol,'_','\_')); ylabel(strrep(ycol,'_','\_'));
title('carsfit CSV quicklook');
end
