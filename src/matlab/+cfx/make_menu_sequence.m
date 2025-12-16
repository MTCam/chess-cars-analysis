function seq = make_menu_sequence(n_blank, tail_choices)
% Build a string array for the stdin script.
% Example: seq = make_menu_sequence(8, ["N","N"]);
seq = [repmat("", n_blank, 1); string(tail_choices(:))];
end
