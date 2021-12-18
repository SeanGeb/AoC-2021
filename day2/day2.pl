solve1([            ], F, D, R) :- R is F * D.
solve1([forward(N)|T], F, D, R) :- NewF is F + N, solve1(T, NewF, D, R).
solve1([down(N)   |T], F, D, R) :- NewD is D + N, solve1(T, F, NewD, R).
solve1([up(N)     |T], F, D, R) :- NewD is D - N, solve1(T, F, NewD, R).

% Course is a series of forward/up/down N commands
% Solution is (final horizontal position * final depth)
solve1(Course, Solution) :- solve1(Course, 0, 0, Solution).

solve2([            ], F, D, _, R) :- R is F * D.
solve2([forward(N)|T], F, D, A, R) :- NewF is F + N, NewD is D + (A * N), solve2(T, NewF, NewD, A, R).
solve2([down(N)   |T], F, D, A, R) :- NewA is A + N, solve2(T, F, D, NewA, R).
solve2([up(N)     |T], F, D, A, R) :- NewA is A - N, solve2(T, F, D, NewA, R).

% Course and Solution are as solve1/2, but considers the submarine's Aim parameter
solve2(Course, Solution) :- solve2(Course, 0, 0, 0, Solution).

% Read and handle file input

% True if the file at Path contains the provided Lines.
% Accepts both Windows and Unix line endings.
file_read_lines(Path, Lines) :-
    open(Path, read, Str),
    read_file(Str, Lines),
    close(Str).
read_file(Stream, []) :- at_end_of_stream(Stream).
read_file(Stream, [X|L]) :-
    \+ at_end_of_stream(Stream),
    read_string(Stream, "\n", "\r", _, X),
    read_file(Stream, L).

% Input lines are of the form /(up|down|forward) \d+/.
% These should be parsed into the predicates used above.
parse_line(Line, up(N)) :-
    string_concat("up ", NumStr, Line), number_string(N, NumStr).
parse_line(Line, down(N)) :-
    string_concat("down ", NumStr, Line), number_string(N, NumStr).
parse_line(Line, forward(N)) :-
    string_concat("forward ", NumStr, Line), number_string(N, NumStr).
parse_lines([], []).
parse_lines([L|Ls], [O|Os]) :- parse_line(L, O), parse_lines(Ls, Os).

solve_all(Path, Solution1, Solution2) :-
    file_read_lines(Path, Lines),
    parse_lines(Lines, Orders),
    solve1(Orders, Solution1),
    solve2(Orders, Solution2).
