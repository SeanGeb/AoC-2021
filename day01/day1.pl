solve1([], S, S).
solve1([_], S, S).
solve1([H1,H2|T], S, R) :- H1 < H2, SNew is S + 1, solve1([H2|T], SNew, R).
solve1([H1,H2|T], S, R) :- H1 >= H2, solve1([H2|T], S, R).

% Problem is a list of numbers
% Solution is the number of times Problem[i+1] > Problem[i]
solve1(Problem, Solution) :- solve1(Problem, 0, Solution).

solve2([], S, S).
solve2([_], S, S).
solve2([_, _], S, S).
solve2([_, _, _], S, S).
solve2([H1,H2,H3,H4|T], S, R) :- (H1+H2+H3) < (H2+H3+H4), SNew is S + 1, solve2([H2,H3,H4|T], SNew, R).
solve2([H1,H2,H3,H4|T], S, R) :- (H1+H2+H3) >= (H2+H3+H4), solve2([H2,H3,H4|T], S, R).

% Problem is a list of numbers
% Solution is the number of times sum(Problem[i+1..i+3]) >
% sum(Problem[i..i+2])
solve2(Problem, Solution) :- solve2(Problem, 0, Solution).

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

% list_number_string(?Numbers, ?Strings)
% True if Numbers and Strings are the same length, and every
% corresponding element of Numbers is represented in the corresponding
% element of Strings.
list_number_string([], []).
list_number_string([N|Nt], [S|St]) :-
    number_string(N, S), list_number_string(Nt, St).

solve_all(Path, Solution1, Solution2) :-
    file_read_lines(Path, Lines),
    list_number_string(Problem, Lines),
    solve1(Problem, Solution1),
    solve2(Problem, Solution2).
