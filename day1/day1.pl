incrs(_, [], 0).
incrs(Prev, [H|T], Result) :- H =< Prev, incrs(H, T, Result).
incrs(Prev, [H|T], Result) :- H > Prev, incrs(H, T, NextResult), Result is NextResult + 1.

% Unifies a list with the number of times, for each pair of adjacent
% elements (A, B), that B > A.
solve1([], 0).
solve1([H|T], Result) :- incrs(H, T, Result).


incrs3(_, [], 0).
incrs3(_, [_], 0).
incrs3(_, [_, _], 0).
incrs3(Prev, [H1, H2, H3 | T], R) :-
    New is H1 + H2 + H3,
    New =< Prev,
    incrs3(New, [H2, H3 | T], R).
incrs3(Prev, [H1, H2, H3 | T], R) :-
    New is H1 + H2 + H3,
    New > Prev,
    incrs3(New, [H2, H3 | T], Rn),
    R is Rn + 1.

% Unifies a list with the number of times, for each run of four adjacent
% elements (A, B, C, D), that B+C+D > A+B+C.
solve2([], 0).
solve2([_], 0).
solve2([_, _], 0).
solve2([H1, H2, H3 | T], R) :- N is H1 + H2 + H3, incrs3(N, [H2, H3 | T], R).
