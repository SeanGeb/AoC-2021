incrs(_, [], 0).
incrs(N, [H|T], R) :- H =< N, incrs(H, T, R).
incrs(N, [H|T], R) :- H > N, incrs(H, T, Rn), R is Rn + 1.

% Unifies a list with the number of times, for each pair of adjacent
% elements (A, B), that B > A.
incrs([], 0).
incrs([H|T], R) :- incrs(H, T, R).


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
incrs3([], 0).
incrs3([_], 0).
incrs3([_, _], 0).
incrs3([H1, H2, H3 | T], R) :- N is H1 + H2 + H3, incrs3(N, [H2, H3 | T], R).
