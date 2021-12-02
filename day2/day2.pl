where([], 0, 0).
where([forward(N)|T], F, D) :- where(T, NewF, D   ), F is NewF + N.
where([down(N)   |T], F, D) :- where(T,    F, NewD), D is NewD + N.
where([up(N)     |T], F, D) :- where(T,    F, NewD), D is NewD - N.

solve1(Course, Solution) :- where(Course, F, D), Solution is F * D.

where([], 0, 0, _).
where([forward(N)|T], F, D, Aim) :-
    where(T, NewF, NewD, Aim),
    F is NewF + N,
    D is NewD + (Aim * N).
where([down(N)|T], F, D, Aim) :-
    NewAim is Aim + N,
    where(T, F, D, NewAim).
where([up(N)|T], F, D, Aim) :-
    NewAim is Aim - N,
    where(T, F, D, NewAim).

solve2(Course, Solution) :- where(Course, F, D, 0), Solution is F * D.

