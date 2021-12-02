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

