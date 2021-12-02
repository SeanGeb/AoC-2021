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
