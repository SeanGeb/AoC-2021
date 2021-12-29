:- use_module(library(pio)).
:- use_module(library(ordsets)).
:- use_module(library(yall)).
:- set_prolog_flag(double_quotes, codes).

row([])    --> "\n", !.
row([V|T]) --> [VCode], {number_codes(V, [VCode])}, !, row(T).
matrix([Row|Rem]) --> row(Row), !, matrix(Rem).
matrix([])        --> [].

parse(FileName, Matrix) :- phrase_from_file(matrix(M), FileName), list_matrix(M, Matrix).

% A matrix term takes the form matrix(MaxX, MaxY, Data), where Data is a
% (MaxY+1)-length list of (MaxX+1)-length lists.
% list_matrix(?List, ?Matrix) - convert between lists-of-lists and matrix terms.
list_matrix(List, matrix(MaxX, MaxY, List)) :-
    length(List, Rows), MaxY is Rows - 1,
    maplist(
        {Cols}/[List]>>length(List, Cols),
        List
    ),
    MaxX is Cols - 1. 


% matrix_get(Matrix, (X, Y), Value) unifies Value with the element at (X, Y) in Matrix.
matrix_get(Matrix, (X, Y), Value) :-
    nth0(Y, Matrix, Row),
    nth0(X, Row, Value).
matrix_get(matrix(_, _, Matrix), Point, Value) :- matrix_get(Matrix, Point, Value).

% neighbours(+Matrix, (+X, +Y), -Values) unifies Values with up to four adjacent values from Matrix.
neighbours(Matrix, Point, Values) :- neighbours(Matrix, Point, up, Values).

% neighbours(+Matrix, (+X, +Y), +Direction, -Values)
neighbours(matrix(MaxX, MaxY, Matrix), (X, Y), up, [V|Values]) :-
    Y < MaxY, !, Yp1 is Y + 1, matrix_get(Matrix, (X, Yp1), V),
    neighbours(matrix(MaxX, MaxY, Matrix), (X, Y), right, Values).
neighbours(matrix(MaxX, MaxY, Matrix), (X, MaxY), up, Values) :-
    !, neighbours(matrix(MaxX, MaxY, Matrix), (X, MaxY), right, Values).

neighbours(matrix(MaxX, MaxY, Matrix), (X, Y), right, [V|Values]) :-
    X < MaxX, !, Xp1 is X + 1, matrix_get(Matrix, (Xp1, Y), V),
    neighbours(matrix(MaxX, MaxY, Matrix), (X, Y), down, Values).
neighbours(matrix(MaxX, MaxY, Matrix), (MaxX, Y), right, Values) :-
    !, neighbours(matrix(MaxX, MaxY, Matrix), (MaxX, Y), down, Values).

neighbours(matrix(MaxX, MaxY, Matrix), (X, Y), down, [V|Values]) :-
    Y > 0, !, Ym1 is Y - 1, matrix_get(Matrix, (X, Ym1), V),
    neighbours(matrix(MaxX, MaxY, Matrix), (X, Y), left, Values).
neighbours(matrix(MaxX, MaxY, Matrix), (X, 0), down, Values) :-
    !, neighbours(matrix(MaxX, MaxY, Matrix), (X, 0), left, Values).

neighbours(matrix(_, _, Matrix), (X, Y), left, [V]) :-
    X > 0, !, Xm1 is X - 1, matrix_get(Matrix, (Xm1, Y), V).
neighbours(matrix(_, _, _), (0, _), left, []) :-
    !.

lt(X, Y) :- X < Y.

% is_minimum(+Matrix, (+X, +Y), ?PointValue) holds if the value at (X, Y) is
% strictly less than its neighbours, having value PointValue.
is_minimum(Matrix, Point, PointValue) :-
    matrix_get(Matrix, Point, PointValue),
    neighbours(Matrix, Point, NeighbourValues),
    maplist({PointValue}/[Neighbour]>>lt(PointValue, Neighbour), NeighbourValues).

% minimums(+Matrix, -Minimums) unifies Minimums with a list of points (X, Y)-Value
% strictly less than their neighbours.
minimums(Matrix, Minimums) :- minimums(Matrix, (0, 0), Minimums).

% minimums(+Matrix, (+X, +Y), -Minimums)
minimums(matrix(MaxX, MaxY, Matrix), (MaxX, MaxY), L) :- !, % tested whole matrix
    (is_minimum(matrix(MaxX, MaxY, Matrix), (MaxX, MaxY), V) -> L = [(MaxX, MaxY)-V]; L = []).
minimums(matrix(MaxX, MaxY, Matrix), (MaxX, Y), Minimums) :- !, % tested whole row
    (is_minimum(matrix(MaxX, MaxY, Matrix), (MaxX, Y), V) -> Minimums = [(MaxX, Y)-V|L]; Minimums = L),
    Yp1 is Y + 1,
    minimums(matrix(MaxX, MaxY, Matrix), (0, Yp1), L).
minimums(Matrix, Point, Minimums) :- !,
    (is_minimum(Matrix, Point, Value) -> Minimums = [Point-Value|L]; Minimums = L),
    (X, Y) = Point, Xp1 is X + 1,
    minimums(Matrix, (Xp1, Y), L).

sum_risks([], 0).
sum_risks([_-V|Rem], N) :- sum_risks(Rem, M), N is (V+1) + M.

solve1(Matrix, SumOfRisks) :-
    minimums(Matrix, MinimumPoints),
    sum_risks(MinimumPoints, SumOfRisks).

% For part 2, start with list of minimum points. For each of those points (the
% centre of a basin), explore outwards. Start with a queue containing the centre
% and push its strictly greater-valued neighbours into the queue. Count the
% number of locations visited to determine the basin size.

% neighbours_pv(+Matrix, (+X, +Y), -Points) unifies Points with up to four adjacent Points from Matrix.
% Each point is in (X,Y)-Value form.
neighbours_pv(Matrix, Point, Points) :- neighbours_pv(Matrix, Point, up, Points).

% neighbours_pv(+Matrix, (+X, +Y), +Direction, -Points)
neighbours_pv(matrix(MaxX, MaxY, Matrix), (X, Y), up, [(X, Yp1)-V|Points]) :-
    Y < MaxY, !, Yp1 is Y + 1, matrix_get(Matrix, (X, Yp1), V),
    neighbours_pv(matrix(MaxX, MaxY, Matrix), (X, Y), right, Points).
neighbours_pv(matrix(MaxX, MaxY, Matrix), (X, MaxY), up, Points) :-
    !, neighbours_pv(matrix(MaxX, MaxY, Matrix), (X, MaxY), right, Points).

neighbours_pv(matrix(MaxX, MaxY, Matrix), (X, Y), right, [(Xp1, Y)-V|Points]) :-
    X < MaxX, !, Xp1 is X + 1, matrix_get(Matrix, (Xp1, Y), V),
    neighbours_pv(matrix(MaxX, MaxY, Matrix), (X, Y), down, Points).
neighbours_pv(matrix(MaxX, MaxY, Matrix), (MaxX, Y), right, Points) :-
    !, neighbours_pv(matrix(MaxX, MaxY, Matrix), (MaxX, Y), down, Points).

neighbours_pv(matrix(MaxX, MaxY, Matrix), (X, Y), down, [(X, Ym1)-V|Points]) :-
    Y > 0, !, Ym1 is Y - 1, matrix_get(Matrix, (X, Ym1), V),
    neighbours_pv(matrix(MaxX, MaxY, Matrix), (X, Y), left, Points).
neighbours_pv(matrix(MaxX, MaxY, Matrix), (X, 0), down, Points) :-
    !, neighbours_pv(matrix(MaxX, MaxY, Matrix), (X, 0), left, Points).

neighbours_pv(matrix(_, _, Matrix), (X, Y), left, [(Xm1, Y)-V]) :-
    X > 0, !, Xm1 is X - 1, matrix_get(Matrix, (Xm1, Y), V).
neighbours_pv(matrix(_, _, _), (0, _), left, []) :-
    !.

gt(X, Y) :- X > Y.

% explore_basin(+Matrix, +StaringPoint-PointValue, -Size) unifies Size with the size of the basin formed around
% StartingPoint, given that StartingPoint has a value of PointValue.
explore_basin(Matrix, StartingPoint-PointValue, Size) :- explore_basin(Matrix, [StartingPoint-PointValue], [], Size).

% explore_basin(Matrix, Queue, Size, Result)
explore_basin(_, [], Seen, Result) :- length(Seen, Result), !.
explore_basin(Matrix, [_-9|QueueRem], Seen, Result) :- % ignore 9s
    !, explore_basin(Matrix, QueueRem, Seen, Result).
explore_basin(Matrix, [Point-Vp|QueueRem], Seen, Result) :-
    neighbours_pv(Matrix, Point, Neighbours),
    include({Vp}/[_-V]>>gt(V, Vp), Neighbours, LargerNeighbours),
    append(QueueRem, LargerNeighbours, NextQueue),
    ord_add_element(Seen, Point, NextSeen),
    explore_basin(Matrix, NextQueue, NextSeen, Result).

% all_basin_sizes determines the basin sizes corresponding to a list of minimum points, in (X, Y)-Value format.
all_basin_sizes(_, [], R, R) :- !.
all_basin_sizes(Matrix, [Minima|Minimums], Sizes, Result) :-
    explore_basin(Matrix, Minima, Size),
    NewSizes = [Size|Sizes],
    all_basin_sizes(Matrix, Minimums, NewSizes, Result).

solve2(Matrix, Solution) :-
    minimums(Matrix, Minimums),
    all_basin_sizes(Matrix, Minimums, [], Sizes),
    write(Sizes),
    msort(Sizes, SortedSizes),
    reverse(SortedSizes, [A, B, C|_]),
    write([A, B, C]),
    Solution is A * B * C.
