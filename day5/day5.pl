:- use_module(library(pio)).
:- use_module(library(lists)).
% Requried for pio/DCG interop
:- set_prolog_flag(double_quotes, codes).

% Parses an integer from a string
intstr(I) -->   % an intstr is...
    digit(D0),  % a single digit in [0-9] called D0
    digits(D),  % followed by 0 or more digits
    { number_codes(I, [D0|D]) }. % converted to an int by number_codes

% Digits is 0 or more chars in [0-9]
digits([D|T]) --> digit(D), !, digits(T).
digits([])    --> "".

% Digit is a char in [0-9]
digit(D) --> [D], { code_type(D, digit) }.

% Parses a comma-seperated list of ints
intlist([H|T]) -->  % an intlist can be...
    intstr(H),      % a string representation of an int
    ",",            % followed by a comma
    !,              % so we commit to finding another int
    intlist(T).     % and finally another intlist
intlist([H]) -->    % an intlist can also be...
    intstr(H),      % a string representation of an int
    !.              % to which we commit (would have matched previous if another int)
intlist([]) --> "". % an empty string is an empty list of ints.

% Greedily consumes any whitespace characters, not allowing backtracking.
optspaces --> " ", !, optspaces.
optspaces --> "".

% Parses lines describing a vent, of the form
% [x0],[y0] -> [x1],[y1]\n
vents([vent((X0, Y0), (X1, Y1))|T]) -->
    intlist([X0, Y0]),    % first x,y pair
    " -> ",               % ->
    intlist([X1, Y1]),    % second x,y pair
    "\n",                 % newline
    !,                    % commit
    vents(T).             % then more vents
vents([]) --> "". % an empty string is a list of 0 vents

% Determines the largest value in the list provided
list_max([X], X) :- !.
list_max([X,Y|T], M) :-
    (
        X > Y
    ->  list_max([X|T], M)
    ;   list_max([Y|T], M)
    ).

% Determines the maximum X and Y co-ords used in the provided vents lists
max_xy([], 0, 0).
max_xy([vent((X0, Y0), (X1, Y1))|T], X, Y) :-
    max_xy(T, Xmax, Ymax),
    list_max([X0, X1, Xmax], X),
    list_max([Y0, Y1, Ymax], Y).

% Given a line between two points, unifies with the next point along that line.
% Straight horizontal line (shared Y)
creep((X0, Y), (X1, Y), (Xn, Y)) :- !, X0 =\= X1, (X0 < X1 -> Xn is X0 + 1; Xn is X0 - 1).
% Straight vertical line (shared X)
creep((X, Y0), (X, Y1), (X, Yn)) :- !, Y0 =\= Y1, (Y0 < Y1 -> Yn is Y0 + 1; Yn is Y0 - 1).
% Diagonal line (|X1-X0| == |Y1-Y0|)
creep((X0, Y0), (X1, Y1), (Xn, Yn)) :-
    LenX is abs(X1 - X0), LenY is abs(Y1 - Y0),
    LenX == LenY, !,         % points must lie on a diagonal line
    (X0 < X1 -> Xn is X0 + 1; Xn is X0 - 1),
    (Y0 < Y1 -> Yn is Y0 + 1; Yn is Y0 - 1).

logical_xor(A, B) :- \+A, !, B. logical_xor(A, B) :- A, !, \+B.

% Unifies a line description with the list of cells it passes through
line_cells((X0, Y0), (X1, Y1), [(X0, Y0)|Others]) :-
    logical_xor(X0 == X1, Y0 == Y1), !,     % must share an X or Y, but not both
    creep((X0, Y0), (X1, Y1), (Xn, Yn)),    % creep to next (X, Y)
    line_cells((Xn, Yn), (X1, Y1), Others).
line_cells((X0, Y0), (X1, Y1), []) :-
    (X0 =\= X1, Y0 =\= Y1), !.              % ignore diagonals entirely
line_cells((X, Y), (X, Y), [(X, Y)]) :- !.  % Finished after creeping to target.

vent_cells(vent((X0, Y0), (X1, Y1)), Cells) :- line_cells((X0, Y0), (X1, Y1), Cells).

% As above, but including diagonals
line_cells_diags((X, Y), (X, Y), [(X, Y)]) :- !.  % Finished after creeping to target.
line_cells_diags((X0, Y0), (X1, Y1), [(X0, Y0)|Others]) :-
    creep((X0, Y0), (X1, Y1), (Xn, Yn)),    % creep to next (X, Y) - unconditionally
    line_cells_diags((Xn, Yn), (X1, Y1), Others).

vent_cells_diags(vent((X0, Y0), (X1, Y1)), Cells) :- line_cells_diags((X0, Y0), (X1, Y1), Cells).

% Produces a list of N zeros.
zerolist(0, []) :- !.
zerolist(N, [0|T]) :- N > 0, NextN is N - 1, zerolist(NextN, T).
% Produces a list of length Y, each element being a list of X zeros.
zeroarray(_, 0, []) :- !.
zeroarray(X, Y, [ZeroRow|T]) :- X > 0, Y > 0, NextY is Y - 1, zerolist(X, ZeroRow), zeroarray(X, NextY, T).

% Increment the value at position N in List by 1 in NewList
list_increment(N, List, NewList) :-
    nth0(N, List, OldValue, ListMinusOldValue),
    NewValue is OldValue + 1,
    nth0(N, NewList, NewValue, ListMinusOldValue).
% Increment the value at position X in the list at position Y of array.
array_increment((X, Y), Array, NewArray) :-
    nth0(Y, Array, OldRow, ArrayMinusOldRow),
    list_increment(X, OldRow, NewRow),
    nth0(Y, NewArray, NewRow, ArrayMinusOldRow).

% Given a list of cells and current state map, increment the cells listed.
mark_cells([], FinalState, FinalState) :- !.
mark_cells([Cell|Cells], State, FinalState) :-
   array_increment(Cell, State, NextState),
   mark_cells(Cells, NextState, FinalState).

% Given a list of vents, determine the array showing how many vents pass through each cell
mark_all_vents([], Result, Result).
mark_all_vents([Vent|Vents], InitialState, Result) :-
    write_vent(Vent),
    vent_cells(Vent, Cells),
    mark_cells(Cells, InitialState, NextState),
    mark_all_vents(Vents, NextState, Result).

% As above, but include diagonals too
mark_all_vents_diags([], Result, Result).
mark_all_vents_diags([Vent|Vents], InitialState, Result) :-
    vent_cells_diags(Vent, Cells),
    mark_cells(Cells, InitialState, NextState),
    mark_all_vents_diags(Vents, NextState, Result).

% Given a state, count the number of cells with value >= 2.
count_ge2([], 0) :- !.
count_ge2([[]|T], N) :- count_ge2(T, N).
count_ge2([[X|T1]|T2], N) :-
    count_ge2([T1|T2], M),
    (
        X >= 2
    ->  N is M + 1
    ;   N is M
    ).

solve1(Vents, CountGE2) :-
    max_xy(Vents, X0, Y0),
    X is X0 + 1, Y is Y0 + 1,
    zeroarray(X, Y, InitialState),
    mark_all_vents(Vents, InitialState, MarkedState),
    count_ge2(MarkedState, CountGE2).
solve2(Vents, CountGE2) :-
    max_xy(Vents, X0, Y0),
    X is X0 + 1, Y is Y0 + 1,
    zeroarray(X, Y, InitialState),
    mark_all_vents_diags(Vents, InitialState, MarkedState),
    count_ge2(MarkedState, CountGE2).

write_state([]).
write_state([[]|T]) :- format("~n"), write_state(T).
write_state([[H|T1]|T2]) :- format("~d  ", [H]), write_state([T1|T2]).

write_vent(vent((X0, Y0), (X1, Y1))) :- format("~d, ~d --> ~d, ~d~n", [X0, Y0, X1, Y1]).
