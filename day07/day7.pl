:- use_module(library(pio)).
% Requried for pio/DCG interop
:- set_prolog_flag(double_quotes, codes).
:- use_module(library(assoc)).

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

% Parses a line consisting of a comma-seperated list of numbers.
puzzle(Data) --> intlist(Data), "\n".

% Convert puzzle input (list of positions) into an associative list mapping
% position to counts
put_vals([], Result, Result).
put_vals([N|Ns], PrevAssoc, Result) :-
    (   % increment value if exists, else put a 1
        get_assoc(N, PrevAssoc, PrevX, NextAssoc, NewX)
    ->  NewX is PrevX + 1
    ;   put_assoc(N, PrevAssoc, 1, NextAssoc)
    ), put_vals(Ns, NextAssoc, Result).
put_vals(Values, Assoc) :- empty_assoc(Initial), put_vals(Values, Initial, Assoc).

% Iterate over keys/values of assoc A: given Target of N, produces
% Σ_{k, v ∈ pairs(A)} |k-N|⋅v
movement_cost([], _Target, Cost, Cost).
movement_cost([Pos-Num|Pairs], Target, PrevCost, Result) :-
    Distance is abs(Pos-Target),
    NextCost is PrevCost + Distance * Num,
    movement_cost(Pairs, Target, NextCost, Result).
movement_cost(Pairs, Target, Cost) :- movement_cost(Pairs, Target, 0, Cost).

% optimal_position(Assoc, Target, CurrentBestScore, CurrentBestPosition, BestCost, BestPosition)
optimal_position(Assoc, Target, BestCost, BestPosition, BestCost, BestPosition) :-
    max_assoc(Assoc, Target, _), !. % done when target is largest value in assoc
optimal_position(Assoc, Target, CurrentBestCost, CurrentBestPosition, BestCost, BestPosition) :-
    assoc_to_list(Assoc, Pairs),
    movement_cost(Pairs, Target, CurrentCost),
    NextTarget is Target + 1,
    (
        CurrentCost < CurrentBestCost
    ->  optimal_position(Assoc, NextTarget, CurrentCost, Target, BestCost, BestPosition)
    ;   optimal_position(Assoc, NextTarget, CurrentBestCost, CurrentBestPosition, BestCost, BestPosition)
    ).
optimal_position(Assoc, BestCost, BestPosition) :-
    min_assoc(Assoc, InitialPosition, _),
    optimal_position(Assoc, InitialPosition, inf, InitialPosition, BestCost, BestPosition).

solve1(Puzzle, Solution) :-
    put_vals(Puzzle, Assoc),
    optimal_position(Assoc, Solution, _).


% Iterate over keys/values of assoc A: given Target of N, and D = |k-N|, produces
% Σ_{k, v ∈ pairs(A)} ½⋅D⋅(D-1)⋅v
movement_cost_2([], _Target, Cost, Cost).
movement_cost_2([Pos-Num|Pairs], Target, PrevCost, Result) :-
    Distance is abs(Pos-Target),
    DistancePlusOne is Distance + 1,
    ThisCost is (DistancePlusOne * Distance * Num) // 2,
    NextCost is PrevCost + ThisCost,
    movement_cost_2(Pairs, Target, NextCost, Result).
movement_cost_2(Pairs, Target, Cost) :- movement_cost_2(Pairs, Target, 0, Cost).

% optimal_position_2(Assoc, Target, CurrentBestScore, CurrentBestPosition, BestCost, BestPosition)
optimal_position_2(Assoc, Target, BestCost, BestPosition, BestCost, BestPosition) :-
    max_assoc(Assoc, Target, _), !. % done when target is largest value in assoc
optimal_position_2(Assoc, Target, CurrentBestCost, CurrentBestPosition, BestCost, BestPosition) :-
    assoc_to_list(Assoc, Pairs),
    movement_cost_2(Pairs, Target, CurrentCost),
    NextTarget is Target + 1,
    (
        CurrentCost < CurrentBestCost
    ->  optimal_position_2(Assoc, NextTarget, CurrentCost, Target, BestCost, BestPosition)
    ;   optimal_position_2(Assoc, NextTarget, CurrentBestCost, CurrentBestPosition, BestCost, BestPosition)
    ).
optimal_position_2(Assoc, BestCost, BestPosition) :-
    min_assoc(Assoc, InitialPosition, _),
    optimal_position_2(Assoc, InitialPosition, inf, InitialPosition, BestCost, BestPosition).

solve2(Puzzle, Solution) :-
    put_vals(Puzzle, Assoc),
    optimal_position_2(Assoc, Solution, _).



write_assoc_pairs([]). 
write_assoc_pairs([K-V|T]) :- format("~d -> ~d~n", [K, V]), write_assoc_pairs(T).
write_assoc(Assoc) :- assoc_to_list(Assoc, Pairs), write_assoc_pairs(Pairs).
