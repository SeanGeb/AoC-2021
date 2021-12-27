:- use_module(library(pio)).
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

% Parses a line consisting of a comma-seperated list of numbers.
puzzle(Data) --> intlist(Data), "\n".

% Aggregates a list of numbers into a list where the element at N is the number of times
% N was seen.
aggregate_counts([], X) :- initial_state(X).
aggregate_counts([Val|Vals], Result) :-
    aggregate_counts(Vals, NextResult),
    list_replace(Val, NextResult, NPlusOne, Result, N),
    NPlusOne is N + 1.

% Replaces OldValue with NewValue at position N in List, giving NewList.
list_replace(N, List, NewValue, NewList, OldValue) :- 
    nth0(N, List, OldValue, ListMinusOldValue),
    nth0(N, NewList, NewValue, ListMinusOldValue).

% State is a list where the element in position N is the number of fish with timer value N.
% At each step, shift every number down by one place; any that were in position 0 are added
% to position 6 (cooldown for their next fish), and also to position 8 (for the new fish produced).
step([Num0|Remainder], Result) :-
    length([Num0|Remainder], 9), % must be a length-9 array in first arg, allowing days 0 to 8 inclusive.
    list_replace(6, Remainder, Num6, Nums0to7, OldNum6),
    Num6 is Num0 + OldNum6,
    append(Nums0to7, [Num0], Result).

zero(0).
initial_state(L) :- length(L, 9), maplist(zero, L).

% Performs N steps
iterate_steps(Days, InitialState, FinalState) :-
    Days > 0,
    step(InitialState, NextState),
    NextDays is Days - 1,
    iterate_steps(NextDays, NextState, FinalState).
iterate_steps(0, FinalState, FinalState).

% Computes the sum of a state
state_sum([], 0).
state_sum([N|Ns], X) :- state_sum(Ns, Y), X is Y + N.

solve1(InitialCounters, FinalSum) :-
    aggregate_counts(InitialCounters, InitialState),
    iterate_steps(80, InitialState, Day80State),
    write(Day80State),
    state_sum(Day80State, FinalSum).
solve2(InitialCounters, FinalSum) :-
    aggregate_counts(InitialCounters, InitialState),
    iterate_steps(256, InitialState, Day256State),
    write(Day256State),
    state_sum(Day256State, FinalSum).
