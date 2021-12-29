% list_sum(La, Lb, Lc) is true if (replacing undef elements by 0),
% La[i] + Lb[i] == Lc[i].
list_sum([], Lb, Lb) :- !.
list_sum(La, [], La) :- !.
list_sum([La|LaT], [Lb|LbT], [Lc|LcT]) :-
    Lc is La + Lb,
    list_sum(LaT, LbT, LcT).


% BitStr is a list of strings of the form /[01]*/.
% Delta is a list representing BitStr, with LSB first, mapping every 1
% bit to a 1, and every 0 bit to a -1.
delta("", []) :- !.
delta(BitStr, [1|DeltasT]) :-
    string_concat(BitStrRem, "1", BitStr), !,
    delta(BitStrRem, DeltasT).
delta(BitStr, [-1|DeltasT]) :-
    string_concat(BitStrRem, "0", BitStr), !,
    delta(BitStrRem, DeltasT).

% list_deltas(Bitstrings, Deltas) is true if Deltas represents the sum
% of delta values for the provided list of bitstrings.
list_deltas([], []).
list_deltas([BitStr|BitStrT], NewDeltas) :-
    delta(BitStr, Delta),
    list_deltas(BitStrT, ListDeltas),
    list_sum(Delta, ListDeltas, NewDeltas).

% rates(Deltas, Gamma, Epsilon)
rates([N], 0, 1) :- N < 0, !.
rates([N], 1, 0) :- N > 0, !.
rates([0], _, _) :- !, fail.
rates([N|Nt], G, E) :-
    rates([N], Gn, En),
    rates(Nt, Gt, Et),
    G is Gn + Gt << 1,
    E is En + Et << 1.

solve1(Bitstrings, Power) :-
    list_deltas(Bitstrings, Deltas),
    rates(Deltas, Gamma, Epsilon),
    Power is Gamma * Epsilon.

% Part 2: eliminate elements based on prefix.
% For the oxy rate, keep elements with the most common value in the next
% position; for the CO2 rate, keep the least common next bit.

prefix_filter(_, [], []) :- !.
prefix_filter(Pre, [H|T], [H|R]) :-
    string_concat(Pre, _, H), !,
    prefix_filter(Pre, T, R).
prefix_filter(Pre, [H|T], R) :-
    \+ string_concat(Pre, _, H), !,
    prefix_filter(Pre, T, R).

list_all([], _Is) :- !.
list_all([Is|T], Is) :- list_all(T, Is).

oxy_rating(_, [Prefix], Prefix) :- !.
oxy_rating(Prefix, Bitstrings, R) :-
    string_concat(Prefix, "0", Prefix0),
    string_concat(Prefix, "1", Prefix1),
    prefix_filter(Prefix0, Bitstrings, Bitstrings0),
    prefix_filter(Prefix1, Bitstrings, Bitstrings1),
    length(Bitstrings0, Len0),
    length(Bitstrings1, Len1),
    (   Len1 >= Len0
    ->  oxy_rating(Prefix1, Bitstrings1, R)
    ;   oxy_rating(Prefix0, Bitstrings0, R)
    ).

co2_rating(_, [Prefix], Prefix) :- !.
co2_rating(Prefix, Bitstrings, R) :-
    string_concat(Prefix, "0", Prefix0),
    string_concat(Prefix, "1", Prefix1),
    prefix_filter(Prefix0, Bitstrings, Bitstrings0),
    prefix_filter(Prefix1, Bitstrings, Bitstrings1),
    length(Bitstrings0, Len0),
    length(Bitstrings1, Len1),
    (   Len0 =< Len1
    ->  co2_rating(Prefix0, Bitstrings0, R)
    ;   co2_rating(Prefix1, Bitstrings1, R)
    ).

% bitstring_number unifies a bitstring like "0101" with its decimal
% value like 5.
bitstring_number("0", 0) :- !.
bitstring_number("1", 1) :- !.
bitstring_number(S, N) :-
    string_length(S, SLen), SLen > 1,
    string_concat(Prefix, "0", S), !,
    bitstring_number(Prefix, NPrefix),
    N is NPrefix << 1.
bitstring_number(S, N) :-
    string_length(S, SLen), SLen > 1,
    string_concat(Prefix, "1", S), !,
    bitstring_number(Prefix, NPrefix),
    N is 1 + NPrefix << 1.

solve2(Bitstrings, Solution) :-
    oxy_rating("", Bitstrings, OxyRatingStr),
    co2_rating("", Bitstrings, Co2RatingStr),
    bitstring_number(OxyRatingStr, OxyRating),
    bitstring_number(Co2RatingStr, Co2Rating),
    Solution is OxyRating * Co2Rating.

% File handling

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

solve_all(Path, Solution1, Solution2) :-
    file_read_lines(Path, Lines),
    solve1(Lines, Solution1),
    solve2(Lines, Solution2).
