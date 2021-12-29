:- use_module(library(pio)).
% Requried for pio/DCG interop
:- set_prolog_flag(double_quotes, codes).

% Digit map:    Digit   Segments    #segments + unique
%  aaaaaa       0       abcefg      6
% b      c      1       cf          2 *
% b      c      2       acdeg       5
% b      c      3       acdfg       5
%  dddddd       4       bcdf        4 *
% e      f      5       abdfg       5
% e      f      6       abdefg      6
% e      f      7       acf         3 *
%  gggggg       8       abcdefg     7 *
%               9       abcdfg      6

% I solved part one by counting the number of matches for this regex:
% (?<=\|[^\n]*)(?<!\w)(\w{2,4}|\w{7})(?!\w)
% (?<=\|[^\n]*)   --> is after a literal "|" on the same line
% (?<!\w)         --> no letter beforehand
% (\w{2,4}|\w{7}) --> 2, 3, 4, or 7 letters
% (?!\w)          --> no letter afterwards

% Parsing rules for the full puzzle
% A single segment specification
seg(a) --> "a".
seg(b) --> "b".
seg(c) --> "c".
seg(d) --> "d".
seg(e) --> "e".
seg(f) --> "f".
seg(g) --> "g".

% Parses a specification of lit segments e.g. adcb into a list [0, 3, 2, 1].
segs([S|T]) --> seg(S), !, segs(T).
segs([]) --> [].

% Parses a space-seperated list of signals or outputs into a list of segs.
seglist([S]) --> segs(S).
seglist([S|T]) --> segs(S), " ", seglist(T).

% Parses a line of input into the segment mappings and the output digits being displayed.
puzzle(Signals, Digits) -->
    seglist(Signals), {length(Signals, 10)},
    " | ",
    seglist(Digits), {length(Digits, 4)}, !.

puzzles([]) --> [].
puzzles([(Signals, Digits)|Rem]) --> puzzle(Signals, Digits), "\n", puzzles(Rem).

% Objective: find a permutation of the terms a-g that satisfies the length and positioning constraints.
% mapping([A, B, ..., G]) maps segments to wires. mapping([d, ...]) would indicate segment a is driven by
% wire d.
% Predicate mapsat(Mapping, Signal) is true if Signal is a feasible order to light a digit, when
% using mapping.
mapsat(mapping([A, B, C, _, E, F, G]), [A, B, C, E, F, G]).    % 0
mapsat(mapping([_, _, C, _, _, F, _]), [C, F]).                % 1
mapsat(mapping([A, _, C, D, E, _, G]), [A, C, D, E, G]).       % 2
mapsat(mapping([A, _, C, D, _, F, G]), [A, C, D, F, G]).       % 3
mapsat(mapping([_, B, C, D, _, F, _]), [B, C, D, F]).          % 4
mapsat(mapping([A, B, _, D, _, F, G]), [A, B, D, F, G]).       % 5
mapsat(mapping([A, B, _, D, E, F, G]), [A, B, D, E, F, G]).    % 6
mapsat(mapping([A, _, C, _, _, F, _]), [A, C, F]).             % 7
mapsat(mapping([A, B, C, D, E, F, G]), [A, B, C, D, E, F, G]). % 8
mapsat(mapping([A, B, C, D, _, F, G]), [A, B, C, D, F, G]).    % 9

% Tests if Map is feasible for the provided list of signals.
mapsat_all(_, []).
mapsat_all(Map, [Signal|Signals]) :- 
    permutation(Signal, PermSignal),
    mapsat(Map, PermSignal),
    mapsat_all(Map, Signals).

% map_apply(Map, Input, Segment) applies Map to Input, indicating it lights Segment.
map_apply(mapping([A, _, _, _, _, _, _]), A, a).
map_apply(mapping([_, B, _, _, _, _, _]), B, b).
map_apply(mapping([_, _, C, _, _, _, _]), C, c).
map_apply(mapping([_, _, _, D, _, _, _]), D, d).
map_apply(mapping([_, _, _, _, E, _, _]), E, e).
map_apply(mapping([_, _, _, _, _, F, _]), F, f).
map_apply(mapping([_, _, _, _, _, _, G]), G, g).

% decode(Map, Input, Digit) determines what segments are lit
decode(_, [], display(f, f, f, f, f, f, f)).
decode(Map, [In|Ins], display(t, B, C, D, E, F, G)) :- map_apply(Map, In, a), decode(Map, Ins, display(f, B, C, D, E, F, G)).
decode(Map, [In|Ins], display(A, t, C, D, E, F, G)) :- map_apply(Map, In, b), decode(Map, Ins, display(A, f, C, D, E, F, G)).
decode(Map, [In|Ins], display(A, B, t, D, E, F, G)) :- map_apply(Map, In, c), decode(Map, Ins, display(A, B, f, D, E, F, G)).
decode(Map, [In|Ins], display(A, B, C, t, E, F, G)) :- map_apply(Map, In, d), decode(Map, Ins, display(A, B, C, f, E, F, G)).
decode(Map, [In|Ins], display(A, B, C, D, t, F, G)) :- map_apply(Map, In, e), decode(Map, Ins, display(A, B, C, D, f, F, G)).
decode(Map, [In|Ins], display(A, B, C, D, E, t, G)) :- map_apply(Map, In, f), decode(Map, Ins, display(A, B, C, D, E, f, G)).
decode(Map, [In|Ins], display(A, B, C, D, E, F, t)) :- map_apply(Map, In, g), decode(Map, Ins, display(A, B, C, D, E, F, f)).

% interpret(Display, N) relates display terms to their number.
%                 a  b  c  d  e  f  g
interpret(display(t, t, t, f, t, t, t), 0).
interpret(display(f, f, t, f, f, t, f), 1).
interpret(display(t, f, t, t, t, f, t), 2).
interpret(display(t, f, t, t, f, t, t), 3).
interpret(display(f, t, t, t, f, t, f), 4).
interpret(display(t, t, f, t, f, t, t), 5).
interpret(display(t, t, f, t, t, t, t), 6).
interpret(display(t, f, t, f, f, t, f), 7).
interpret(display(t, t, t, t, t, t, t), 8).
interpret(display(t, t, t, t, f, t, t), 9).

% input_number(Map, Input, Value) relates a set of Input wires to the displayed Number under Map.
input_number(Map, Input, Value) :- decode(Map, Input, Display), interpret(Display, Value).

% decode_all(Map, Inputs, Value)
decode_all(_, [], R, R).
decode_all(Map, [Input|Inputs], N, R) :-
    input_number(Map, Input, P),
    Q is N * 10 + P,
    decode_all(Map, Inputs, Q, R).
decode_all(Map, Inputs, N) :- decode_all(Map, Inputs, 0, N).

% Finally - we accept a list of (Signals, Digits) pairs, and solve each instance, summing the results.
solve2([], R, R).
solve2([(Signals, Digits)|Rem], PrevSum, Result) :-
    mapsat_all(Map, Signals), % find this line's mapping
    decode_all(Map, Digits, DisplayedNumber),
    Sum is PrevSum + DisplayedNumber,
    solve2(Rem, Sum, Result).
solve2(Pairs, Sum) :- solve2(Pairs, 0, Sum).
