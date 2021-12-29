:- use_module(library(dcg/basics)).
:- use_module(library(pio)).
:- set_prolog_flag(double_quotes, codes).

open_bracket(round)  --> "(".
open_bracket(square) --> "[".
open_bracket(curly)  --> "{".
open_bracket(tri)    --> "<".
close_bracket(round)  --> ")".
close_bracket(square) --> "]".
close_bracket(curly)  --> "}".
close_bracket(tri)    --> ">".

% corrupted(Missing) unifies Missing with the first incorrect closing bracket.
% If there aren't any incorrect closing brackets, the predicate fails.
corrupted(Missing) --> corrupted(Missing, []).

% corrupted(Missing, Stack) - stack is a list of unclosed bracket types
% on an empty stack, any closing bracket is inappropriate
corrupted(Missing, []) --> close_bracket(Missing), !.
% always push an open bracket to the stack
corrupted(Missing, Stack) --> open_bracket(Type), !, corrupted(Missing, [Type|Stack]).
% if we see an appropriate closing bracket, pop from the stack
corrupted(Missing, [Top|Stack]) --> close_bracket(Top), !, corrupted(Missing, Stack).
% if we see an inappropriate closing bracket, conclude.
corrupted(Actual, [Top|_]) --> close_bracket(Actual), {Actual \= Top}, !, string_without("\n", _).

% Score associates an improperly-placed closing bracket with its syntax error score.
score(round, 3).
score(square, 57).
score(curly, 1197).
score(tri, 25137).

% Computes an error score for a line
error_score(N) --> (
    corrupted(Missing)
    ->  {score(Missing, N)}
    ;   ({N = 0}, string_without("\n", _))
).

% Computes the total error scores over all lines
error_scores(N) --> error_scores(0, N).
error_scores(N, N) --> eos, !.
error_scores(N, R) --> error_score(M), eol, {X is N + M}, error_scores(X, R).

solve1(FileName, Score) :- phrase_from_file(error_scores(Score), FileName).

% completion(Completion) unifies Completion with the ordered list of brackets
% required to close the input brackets. If the input is corrupt, it fails.
completion(Compl) --> completion(Compl, []).
% completion(Completion, Stack)
completion(Stack, Stack) --> eos, !.
completion(Compl, Stack) --> open_bracket(Type), !, completion(Compl, [Type|Stack]).
completion(Compl, [T|Stack]) --> close_bracket(T), !, completion(Compl, Stack).

% Points for each bracket type in part 2
points(round, 1).
points(square, 2).
points(curly, 3).
points(tri, 4).

% Score a completion list
score_compl(N) --> score_compl(0, N).
score_compl(N, R) --> [X], {points(X, M), !, P is 5 * N + M}, score_compl(P, R).
score_compl(N, N) --> [], !.

lines([]) --> eos, !.
lines([L|Ls]) --> string_without("\n", L), eol, lines(Ls).

score_all_compls([], []).
score_all_compls([Line|Lines], [Score|Scores]) :-
    phrase(completion(Compl), Line), !,
    phrase(score_compl(Score), Compl),
    score_all_compls(Lines, Scores).
score_all_compls([_|Lines], Scores) :- score_all_compls(Lines, Scores).

median(UnsortValues, Median) :- msort(UnsortValues, Values), reverse(Values, RevValues), median(Values, RevValues, Median).
median([X|_], [X|_], X) :- !.
median([_|L], [_|M], X) :- median(L, M, X).

solve2(FileName, Solution) :-
    phrase_from_file(lines(Lines), FileName),
    score_all_compls(Lines, Scores),
    median(Scores, Solution).
