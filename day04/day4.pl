% Format:

% n,n,n,n,...,n     | bingo numbers in drawing order
%
% n n n n n         |
% n n n n n         | bingo card. more 
% n n n n n         | cards follow, each
% n n n n n         | seperated with \n
% n n n n n         |
%
% n n n n n ...

% Want:
% - Find first winning board, then using that board only:
%   - Find the sum of the unmarked numbers on that board
%   - Multiply that sum by the last number called
%   - Solution is the product of those values

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% --- Parsing the input --- %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

string_number(String, Number) :- number_string(Number, String).

% e.g. draw_order("1,2,3", [1,2,3])
draw_order(Str, Nums) :-
    split_string(Str, ",", "", NumStrs),
    maplist(string_number, NumStrs, Nums).

% String is a length-5 list of row strings
% Card is a length-5 list of length-5 lists, each list representing one row on the card
card(Str, card(CardRows)) :-
    maplist(parse_row, Str, CardRows).

% e.g. number_unmarkednum(5, n(5))
number_unmarkednum(X, n(X)).

% e.g. parse_row("1 2 3 4 5", [n(1), n(2), n(3), n(4), n(5)])
parse_row(Str, Nums) :-
    split_string(Str, " ", " ", NumStrs),
    maplist(string_number, NumStrs, ParsedNums),
    maplist(number_unmarkednum, ParsedNums, Nums).

% e.g. split_doublenl("foo\n\nbar", ["foo", "bar"])
split_doublenl(Str, Res) :-
    split_string(Str, "\n", "", Lines),
    split_by_blanks(Lines, [], Res).

% e.g. split_by_blanks(["a", "b", "c", "", "def"], [["a", "b", "c"], ["def"]])
split_by_blanks([], Acc, [Acc]).
split_by_blanks([""|Tail], Acc, [Acc|R]) :-
    !, split_by_blanks(Tail, [], R).
split_by_blanks([H|T], Acc, R) :-
    append(Acc, [H], AccNext),
    split_by_blanks(T, AccNext, R).

% Parses the draw and cards from Str
parse(Str, Draw, Cards) :-
    split_doublenl(Str, [[DrawStr]|CardStrs]),
    draw_order(DrawStr, Draw),
    maplist(card, CardStrs, Cards).

%%%%%%%%%%%%%%%%%%%%%%%%%
% --- Marking cards --- %
%%%%%%%%%%%%%%%%%%%%%%%%%

% e.g. mark_row([n(1), n(2), n(3)], 2, [n(1), y(2), n(3)])
mark_row([], _, []) :- !.
mark_row([n(D)|UnmarkedRem], D, [y(D)|MarkedRem]) :-
    !, mark_row(UnmarkedRem, D, MarkedRem).
mark_row([n(X)|UnmarkedRem], D, [n(X)|MarkedRem]) :- X =\= D,
    !, mark_row(UnmarkedRem, D, MarkedRem).
mark_row([y(X)|UnmarkedRem], D, [y(X)|MarkedRem]) :-
    !, mark_row(UnmarkedRem, D, MarkedRem).

% Applies mark_row to a whole card
mark_card(card([]), _, card([])) :- !.
mark_card(card([Row|Rem]), Draw, card([MarkedRow|MarkedRem])) :-
    mark_row(Row, Draw, MarkedRow),
    mark_card(card(Rem), Draw, card(MarkedRem)).

% mark_cards(+Cards, +Draw, -MarkedCards)
% Marks all instances of Draw in Cards, giving MarkedCards.
mark_cards([], _, []).
mark_cards([Card|UnmarkedRem], Draw, [MarkedCard|MarkedRem]) :-
    mark_card(Card, Draw, MarkedCard),
    mark_cards(UnmarkedRem, Draw, MarkedRem).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% --- Determining if a card has won --- %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_marked(y(_)).
head([H|_], H).
tail([_|T], T).

% True iff card has a row of y(_) terms.
row_winner(card([R|Rem])) :- (maplist(is_marked, R), !); row_winner(card(Rem)).
% True iff card has a column of y(_) terms.
col_winner(card(L)) :-
    (   % Either the current leftmost col is all marked...
        maplist(head, L, Heads),
        maplist(is_marked, Heads), !
    );
    (   % Or the remainders of each row have the same property.
        maplist(tail, L, Tails),
        col_winner(card(Tails))
    ).
% True iff card has a row or a column of y(_) terms.
winner(Card) :- (row_winner(Card), !); col_winner(Card).

% has_winner([Card1, Card2, Card3, ...], CardN) when CardN is first winner, else false.
has_winner([Card|_], Card) :- winner(Card), !.
has_winner([_|T], Winner) :- has_winner(T, Winner).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% --- Running the whole game --- %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Find the first winning card in this game
find_winner([Draw|DrawRem], Cards, Winner, WinningNumber) :-
    mark_cards(Cards, Draw, MarkedCards),
    (
        has_winner(MarkedCards, WinningCard)
        -> (
            WinningNumber = Draw,
            Winner = WinningCard
        )
        ; find_winner(DrawRem, MarkedCards, Winner, WinningNumber)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% --- Producing the answer --- %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Finds sum of unmarked numbers on card provided
unmarked_sum(card([]), 0).
unmarked_sum(card([[]|Rem]), N) :- unmarked_sum(card(Rem), N).
unmarked_sum(card([[y(_)|T]|Rem]), N) :- unmarked_sum(card([T|Rem]), N).
unmarked_sum(card([[n(X)|T]|Rem]), N) :-
    unmarked_sum(card([T|Rem]), M),
    N is M + X.

solve1(Draw, Cards, Solution) :-
    find_winner(Draw, Cards, WinningCard, LastNumberDrawn),
    unmarked_sum(WinningCard, SumOfUnmarked),
    Solution is SumOfUnmarked * LastNumberDrawn.

%%%%%%%%%%%%%%%%%%
% --- Part 2 --- %
%%%%%%%%%%%%%%%%%%
% Here, we want to find the _last_ board that wins.

find_last_winner([Draw|DrawRem], Cards, LastWinner, LastNumber) :-
    mark_cards(Cards, Draw, MarkedCards),
    filter_winners(MarkedCards, NonWinners, Winners),
    length(NonWinners, CardsLeft),
    (
        CardsLeft == 0
    ->  ([LastWinner] = Winners, LastNumber = Draw)
    ;   find_last_winner(DrawRem, NonWinners, LastWinner, LastNumber)
    ).
filter_winners([], [], []).
filter_winners([Card|CardsRem], [Card|NonWinnersRem], Winners) :-
    \+ winner(Card),
    filter_winners(CardsRem, NonWinnersRem, Winners).
filter_winners([Card|CardsRem], NonWinners, [Card|WinnersRem]) :-
    winner(Card),
    filter_winners(CardsRem, NonWinners, WinnersRem).

solve2(Draw, Cards, Solution) :-
    find_last_winner(Draw, Cards, LastWinner, LastNumber),
    unmarked_sum(LastWinner, SumOfUnmarked),
    Solution is SumOfUnmarked * LastNumber.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%
% --- Printing output --- %
%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_num(n(X), X) :- !.
get_num(y(X), X) :- !.

% card_string(card([]), Acc, Acc) :- !.
% card_string(card([[]|T]), Acc, Res) :- !,
%     string_concat(Acc, "\n", NewAcc),
%     card_string(card(T), NewAcc, Res).
% card_string(card([[H|T1]|T2]), Acc, Res) :-
%     get_num(H, HNum),
%     number_string(HNum, HStr),
%     string_concat(Acc, HStr, Acc2),
%     string_concat(Acc2, "\t", NewAcc),
%     card_string(card([T1|T2]), NewAcc, Res).
% card_string(Card, Str) :- card_string(Card, "", Str).
% write_card(Card) :- card_string(Card, Str), write(Str).
write_card(card([])).
write_card(card([Row|Rem])) :-
    row_strings(Row, Strs),
    format("~s~6+~s~6+~s~6+~s~6+~s~n", Strs),
    write_card(card(Rem)).
row_strings([], []).
row_strings([n(X)|EleRem], [Str|StrRem]) :-
    number_string(X, Str),
    row_strings(EleRem, StrRem).
row_strings([y(X)|EleRem], [Str|StrRem]) :-
    number_string(X, XStr),
    string_concat("[", XStr, IntStr),
    string_concat(IntStr, "]", Str),
    row_strings(EleRem, StrRem).

% File handling
file_slurp(Path, Content) :-
    open(Path, read, FileStream),
    read_string(FileStream, "", "", _, Content),
    close(FileStream).