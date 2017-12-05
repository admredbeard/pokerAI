:-module(saver, [whattowrite/6, straightchance/4, flushchance/4, ourCards/3]).
:-use_module(pokerrules).

compare(numbers, 2).
compare(numbers, 3).
compare(numbers, 6).
compare(numbers, 7).
compare(numbers, 8).
compare(numbers, 9).
compare(cards, 1).
compare(cards, 4).
compare(numbers, 5).

whattowrite(Turn, [card(S,V),card(S2,V2)], Cards, Got, FiveBest, Write1) :-
  check(Cards, FiveBest, Got, Included),
  sortByNumber(Cards, Sorted),
  doubleRemove(Sorted, Doubleremoved),
  flushchance(Cards, FlushCards, FlushChance, Needed_F),
  straightchance(Doubleremoved, StraightCards, StraightChance, Needed_S),
  compare(X, Got),
  (X == numbers -> Compare1 = [V, V2]
  ; Compare1 = [card(S,V),card(S2,V2)]),
  ourCards(Compare1, Included, Num1),
  ourCards([card(S,V),card(S2,V2)], FlushCards, Num3),
  ourCards([V,V2], StraightCards, Num5),
  Write1 = [Turn, Got, Num1, FlushChance, Needed_F, Num3, StraightChance, Needed_S, Num5], !.

%Grants a bonus if there are 4 cards which gives a possible straight chance
straightchance(Hand, Cards, X, Y) :-
  straightchance1(Hand, Cards, X, Y), !.

straightchance(Hand, Cards, X, Y) :-
  straightchance2(Hand, Cards, X, Y), !.

straightchance1([card(_,V1),card(_,V2),card(_,V3),card(_,V4)|_], [V1,V2,V3,V4], 1, 1) :-
  X is V1 - V4,
  X == 3, !.
straightchance1([card(_,V1),card(_,V2),card(_,V3),card(_,V4)|_], [V1,V2,V3,V4], 1, 2) :-
  X is V1 - V4,
  X == 4, !.
straightchance1([card(C,14)|R], Y, X, W) :- %Counting Ace as a possible straight with low cards aswell
  append(R, [card(C,1)], L),
  straightchance1(L, Y, X, W).
straightchance1([card(_,_)|R], Y, X, W) :-
  straightchance1(R, Y, X, W).

straightchance2([], [], 0, 0):- !.
straightchance2([card(_,V1),card(_,V2),card(_,V3)|_], [V1,V2,V3], 2, 1) :-
  X is V1 - V3,
  X == 2, !.
straightchance2([card(_,V1),card(_,V2),card(_,V3)|_], [V1,V2,V3], 2, 2) :-
  X is V1 - V3,
  X == 3, !.
straightchance2([card(_,V1),card(_,V2),card(_,V3)|_], [V1,V2,V3], 2, 3) :-
  X is V1 - V3,
  X == 4, !.
straightchance2([card(C,14)|R], Y, X, W) :- %Counting Ace as a possible straight with low cards aswell
  append(R, [card(C,1)], L),
  straightchance2(L, Y, X, W).
straightchance2([card(_,_)|R], Y, X, W) :-
  straightchance2(R, Y, X, W).

flushchance(Hand, Cards, 1, 1) :-
  samecolor(Hand, Cards),
  length(Cards, 4), !.
flushchance(Hand, Cards, 1, 2) :-
  samecolor3(Hand, Cards).
flushchance(_, [], 0, 0).

samecolor3([card(Suit, V1)|Hand], [card(Suit,V1)|Total]) :-
  findall(card(Suit, V), member(card(Suit, V), Hand), Total),
  length(Total, X),
  X == 2, !.
samecolor3([card(_, _)|Hand], T) :-
  samecolor3(Hand, T).

ourCards([X, Y], FiveBest, 2) :-
  memberchk(X, FiveBest),
  memberchk(Y, FiveBest).

ourCards([X, Y], FiveBest, 1) :-
  memberchk(X, FiveBest),
  \+memberchk(Y, FiveBest).

ourCards([X, Y], FiveBest, 1) :-
  memberchk(Y, FiveBest),
  \+memberchk(X, FiveBest).

ourCards([X, Y], FiveBest, 0) :-
  \+memberchk(X, FiveBest),
  \+memberchk(Y, FiveBest).
