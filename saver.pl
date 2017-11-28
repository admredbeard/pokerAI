
:-module(saver, [whattowrite/7, whattowrite/8, straightchance/4, flushchance/4, ourCards/3]).
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

whattowrite(3, [card(S,V),card(S2,V2)], [card(S3,V3),card(S4,V4)], P1seven, P2seven, Write1, Write2, Winner) :-
  whoWon(P1seven, P2seven, Winner, Included, Included2, Got, Got2),
  sortByNumber(P1seven, Sorted),
  doubleRemove(Sorted, Doubleremoved),
  flushchance(P1seven, F_cards1, F_Chance, Needed_F),
  flushchance(P2seven, F_cards2, F_Chance2, Needed_F2),
  straightchance(Doubleremoved, S_cards1, S_Chance, Needed_S),
  straightchance(Doubleremoved, S_cards2, S_Chance2, Needed_S2),
  compare(X, Got),
  compare(Y, Got2),
  (X == numbers -> Compare1 = [V, V2]
  ; Compare1 = [card(S,V),card(S2,V2)]),
  (Y == numbers -> Compare2 = [V3, V4]
  ; Compare2 = [card(S3,V3), card(S4,V4)]),
  ourCards(Compare1, Included, Num1),
  ourCards(Compare2, Included2, Num2),
  ourCards([card(S,V),card(S2,V2)], F_cards1, Num3),
  ourCards([card(S3,V3), card(S4,V4)], F_cards2, Num4),
  ourCards([V,V2], S_cards1, Num5),
  ourCards([V3,V4], S_cards2, Num6),
  Write1 = [3, Got, Num1, F_Chance, Needed_F, Num3, S_Chance, Needed_S, Num5],
  Write2 = [3, Got2, Num2, F_Chance2, Needed_F2, Num4, S_Chance2, Needed_S2, Num6], !.

whattowrite(Turn, [card(S,V),card(S2,V2)], [card(S3,V3),card(S4,V4)], P1seven, P2seven, Write1, Write2) :-
  check(P1seven, _, Got, Included),
  check(P2seven, _, Got2, Included2),
  sortByNumber(P1seven, Sorted),
  doubleRemove(Sorted, Doubleremoved),
  flushchance(P1seven, F_cards1, F_Chance, Needed_F),
  flushchance(P2seven, F_cards2, F_Chance2, Needed_F2),
  straightchance(Doubleremoved, S_cards1, S_Chance, Needed_S),
  straightchance(Doubleremoved, S_cards2, S_Chance2, Needed_S2),
  compare(X, Got),
  compare(Y, Got2),
  (X == numbers -> Compare1 = [V, V2]
  ; Compare1 = [card(S,V),card(S2,V2)]),
  (Y == numbers -> Compare2 = [V3, V4]
  ; Compare2 = [card(S3,V3), card(S4,V4)]),
  ourCards(Compare1, Included, Num1),
  ourCards(Compare2, Included2, Num2),
  ourCards([card(S,V),card(S2,V2)], F_cards1, Num3),
  ourCards([card(S3,V3), card(S4,V4)], F_cards2, Num4),
  ourCards([V,V2], S_cards1, Num5),
  ourCards([V3,V4], S_cards2, Num6),
  Write1 = [Turn, Got, Num1, F_Chance, Needed_F, Num3, S_Chance, Needed_S, Num5],
  Write2 = [Turn, Got2, Num2, F_Chance2, Needed_F2, Num4, S_Chance2, Needed_S2, Num6], !.


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
