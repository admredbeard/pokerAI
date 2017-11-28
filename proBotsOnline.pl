:-module(proBotsOnline, [preflop/2, flop/3, turn/3, river/3]).

:-use_module(readwrite).
:-use_module(pokerrules).
:-use_module(saver).

preflop(Cards, Winrate) :-
  handSort(Cards, P1Sorted),
  file_name(P1Sorted, P1Name),
  open(P1Name, read, S),
  read(S, total_WL(X, Y)),
  Winrate is X / (X+Y),
  close(S).

flop([A,B], [C,D,E], Winrate) :-
  handSort([A,B], P1Sorted),
  file_name(P1Sorted, P1Name),
  hasgot(flop, [A,B], [A,B,C,D,E], This),
  open(P1Name, read, Stream),
  read(Stream, _),
  find(Stream, This, Winrate),
  close(Stream).

turn([A,B], [C,D,E,F], Winrate) :-
  handSort([A,B], P1Sorted),
  file_name(P1Sorted, P1Name),
  hasgot(flop, [A,B], [A,B,C,D,E,F], This),
  open(P1Name, read, Stream),
  read(Stream, _),
  find(Stream, This, Winrate),
  close(Stream).

river([A,B], [C,D,E,F,G], Winrate) :-
  handSort([A,B], P1Sorted),
  file_name(P1Sorted, P1Name),
  hasgot(flop, [A,B], [A,B,C,D,E,F,G], This),
  open(P1Name, read, Stream),
  read(Stream, _),
  find(Stream, This, Winrate),
  close(Stream).

find(Stream, Hand, Winrate) :-
  read(Stream, [Hands, W, L]),
  ( Hand == Hands -> Winrate is W/(W+L)
    ; find(Stream, Hand, Winrate)
    ).

compare(numbers, 2).
compare(numbers, 3).
compare(numbers, 6).
compare(numbers, 7).
compare(numbers, 8).
compare(numbers, 9).
compare(cards, 1).
compare(cards, 4).
compare(numbers, 5).

hasgot(flop, [card(S,V),card(S2,V2)], P1seven, Write1) :-
  check(P1seven, _, Got, Included),
  flushchance(P1seven, F_cards1, F_Chance, Needed_F),
  straightchance(P1seven, S_cards1, S_Chance, Needed_S),
  compare(X, Got),
  (X == numbers -> Compare1 = [V, V2]
  ; Compare1 = [card(S,V),card(S2,V2)]),
  ourCards(Compare1, Included, Num1),
  ourCards([card(S,V),card(S2,V2)], F_cards1, Num3),
  ourCards([card(S,V),card(S2,V2)], S_cards1, Num5),
  Write1 = [1, Got, Num1, F_Chance, Needed_F, Num3, S_Chance, Needed_S, Num5].

hasgot(turn, [card(S,V),card(S2,V2)], P1seven, Write1) :-
  check(P1seven, _, Got, Included),
  flushchance(P1seven, F_cards1, F_Chance, Needed_F),
  straightchance(P1seven, S_cards1, S_Chance, Needed_S),
  compare(X, Got),
  (X == numbers -> Compare1 = [V, V2]
  ; Compare1 = [card(S,V),card(S2,V2)]),
  ourCards(Compare1, Included, Num1),
  ourCards([card(S,V),card(S2,V2)], F_cards1, Num3),
  ourCards([card(S,V),card(S2,V2)], S_cards1, Num5),
  Write1 = [1, Got, Num1, F_Chance, Needed_F, Num3, S_Chance, Needed_S, Num5].

hasgot(river, [card(S,V),card(S2,V2)], P1seven, Write1) :-
  check(P1seven, _, Got, Included),
  compare(X, Got),
  (X == numbers -> Compare1 = [V, V2]
  ; Compare1 = [card(S,V),card(S2,V2)]),
  ourCards(Compare1, Included, Num1),
  Write1 = [3, Got, Num1].
