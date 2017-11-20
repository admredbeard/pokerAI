
:-module(game,[start/1, testf/0, teste/0]).
:-use_module(dealer).
:-use_module(pokerrules).
:-use_module(readwrite).

start(X) :-
  createDeck(Deck),
  statistics(walltime, [Start, _]),
  go(Deck,X),
  statistics(walltime, [End, _]),
  Time is End - Start,
  write('Done with '),
  write(X),
  write(' hands in '),
  write(Time),
  write(' ms.').

go(_, 0) :-
  !.
go(Deck, Loop) :-
  Loop\==0,
  shuffleDeck(Deck, Shuffled),
  dealhands(P1, P2, Shuffled, R),
  dealflop(Flop, R, Re),
  dealturn(Turn, Re, Res),
  dealriver(River, Res),
  playersevenCards(P1, Flop, Turn, River, P1seven),
  playersevenCards(P2, Flop, Turn, River, P2seven),
  whoWon(P1seven,P2seven, Winner),
  handSort(P1, P1Sorted),
  handSort(P2, P2Sorted),  %printtofile(P1,P2,Flop,Turn,River),
  file_name(P1Sorted, P1Name),      %find out what the name of the file is
  file_name(P2Sorted, P2Name),
  (Winner == p1 -> add(preflop, win, P1Name), add(preflop, loss, P2Name) %only for preflop atm "preflop" can be replaced by anything
  ;Winner == p2 -> add(preflop, loss, P1Name), add(preflop, win, P2Name)
  ;Winner == tie -> !
  ),
  Y is Loop - 1,
  go(Shuffled, Y).
