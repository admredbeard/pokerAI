
:-module(game,[start/1]).
:-use_module(dealer).
:-use_module(pokerrules).
:-use_module(readwrite).
:-use_module(saver).

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
  playersixCards(P1, Flop, Turn, P1six),
  playersixCards(P2, Flop, Turn, P2six),
  playerfiveCards(P1, Flop, P1five),
  playerfiveCards(P2, Flop, P2five),
  handSort(P1, P1Sorted),
  handSort(P2, P2Sorted),                                   %printtofile(P1,P2,Flop,Turn,River),
  whattowrite(3, P1, P2, P1seven, P2seven, Writeriver1, Writeriver2, Winner),
  whattowrite(2, P1, P2, P1six, P2six, Writeturn1, Writeturn2),
  whattowrite(1, P1, P2, P1five, P2five, Writeflop1, Writeflop2),
  HandP1 = [Writeflop1, Writeturn1, Writeriver1],
  HandP2 = [Writeflop2, Writeturn2, Writeriver2],
  file_name(P1Sorted, P1Name),                              %find out what the name of the file is
  file_name(P2Sorted, P2Name),
  (Winner == p1 -> add(HandP1, win, P1Name), add(HandP2, loss, P2Name)  %only for preflop atm "preflop" can be replaced by anything
  ;Winner == p2 -> add(HandP1, loss, P1Name), add(HandP2, win, P2Name)
  ;Winner == tie -> !
  ),
  Y is Loop - 1,
  go(Shuffled, Y).
