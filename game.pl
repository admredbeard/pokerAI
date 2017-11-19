
:-module(game,[start/1, createfile/0]).
:-use_module(dealer).
:-use_module(pokerrules).

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
  %printtofile(P1,P2,Flop,Turn,River),
  (Winner == p1 -> printtofile(P1, won), printtofile(P2, lost)
  ;Winner == p2 -> printtofile(P1, lost), printtofile(P2, won)
  ;Winner == tie -> !
  ),
  Y is Loop - 1,
  go(Shuffled, Y).


printtofile(P1, Won) :-
    open('result.txt', append, Stream),
    write(Stream,[P1, Won]),
    nl(Stream),
    close(Stream).

createfile :-
  createDeck(Deck),
  open('result.txt', append, Stream),
  doouter(Deck, Outer),
  doinner
  close(Stream).

doouter()

doinner([],Stream) :-
  nl(Stream),.
doinner([H|T], Stream) :-
  write(Stream, [H, 0, 0]),
  createfile(T, Stream).
