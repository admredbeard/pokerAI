
:-module(game,[start/1]).
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
  handSort(P1, P1Sorted),
  handSort(P2, P2Sorted),  %printtofile(P1,P2,Flop,Turn,River),
  open('result.txt', read, Read),
  readfromfile(Hand, Read, Line),
  close(Read),
  (Winner == p1 -> printtofile(P1Sorted, won, Line), printtofile(P2Sorted, lost, Line)
  ;Winner == p2 -> printtofile(P1Sorted, lost, Line), printtofile(P2Sorted, won, Line)
  ;Winner == tie -> !
  ),
  Y is Loop - 1,
  go(Shuffled, Y).

%when we get nil from readfromfile we know the hand does not exist, so append it
printtofile(Cards, Won, nil) :-
  open('result.txt', append, Append),
  write(Append,[Cards, 0, 0]),
  write('.'),
  nl(Append),
  close(Append),
  !.
%This is where we should change the Won and Totalplayed ints
printtofile(Cards, Won, [Pos, Won, Played]) :-
  print('It exists :O').

%End of stream, hand does not exist yet
readfromfile(Hand,Stream,nil) :-
  at_end_of_stream(Stream),
  !.
%Found it, return position and integers
readfromfile(Hand,Stream,[Pos, Won, Played]) :-
  read(Stream, [Hand, Won, Played]),
  line_count(Stream, Pos),
  !.
%Not this line, keep on looking!
readfromfile(Hand,Stream,[Pos, Won, Played]) :-
  readfromfile(Hand,Stream,[Pos, Won, Played]).
