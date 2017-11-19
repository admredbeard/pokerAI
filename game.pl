
:-module(game,[start/1, testf/0, teste/0]).
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
  (Winner == p1 -> printtofile(P1Sorted, won), printtofile(P2Sorted, lost)
  ;Winner == p2 -> printtofile(P1Sorted, lost), printtofile(P2Sorted, won)
  ;Winner == tie -> !
  ),
  Y is Loop - 1,
  go(Shuffled, Y).

%Vi testar ifall det går att lägga till ett kort som finns
testf :-
printtofile([card(d,14),card(s,3)], won).
%Vi testar ifall det går att lägga till ett kort som inte finns
teste :-
  printtofile([card(d,6),card(s,5)], won).

%when we get nil from readfromfile we know the hand does not exist, so append it
printtofile(Cards, DidWeWin) :-
  open('result.txt', read, Read),
  readfromfile(Cards, Read, [X, _, _]),
  close(Read),
  X == nil,
  open('result.txt', append, Append),
  write(Append,[Cards, 0, 0]),
  print(Append, '.'),
  nl(Append),
  close(Append),
  !.
%This is where we should change the Won and Totalplayed ints
printtofile(Cards, DidWeWin) :-
  open('result.txt', read, Read),
  readfromfile(Cards, Read, [Pos, Won, Played]),
  NewWon is Won +1,
  NewPlayed is Played +1,
  close(Read),
  %rFunkAR ICKE
  open('result.txt', append, Append),
  seek(Append, Pos, bof, NewPos),
  %set_stream_position(Append, Pos),
  write(Append,[Cards, NewWon, NewPlayed]),
  print(Append, '.'),
  nl(Append),
  close(Append),
  !.
%Found it, return position and integers
readfromfile(Hand,Stream,[Pos, Won, Played]) :-
  \+at_end_of_stream(Stream),
  read(Stream, Term),
  Term = [Hand,Won,Played],
  line_count(Stream, Pos),
  !.
%Not this line, keep on looking!
readfromfile(Hand,Stream,[Pos, Won, Played]) :-
  \+at_end_of_stream(Stream),
  readfromfile(Hand,Stream,[Pos, Won, Played]),
  !.

%End of stream, hand does not exist yet
readfromfile(Hand,Stream,[nil, Won, Played]) :-
  at_end_of_stream(Stream),
  !.
