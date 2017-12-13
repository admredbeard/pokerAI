:-module(game,[start/2]).
:-use_module(dealer).
:-use_module(pokerrules).
:-use_module(readwrite).
:-use_module(saver).
:-use_module(noobBotOffline).
:-use_module(proBotsOnline).
:-use_module(library(random)).
:-use_module(library(system)).


start(X, Startingtable) :-
  createDeck(Deck),
  now(Seed),
  setrand(Seed),
  statistics(walltime, [Start, _]),
  go(Deck,X, Startingtable),
  statistics(walltime, [End, _]),
  Time is End - Start,
  write('Done with '),
  write(X),
  write(' hands in '),
  write(Time),
  write(' ms.').

go(_, 0, [P1Stack, P2Stack, Bigblind, First, Second]) :-
  format('Player1 has: ~d chips left, Player2 has ~d chips left ~nBigblind is: ~d, and ~w starts to act. ~nDo you want to continue? Write number of hands~n >>', [P1Stack, P2Stack, Bigblind, First]),
  read(X),
  start(X, [P1Stack, P2Stack, Bigblind, First, Second]).

%just a loop for either the game or adding new hands to the db
go(Deck, Loop, Oldtable) :-
  Loop \= 0,
  startgame(Deck, Oldtable, [P1Stack, P2Stack, Bigblind, First, Second], NewDeck),
  %addtodatabase(P1, P2, Flop, Turn, River), this one is used when we want to add more hands to the database
  IncBlind is Loop  mod 100,
  (IncBlind == 0 -> NewBigblind is Bigblind, addtable(P2Stack) %uncomment to record how it changes its threshold
  ; NewBigblind is Bigblind),
  Y is Loop - 1, !,
  ( P1Stack == 0 -> format('Gameover, p2 wins! ~n', []),!
  ; P2Stack == 0 ->  format('Gameover, p1 wins! ~n', []),!
  ; go(NewDeck, Y, [P1Stack, P2Stack, NewBigblind, First, Second])).

%the initialization of the game, shuffling the deck, dealing the cards and setting up the table
startgame(Deck, [P1Stack, P2Stack, Bigblind, First, Second], [NewP1Stack, NewP2Stack, Bigblind, Second, First], Shuffled) :-
  shuffleDeck(Deck, Shuffled),
  dealhands(P1, P2, Shuffled, R),
  dealflop(Flop, R, Re),
  dealturn(Turn, Re, Res),
  dealriver(River, Res),
  Pot is Bigblind,
  To_call is Bigblind / 2,
  ( First == p1 -> Stack1 is P1Stack - To_call, Stack2 is P2Stack - Bigblind
      ; Stack1 is P1Stack - Bigblind, Stack2 is P2Stack - To_call),
  Statics = [Flop, Turn, River, [First, Second]],
  Table = [First,P1, P2, Stack1, Stack2, _Cards, Pot, Bigblind, To_call, 0],
  event_handler(1, Second, First, Table, Statics, [NewP1Stack, NewP2Stack]),
  format('p1 ~d, p2 ~d ~n', [NewP1Stack, NewP2Stack]).

  %if turn is 5 (after the acts after river) we decide a winner
event_handler(5, _, _, Table, Statics, [NewP1Stack, NewP2Stack]) :-
  decider(Table, Statics, NewP1Stack, NewP2Stack).

event_handler(Turn, Acted, Last_to_act, [First, P1, P2, P1Stack, P2Stack, Cards, Pot, Big, To_call, Raises], Everythingelse, Stacks) :-
  Acted == Last_to_act,  %checks if it's time to deal out cards that is if both have acted or number of raises are above 4
  (P1Stack == 0 ; P2Stack == 0),
  Next is Turn + 1,
  event_handler(Next, Last_to_act, Last_to_act, [First, P1, P2, P1Stack, P2Stack, Cards, Pot, Big, To_call, Raises], Everythingelse, Stacks).

event_handler(Turn, Acted, Last_to_act, [First, P1, P2, P1Stack, P2Stack,
 Cards, Pot, Big, To_call, _], Everythingelse, Stacks) :-
  (P1Stack == 0 ; P2Stack == 0),
  Next is Turn + 1,
  event_handler(Next, Acted, Last_to_act, [First, P1, P2, P1Stack, P2Stack, Cards, Pot, Big, To_call, 4], Everythingelse, Stacks).


%if someone folds the amount in the pot goes to the opponent
event_handler(_, _, p1fold, [_, _, _, P1Stack, P2Stack, _, Pot, _, To_call, _], _, [P1Stack, NewP2Stack]) :-
  Amount is Pot + To_call,
  NewP2Stack is Amount + P2Stack,
  format('Player 1 folds, Player 2 win: ~d$~n', [Amount]), !.

event_handler(_, _, p2fold, [_, P1, P2, P1Stack, P2Stack, _, Pot, _, To_call, _], [Flop,Turn,River|_], [NewP1Stack, P2Stack]) :-
  Amount is Pot + To_call,
  NewP1Stack is Amount + P1Stack,
  format('Player 2 folds, Player 1 win: ~d$~n', [Amount]), !.
%event_handler(+Turn, +Acted, -Last_to_act, +Table, +Everythingelse, -Stacks)
event_handler(Turn, Acted, Last_to_act, [First, P1, P2, P1Stack, P2Stack, Cards, Pot, Big, To_call, Raises], Everythingelse, Stacks) :-
  (Acted == Last_to_act ; Raises > 4),  %checks if it's time to deal out cards that is if both have acted or number of raises are above 4
  deal(Turn, [First, P1, P2, P1Stack, P2Stack, Cards, Pot, Big, To_call, Raises], [Firstact|Newtable], Everythingelse),
  Next is Turn + 1,
  (   Firstact == p1 -> Second = p2
    ; Second = p1),
  event_handler(Next, Second, Firstact, [Firstact|Newtable], Everythingelse, Stacks).

%event_handler for first player (currently not a computer) setting Acted as p1 and deciding Last_to_act and Stacks depending on playeraction
event_handler(Turn, _, p1, Table, Everythingelse, Stacks) :-
  ai(Turn, Last_to_act, Table, Newtable),
  event_handler(Turn, p1, Last_to_act, Newtable, Everythingelse, Stacks).

%event_handler for second player same rules here
event_handler(Turn, _, p2, Table, Everythingelse, Stacks) :-
  ai2(Turn, Last_to_act, Table, Newtable),
  event_handler(Turn, p2, Last_to_act, Newtable, Everythingelse, Stacks).

%deal(+Num, +Oldtable, -Newtable, +Cards), resets the table and decides who should start, and deals flop/turn/river
deal(1, [First,P1, P2, P1Stack, P2Stack, _, Pot, Big, _, _], [NewFirst,P1,P2,P1Stack,P2Stack,Flop,Pot,Big,0,0], [Flop|_]):-
  ( First == p1 -> NewFirst = p2
    ; NewFirst = p1).
  %  format('Flop: ~w, Pot: ~d~n', [Flop, Pot]).
deal(2, [First,P1, P2, P1Stack, P2Stack, Cards, Pot, Big, _, _], [First,P1,P2,P1Stack,P2Stack,[Turn|Cards],Pot,Big,0,0], [_,[Turn]|_]).% :-
  %format('Turn: ~w, Pot: ~d~n', [Turn, Pot]).
deal(3, [First,P1, P2, P1Stack, P2Stack, Cards, Pot, Big, _, _], [First,P1,P2,P1Stack,P2Stack,[River|Cards],Pot,Big,0,0], [_,_,[River]|_]).% :-
  %format('River: ~w, Pot: ~d~n', [River, Pot]).
%deal 4 is for when all hands are dealt and all actions has been taken
deal(4, [First,P1, P2, P1Stack, P2Stack, Cards, Pot, Big, _, _], [First,P1,P2,P1Stack,P2Stack,Cards,Pot,Big,0,0], _).

%decider(+Table, +Static, -NewP1Stack, -NewP2Stack) checks who won and deals the pot to that player
decider([_,P1,P2,P1Stack,P2Stack,_,Pot,Bigblind,_,_], [Flop, Turn, River|_], NewP1Stack, NewP2Stack) :-
  playersevenCards(P1, Flop, Turn, River, P1seven),
  playersevenCards(P2, Flop, Turn, River, P2seven),
  whoWon(P1seven, P2seven, Winner, _, _, _, _),
  Amount is Bigblind*16,
  (   Pot =< Amount, Winner == p2 -> addpercent(smallwin)
    ; Pot >= Amount, Winner == p1 -> addpercent(bigloss)
    ; Winner == p1 -> addpercent(smalloss)
    ; Winner == p2 -> addpercent(smallwin)
    ; !
    ),
  (   Winner == p1 -> NewP1Stack is P1Stack + Pot, NewP2Stack is P2Stack, format('Player one Wins~n', [])
    ; Winner == p2 -> NewP2Stack is P2Stack + Pot, NewP1Stack is P1Stack, format('Player two Wins~n', [])
    ; NewP1Stack is P1Stack + Pot / 2, NewP2Stack is P2Stack + Pot / 2, format('It is a tie~n', [])
    ), !.


%addpercent used to change the value of the ai's threshold big wins now at 67%, smallwins at 55%, the threshold will stay the same if this is upheld.
addpercent(bigwin) :-
  open('bet.txt', read, Stream),
  read(Stream, Percent),
  NewPercent is Percent - 14,
  close(Stream),
  open('bet.txt', write, Stream2),
  write(Stream2, NewPercent), write(Stream2, '.'),
  close(Stream2).

addpercent(smallwin) :-
  open('bet.txt', read, Stream),
  read(Stream, Percent),
  NewPercent is Percent - 10,
  close(Stream),
  open('bet.txt', write, Stream2),
  write(Stream2, NewPercent), write(Stream2, '.'),
  close(Stream2).

addpercent(bigloss) :-
  open('bet.txt', read, Stream),
  read(Stream, Percent),
  NewPercent is Percent + 28,
  close(Stream),
  open('bet.txt', write, Stream2),
  write(Stream2, NewPercent), write(Stream2, '.'),
  close(Stream2).

addpercent(smalloss) :-
  open('bet.txt', read, Stream),
  read(Stream, Percent),
  NewPercent is Percent + 12,
  close(Stream),
  open('bet.txt', write, Stream2),
  write(Stream2, NewPercent), write(Stream2, '.'),
  close(Stream2).

addtable(Stack) :-
  open('bet.txt', read, Stream1),
  read(Stream1, Percent),
  close(Stream1),
  open('table1.txt', append, Stream2),
  format(Stream2, '~d, ~d~n', [Stack, Percent]),
  close(Stream2).
