:-module(proBotsOnline, [ai/4, player/4, preflop/2, flop/3, turn/3, river/3]).

:-use_module(readwrite).
:-use_module(pokerrules).
:-use_module(saver).
:-use_module(acts).

%Player(+turn, -Last_to_act, +Table, -Newtable)
%this is when there is a person playing vs the bot
player(Turn, Last_to_act, [First, P1, P2, P1Stack, P2Stack, Cards, Pot, Big, To_call, Raises], Newtable):-
  format('what do you want to do? check, bet or fold?~n>>', []),
  read(X),
  (   X == check -> actcheck(p1, [First, P1, P2, P1Stack, P2Stack, Cards, Pot, Big, To_call, Raises], Newtable, Last_to_act)
    ; X == bet -> bet(p1, [First, P1, P2, P1Stack, P2Stack, Cards, Pot, Big, To_call, Raises], Newtable, Last_to_act)
    ; X == fold ->  fold(p1, [First, P1, P2, P1Stack, P2Stack, Cards, Pot, Big, To_call, Raises], Newtable, Last_to_act)
    ; write('not a valid action'), player(Turn, Last_to_act, [First, P1, P2, P1Stack, P2Stack, Cards, Pot, Big, To_call, Raises], Newtable)
    ).

%ai(+Turn, -Last_to_act, +Table, -Newtable)
%TODO making the ai work as we want
ai(Turn, Last_to_act, [Firstact|Table], Newtable):-
  whattodo(Turn, Act, [Firstact|Table], Newtable),
  (Firstact == p2 ; Act == raise),
  Last_to_act = p1.
ai(Turn, Last_to_act, Table, Table):-
  Last_to_act = p2.

whattodo(_, raise, [First, P1, P2, P1Stack, P2Stack, Cards, Pot, Big, To_call, Raises],[First, P1, P2, P1Stack, P2Stack, Cards, Pot, Big, To_call, NewRaises]) :-
  NewRaises is Raises + 1.

%preflop(+Cards, -Winrate)
%these predicates work in the same way, it calcylates which name and what values to look for and finds them in the file and returns a winratio determined by the facts in the db
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
  whattowrite(1, [A,B], [A,B,C,D,E], _X, _Y, This),
  open(P1Name, read, Stream),
  read(Stream, _),
  find(Stream, This, Winrate),
  close(Stream).

turn([A,B], [C,D,E,F], Winrate) :-
  handSort([A,B], P1Sorted),
  file_name(P1Sorted, P1Name),
  whattowrite(2, [A,B], [A,B,C,D,E,F], _X, _Y, This),
  open(P1Name, read, Stream),
  read(Stream, _),
  find(Stream, This, Winrate),
  close(Stream).

river([A,B], [C,D,E,F,G], Winrate) :-
  handSort([A,B], P1Sorted),
  file_name(P1Sorted, P1Name),
  whattowrite(3, [A,B], [A,B,C,D,E,F,G], _X, _Y, This),
  open(P1Name, read, Stream),
  read(Stream, _),
  find(Stream, This, Winrate),
  close(Stream).

%find(+Stream, +Hand, -Winrate)
%finds the correct values in the file and returns the winrate.
find(Stream, Hand, Winrate) :-
  read(Stream, [Hands, W, L]),
  ( Hand == Hands -> Winrate is W/(W+L)
    ; find(Stream, Hand, Winrate)
    ).
