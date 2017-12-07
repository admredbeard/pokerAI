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
  (
      X == end_of_file -> nl, !
    ; X == check  -> actcheck(p1, [First, P1, P2, P1Stack, P2Stack, Cards, Pot, Big, To_call, Raises], Newtable, Last_to_act)
    ; X == bet    -> bet(p1, [First, P1, P2, P1Stack, P2Stack, Cards, Pot, Big, To_call, Raises], Newtable, Last_to_act)
    ; X == fold   -> fold(p1, [First, P1, P2, P1Stack, P2Stack, Cards, Pot, Big, To_call, Raises], Newtable, Last_to_act)
    ; format('Not a valid action~n', []), player(Turn, Last_to_act, [First, P1, P2, P1Stack, P2Stack, Cards, Pot, Big, To_call, Raises], Newtable)
  ).

%ai(+Turn, -Last_to_act, +Table, -Newtable)
%TODO making the ai work as we want
ai(Turn, Last_to_act, [First, P1, P2, P1Stack, P2Stack, Cards, Pot, Big, To_call, Raises], Newtable):-
  Next is Turn - 1,
  (   Next == 0 -> preflop(P1, Winrate)
    ; flop(Next, P1, Cards, Winrate)
  ),
  (   Winrate > 0.4 -> bet(p1, [First, P1, P2, P1Stack, P2Stack, Cards, Pot, Big, To_call, Raises], Newtable, Last_to_act)
    ; Winrate > 0.30 -> actcheck(p1, [First, P1, P2, P1Stack, P2Stack, Cards, Pot, Big, To_call, Raises], Newtable, Last_to_act)
    ; To_call == 0 ->  actcheck(p1, [First, P1, P2, P1Stack, P2Stack, Cards, Pot, Big, To_call, Raises], Newtable, Last_to_act)
    ; Winrate < 0.3 ->  fold(p1, [First, P1, P2, P1Stack, P2Stack, Cards, Pot, Big, To_call, Raises], Newtable, Last_to_act)
  ).

whattodo(_, raise, [First, P1, P2, P1Stack, P2Stack, Cards, Pot, Big, To_call, Raises],[First, P1, P2, P1Stack, P2Stack, Cards, Pot, Big, To_call, NewRaises]) :-
  NewRaises is Raises + 1.

%preflop(+Cards, -Winrate)
%these predicates work in the same way, it calcylates which name and what values to look for and finds them in the file and returns a winratio determined by the facts in the db
preflop(Cards, Winrate) :-
  handSort(Cards, Hand),
  readwrite:file_name(Hand, P1Name),
  open(P1Name, read, S),
  read(S, total_WL(X, Y)),
  Winrate is X / (X+Y),
  close(S).

flop(Turn, Hand, Cards, Winrate) :-
  append(Hand, Cards, Total),
  handSort(Hand, P1Sorted),
  readwrite:file_name(P1Sorted, P1Name),
  whattowrite(Turn, Total, Total, _X, _Y, This),
  open(P1Name, read, Stream),
  read(Stream, _),
  find(Stream, This, Winrate),
  close(Stream).

%find(+Stream, +Hand, -Winrate)
%finds the correct values in the file and returns the winrate.
find(Stream, Hand, Winrate) :-
  \+at_end_of_stream(Stream),
  read(Stream, [Hands, W, L]),
  ( Hand == Hands -> Winrate is W/(W+L)
    ; find(Stream, Hand, Winrate)
    ).
find(Stream, _, 1) :-
  at_end_of_stream(Stream).
