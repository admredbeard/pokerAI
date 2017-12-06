:-module(acts, [actcheck/4, bet/4, fold/4]).


%actcheck(+Player, +Table, -Newtable, -Last_to_act)
%calculates the new table and who should be the last to act when a player checks.
actcheck(p1, [First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, Raises], Newtable, Last_to_act) :-
  ( To_call == 0 -> Newtable = [First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, Raises]
  ; NewP1Stack is Stack1 - To_call, NewPot is Pot + To_call*2, Newtable = [First, P1, P2, NewP1Stack, Stack2, Cards, NewPot, Bigblind, 0, 0]
  ),
  ( First == p1, Raises == 0 -> Last_to_act = p2
  ; Last_to_act = p1
  ),
  format('Player 1 calls~n', []).

actcheck(p2, [First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, Raises], Newtable, Last_to_act) :-
  write(To_call),
  ( To_call == 0 -> Newtable = [First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, Raises]
  ; NewP2Stack is Stack2 - To_call, NewPot is Pot + To_call*2, Newtable = [First, P1, P2, Stack1, NewP2Stack, Cards, NewPot, Bigblind, 0, 0]
  ),
  ( First == p2, Raises == 0 -> Last_to_act = p1
  ; Last_to_act = p2
  ),
  format('Player 2 calls~n', []).

%bet(+Player, +Table, -Newtable, -Last_to_act)
%we limit the raises to 2 per person so if someone tries to reraise a 5th time they automatically calls instead
bet(Player, [First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, 5], Newtable, Last_to_act) :-
  format('raise-limit reached', []),
  actcheck(Player, [First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, 5], Newtable, Last_to_act), !.

%calculates the new table and tells the event_handler that the other person is the last to act
bet(p1, [First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, Raises], Newtable, Last_to_act) :-
  NewP1Stack is Stack1 - To_call - Bigblind,
  NewPot is Pot + To_call*2,
  NewToCall is Bigblind,
  TotalRaises is Raises + 1,
  Newtable = [First, P1, P2, NewP1Stack, Stack2, Cards, NewPot, Bigblind, NewToCall, TotalRaises],
  Last_to_act = p2,
  format('Player 1 Bets~n', []).

bet(p2, [First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, Raises], Newtable, Last_to_act) :-
  NewP2Stack is Stack2 - To_call - Bigblind,
  NewPot is Pot + To_call*2,
  NewToCall is Bigblind,
  TotalRaises is Raises + 1,
  write(TotalRaises),
  Newtable = [First, P1, P2, Stack1, NewP2Stack, Cards, NewPot, Bigblind, NewToCall, TotalRaises],
  Last_to_act = p1,
  format('Player 2 Bets~n', []).

fold(p1, Table, Table, p1fold).

fold(p2, Table, Table, p2fold).
