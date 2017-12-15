:-module(acts, [actcheck/4, bet/4, fold/4]).


%actcheck(+Player, +Table, -Newtable, -Last_to_act)
%calculates the new table and who should be the last to act when a player checks.
actcheck(p1, [First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, Raises], Newtable, Last_to_act) :-
  Stack1 > To_call,
  ( To_call == 0 -> Newtable = [First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, Raises]
  ; NewP1Stack is Stack1 - To_call, NewPot is Pot + To_call*2, Newtable = [First, P1, P2, NewP1Stack, Stack2, Cards, NewPot, Bigblind, 0, 0]
  ),
  format('You call~n', []),
  ( First == p1, Raises == 0 -> Last_to_act = p2
  ; Last_to_act = p1
  ).
  %format('Player 1 calls with ~d ~n', [To_call]), !.

actcheck(p2, [First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, Raises], Newtable, Last_to_act) :-
  Stack2 > To_call,
  ( To_call == 0 -> Newtable = [First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, Raises]
  ; NewP2Stack is Stack2 - To_call, NewPot is Pot + To_call*2, Newtable = [First, P1, P2, Stack1, NewP2Stack, Cards, NewPot, Bigblind, 0, 0]
  ),
  format('player 2 calls~n', []),
  ( First == p2, Raises == 0 -> Last_to_act = p1
  ; Last_to_act = p2
  ).
  %format('Player 2 calls ~d ~n', [To_call]), !.

actcheck(p1, [First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, Raises], Newtable, Last_to_act) :-
  Return is To_call - Stack1,
  NewP2Stack is Stack2 + Return,
  NewPot is Stack1*2 + Pot,
  Newtable = [First, P1, P2, 0, NewP2Stack, Cards, NewPot, Bigblind, 0, 0],
  Last_to_act = p1,
  format('You call with allin ~d ~n', [Stack1]), !.

actcheck(p2, [First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, Raises], Newtable, Last_to_act) :-
  Return is To_call - Stack2,
  NewP1Stack is Stack1 + Return,
  NewPot is Stack2*2 + Pot,
  Newtable = [First, P1, P2, NewP1Stack, 0, Cards, NewPot, Bigblind, 0, 0],
  Last_to_act = p2,
  format('Player 2 call with allin ~d ~n', [Stack2]), !.

%bet(+Player, +Table, -Newtable, -Last_to_act)
%we limit the raises to 2 per person so if someone tries to reraise a 5th time they automatically calls instead
bet(Player, [First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, 4], Newtable, Last_to_act) :-
  %format('raise-limit reached~n', []),
  actcheck(Player, [First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, 5], Newtable, Last_to_act), !.

%calculates the new table and tells the event_handler that the other person is the last to act
bet(p1, [First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, Raises], Newtable, Last_to_act) :-
  Sum is Bigblind + To_call,
  Stack1 > Sum,
  NewP1Stack is Stack1 - To_call - Bigblind,
  NewPot is Pot + To_call*2,
  NewToCall is Bigblind,
  TotalRaises is Raises + 1,
  Newtable = [First, P1, P2, NewP1Stack, Stack2, Cards, NewPot, Bigblind, NewToCall, TotalRaises],
  Last_to_act = p2,
  format('You bet ~d ~n', [Bigblind]), !.

bet(p2, [First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, Raises], Newtable, Last_to_act) :-
  Sum is Bigblind + To_call,
  Stack2 > Sum,
  NewP2Stack is Stack2 - To_call - Bigblind,
  NewPot is Pot + To_call*2,
  NewToCall is Bigblind,
  TotalRaises is Raises + 1,
  Newtable = [First, P1, P2, Stack1, NewP2Stack, Cards, NewPot, Bigblind, NewToCall, TotalRaises],
  Last_to_act = p1,
  format('Player 2 Bets ~d~n', [Bigblind]), !.

bet(p1, [First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, _], Newtable, Last_to_act) :-
  Return is To_call - Stack1,
  NewP2Stack is Stack2 + Return,
  NewPot is Stack1*2 + Pot,
(To_call == 0 -> NewToCall is Stack1, Last_to_act = p2
; NewToCall is 0, Last_to_act = p1),
  Newtable = [First, P1, P2, 0, NewP2Stack, Cards, NewPot, Bigblind, NewToCall, 4],
  format('You bet ~d and went allin~n', [Stack1]), !.

bet(p2, [First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, _], Newtable, Last_to_act) :-
  Return is To_call - Stack2,
  NewP1Stack is Stack1 + Return,
  NewPot is Stack2*2 + Pot,
  (To_call == 0 -> NewToCall is Stack2, Last_to_act = p1
  ; NewToCall is 0, Last_to_act = p2),
  Newtable = [First, P1, P2, NewP1Stack, 0, Cards, NewPot, Bigblind, NewToCall, 4],
  format('Player 2 bets ~d and went allin~n', [Stack2]), !.


fold(p1, Table, Table, p1fold).
fold(p2, Table, Table, p2fold).
