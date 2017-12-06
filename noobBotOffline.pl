:-module(noobBotOffline, [bot/4]).
:-use_module(pokerrules).
:-use_module(acts).
:-use_module(library(random)).

%bot(+Num, -Last_to_act, +Table, -Newtable) our "bad bot" decides what to do depending on turn and how the table looks
bot(1, Last_to_act, Table, Newtable):-
  whatToDo_preFlop(Table, Newtable, Last_to_act).
bot(2, Last_to_act, Table, Newtable):-
  whatToDo_Flop(Table, Newtable, Last_to_act).
bot(3, Last_to_act, Table, Newtable):-
  whatToDo_Turn(Table, Newtable, Last_to_act).
bot(4, Last_to_act, Table, Newtable):-
  whatToDo_River(Table, Newtable, Last_to_act).


%playeractions changed to whether or not there is something to call or number of raises
%oldplayerfrombefore
whatToDo_preFlop([First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, Raises], Newtable, Last_to_act):-
  value_preflop(P2, NewValue, First), % The hand is evaluated here
  ( NewValue < 0, To_call > 0 -> fold(p2, [First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, Raises], Newtable, Last_to_act)
  ; NewValue < 50, To_call == 0 -> actcheck(p2, [First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, Raises], Newtable, Last_to_act)
  ; NewValue < 100, To_call > 0 -> actcheck(p2, [First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, Raises], Newtable, Last_to_act)
  ; Raises > 2 -> actcheck(p2, [First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, Raises], Newtable, Last_to_act)
  ; To_call > 0 -> bet(p2, [First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, Raises], Newtable, Last_to_act)
  ; To_call == 0 -> bet(p2, [First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, Raises], Newtable, Last_to_act)
  ).

whatToDo_Flop([First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, Raises], Newtable, Last_to_act) :-
  append(P2, Cards, Hand),
  random(-5,10,Humanfactor),
  chance(Hand, R),
  check(Hand, X, V, _),
  pairevaluator(X, Res, P2),
  Result is R + Res + Humanfactor,
  (   V < 8 -> bet(p2, [First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, Raises], Newtable, Last_to_act)
    ; Raises > 2, Result > 40 -> actcheck(p2, [First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, Raises], Newtable, Last_to_act)
    ; Result > 40 -> bet(p2, [First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, Raises], Newtable, Last_to_act)
    ; To_call == 0, Result > 20 -> bet(p2, [First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, Raises], Newtable, Last_to_act)
    ; To_call > 0, Result > 20 -> actcheck(p2, [First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, Raises], Newtable, Last_to_act)
    ; Result > 0 -> actcheck(p2, [First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, Raises], Newtable, Last_to_act)
    ; To_call == 0 -> actcheck(p2, [First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, Raises], Newtable, Last_to_act)
    ; fold(p2, [First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, Raises], Newtable, Last_to_act)
    ).

whatToDo_Turn([First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, Raises], Newtable, Last_to_act) :-
  append(P2, Cards, Hand),
  random(-5,10,Humanfactor),
  check(Hand, X, V, _),
  pairevaluator(X, Res, P2),
  Result is Res + Humanfactor,
  (   V < 7 -> bet(p2, [First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, Raises], Newtable, Last_to_act)
    ; Raises > 2, Result > 115 -> actcheck(p2, [First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, Raises], Newtable, Last_to_act)
    ; Result > 115 -> bet(p2, [First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, Raises], Newtable, Last_to_act)
    ; To_call == 0, Result > 50 -> bet(p2, [First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, Raises], Newtable, Last_to_act)
    ; To_call > 0, Result > 50 -> actcheck(p2, [First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, Raises], Newtable, Last_to_act)
    ; Result > 10 -> actcheck(p2, [First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, Raises], Newtable, Last_to_act)
    ; To_call == 0 -> actcheck(p2, [First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, Raises], Newtable, Last_to_act)
    ; fold(p2, [First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, Raises], Newtable, Last_to_act)
    ).
%  .

whatToDo_River([First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, Raises], Newtable, Last_to_act) :-
    append(P2, Cards, OurHand),
    random(0,25,Humanfactor),
    SharedHand = Cards,
    check(SharedHand, FiveBest1, HV1, _),  % We check if our best hand of a total 5 cards is only by using the 5 shared
    check(OurHand, FiveBest2, HV2, _),     % cards or in combination with our own hand consisting of 7 cards
    pairevaluator(FiveBest2, R, P2),
    chance(SharedHand, SharedChance),
    Result is R - (SharedChance * 2) + Humanfactor,
    (   HV1 \== HV2,  HV2 < 6 -> bet(p2, [First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, Raises], Newtable, Last_to_act) %Cases where our hand is stronger than the shared hand
      ; FiveBest1 \== FiveBest2,  HV2 < 6 -> bet(p2, [First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, Raises], Newtable, Last_to_act) %Cases where our hand is the same as the shared, like
      ; Raises > 2 , HV1 < 5 -> actcheck(p2, [First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, Raises], Newtable, Last_to_act) %Cases where the shared hand is a flush or better
      ; HV1 < 5 -> bet(p2, [First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, Raises], Newtable, Last_to_act)
      ; Raises > 2,  HV2 == 5, Humanfactor < 15 ->  actcheck(p2, [First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, Raises], Newtable, Last_to_act) %Cases where we have a shared straight
      ; HV2 == 5, Humanfactor < 15 -> bet(p2, [First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, Raises], Newtable, Last_to_act)  %and we want to have a bluff variable
      ; HV1 == 5 -> actcheck(p2, [First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, Raises], Newtable, Last_to_act) %Also cases where it acts upon a shared straight but humanfactor above 15
      ; Raises > 2,  Result > 115 -> actcheck(p2, [First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, Raises], Newtable, Last_to_act) %The rest of the cases are based upon
      ; Result > 115 -> bet(p2, [First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, Raises], Newtable, Last_to_act)  %if we have three of a kind or lower
      ; To_call == 0, Result > 50 -> bet(p2, [First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, Raises], Newtable, Last_to_act)
      ; actcheck(p2, [First, P1, P2, Stack1, Stack2, Cards, Pot, Bigblind, To_call, Raises], Newtable, Last_to_act)
      ).


%Evaluates three of a kind
pairevaluator([V1,V1,V1,V4,V5], Result, [card(_, A),card(_, B)]):-
  max(A, B, C),
  ( V1 == A, V1 == B -> Result is 200             %Three of a kind with pocketpair
  ; (V1 == A ; V1 == B) -> Result is V1*V1 + 50   %Three of a kind with a card from our hand
  ; (V4 == C ; V5 == C), C > 11 -> Result is C*2  %Shared three of a kind but having a high kicker
  ; Result is 0
  ), !.


%Evaluates two pair
pairevaluator([V1,V1,V3,V3,V5], Result, [card(_, A),card(_, B)]):-
  (   V1 == A, V1 == B -> Result is V1 * V1 * 2                         %Two pair with the high pocketpair
    ; V3 == A, V3 == B -> Result is V3 * V3 + 70                        %Two pair with the low pocketpair
    ; (V1 == A, V3 == B ; V3 == A, V1 == B) -> Result is V1 * V3 * 2.5  %Two pair with both cards making one pair each
    ; (V1 == A ; V1 == B) -> Result is V1 * V1 * 1.5                    %Two pair with the high card connected
    ; (V3 == A ; V3 == B) -> Result is V3 * V3 * 0.5                    %Two pair with the low card connected
    ; (V5 == A ; V5 == B), V5 > 11 -> Result is V5                      %Two pair with shared hand but a high kicker
    ; Result is 0
    ), !.

%Evaluates single pair
pairevaluator([V1,V1,_,_,V5], Result, [card(_, A),card(_, B)]):-
  (   V1 == A, V1 == B -> Result is V1*2              %pocketpair
    ; (V1 == A; V1 == B), V1 > V5 -> Result is V1 + 5 %pair with 1 card in hand
    ; (A > 11 ; B > 11) -> Result is 7                %pair on table high kicker on hand
    ; Result is 0                                     %else
  ), !.

%evaluates highest card
pairevaluator(_, Result, [card(_, A),card(_, B)]):-
  (   (A > 11 ; B > 11) -> Result is 5  %If we have a card Queen or higher
    ; Result is 0
    ).

%Calculates the chance of getting a straight, flush or both
chance(Hand, Value) :-
  sortByNumber(Hand, Sorted),
  sortByColor(Sorted, ColorSorted),
  doubleRemove(Sorted, Res),
  (   flush_Chance(ColorSorted, X), straight_Chance(Res, Y) ->  Value is X*Y+20  %Both Flush and Straight chance
    ; flush_Chance(ColorSorted, X) ->  Value is (X+5)*2                          %Flush chance
    ; straight_Chance(Sorted, Y) -> Value is Y+5                                 %Straight chance
    ; Value is 0
    ).

%Grants a bonus if there are 4 cards of the same color
flush_Chance([card(X, Y),card(X, _),card(X, _),card(X, _)|_], Y).
flush_Chance([card(_,_)|R], Y) :-
  flush_Chance(R, Y).

%Grants a bonus if there are 4 cards which gives a possible straight chance
straight_Chance([card(_,A),card(_,_),card(_,_),card(_,D)|_], A) :-
  X is A - D,
  (X == 3 ; X == 4).
straight_Chance([card(C,14)|R], Y) :- %Counting Ace as a possible straight with low cards aswell
  append(R, [card(C,1)], L),
  straight_Chance(L, Y).
straight_Chance([card(_,_)|R], Y) :-
  straight_Chance(R, Y).

/*Evaluates the hand pre_flop and returns a value
which the bot bases it's first decision on */
value_preflop([card(_, X),card(_, X)], Y, First):- % Cases where we have a pocketpair  % We want to know Last_to_Act because we need to know if we are big or small blind. Being small blind grants a lower value
  random(-75, 75, Humanfactor), % Calculated variable to make the bot harder to read.
 (  First == p1 -> Y is X*30 + 40 + Humanfactor
  ;  Y is X * 30 + Humanfactor ).

value_preflop([card(Color1, V1),card(Color2, V2)], Y, First):-
  absolut(V1, V2, Res), StraightChanceBonus is 20 - Res * Res, % Gives points if the cards have a chance of making a straight
  HandValue is V1 * V2,
  max(V1, V2, Hi), HighCardBonus is Hi * 2, % THe highest card in your starter hand grants bonus value
  random(-75, 75, Humanfactor),
  (  First == p1, Res < 5 -> Z is HandValue + StraightChanceBonus + HighCardBonus
  ;  First == p2, Res < 5 -> Z is HandValue + StraightChanceBonus + HighCardBonus - 30
  ;  First == p1 -> Z is HandValue + HighCardBonus + 30
  ;  First == p2 -> Z is HandValue + HighCardBonus - 30 ),
  (  Color1 == Color2 -> Y is Z + 30 + Humanfactor % If the cards are the same color we grant a 30 value bonus
  ; Y is Z + Humanfactor ).

%Returns the absolut value Z from X - Y
absolut(14, Y, Z):-
  Y < 6,
  absolut(1, Y, Z).
absolut(X, 14, Z):-
  X < 6,
  absolut(X, 1, Z).
absolut(X, Y, Z):-
  X < Y,
  Z is Y - X, !.
absolut(X, Y, Z):-
  Z is X - Y.

%Returns the max value of X and Y and returns in Z
max(X, Y, Y):-
  X < Y, !.
max(X, _, X).
