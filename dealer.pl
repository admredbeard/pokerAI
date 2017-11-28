
:-module(dealer,[dealhands/4, dealflop/3, dealturn/3, dealriver/2,
                createDeck/1, shuffleDeck/2, playersevenCards/5,
                card/2, playersixCards/4, playerfiveCards/3]).

:-use_module(library(random)).
:-use_module(library(system)).



% There are 4 different colors, clubs, diamonds, hearts and spades
color('c').
color('d').
color('h').
color('s').

% There are 13 different values
value(14).
value(2).
value(3).
value(4).
value(5).
value(6).
value(7).
value(8).
value(9).
value(10).
value(11).
value(12).
value(13).



/*card(Color-,Value-)
A card is defined by 2 terms, Color and Value*/
card(Color, Value):-
  color(Color),
  value(Value).

createDeck(Deck):-
  findall(card(X, Y), card(X, Y), Deck). %finds all possible cards and puts in the list L

shuffleDeck(Deck, Shuffled) :-
  statistics(walltime, [MSSinceStart, _]), %Mera random än now(X) då walltime är i millisekunder?
  setrand(MSSinceStart), %sets the seed to milliseconds since start * system time in seconds
  random_permutation(Deck, Shuffled).  %shuffles the deck L

% dealtp(Deck+), sets P1cards and P2cards aswell as deck
dealhands([C1, C3], [C2, C4], [C1,C2,C3,C4|Deck], Deck).

% dealflop(Deck+), sets flop and rest of deck
dealflop([C2,C3,C4], [_,C2,C3,C4|Deck], Deck).

% dealturn(Deck+), sets turn and rest of deck
dealturn([C2], [_,C2|Deck], Deck).

% dealriver(Deck+), sets river and rest of the deck
dealriver([C2], [_,C2|_]).

%player1Cards(Cards-), gets the asserted predicates and returns them as one list
playersevenCards([C1, C2], [C3, C4, C5], [C6], [C7], [C1,C2,C3,C4,C5,C6,C7]).
playersixCards([C1, C2], [C3, C4, C5], [C6], [C1, C2,C3, C4, C5,C6]).
playerfiveCards([C1,C2], [C3,C4,C5], [C1,C2,C3,C4,C5]).
