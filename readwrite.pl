:-module(readwrite, [add/3, file_name/2]).
:-use_module(library(file_systems)).
:-use_module(library(codesio)).

file_name(This, FileName) :-
  format_to_codes('~p.txt', [This], Codes),
  name(FileName, Codes).

add(Hand, Win_or_loss, From) :-
  file_exists(From),
  find_and_write(Hand, From, 'temp.txt', Win_or_loss),
  delete_file(From),
  rename_file('temp.txt', From).

add(Hand, Win_or_loss, From) :-
  \+file_exists(From),
  open(From, append, Stream1),
  format(Stream1, 'total_WL(~d,~d).~n', [0,0]),
  close(Stream1),
  find_and_write(Hand, From, 'temp.txt', Win_or_loss),
  delete_file(From),
  rename_file('temp.txt', From).

find_and_write(Hand, First, Second, loss) :-
  open(First, read, Stream1),
  open(Second, append, Stream2),
  read(Stream1, total_WL(X, Y)),
  Total_loss is Y + 1,
  format(Stream2, 'total_WL(~d,~d).~n', [X,Total_loss]),
  file_search(Stream1, Stream2, Hand, loss),
  close(Stream1),
  close(Stream2).

find_and_write(Hand, First, Second, win) :-
  open(First, read, Stream1),
  open(Second, append, Stream2),
  read(Stream1, total_WL(X, Y)),
  Total_Win is X + 1,
  format(Stream2, 'total_WL(~d,~d).~n', [Total_Win,Y]),
  file_search(Stream1, Stream2, Hand, win),
  close(Stream1),
  close(Stream2).

%file_search(+Stream, +Hand, -Lines) uses the stream to find the hand
file_search(Stream1, Stream2, [Flop, Turn, River], Win_or_loss) :-
  \+at_end_of_stream(Stream1),
  read(Stream1, [Hands, X, Y]),
  (   Hands == Flop, Win_or_loss == win -> Won is X + 1, write(Stream2, [Flop, Won, Y]), write(Stream2, '.'), nl(Stream2), file_search(Stream1, Stream2, [[], Turn, River], Win_or_loss)
    ; Hands == Flop, Win_or_loss == loss -> Loss is Y + 1, write(Stream2, [Flop, X, Loss]), write(Stream2, '.'), nl(Stream2), file_search(Stream1, Stream2, [[], Turn, River], Win_or_loss)
    ; Hands == Turn, Win_or_loss == win -> Won is X + 1, write(Stream2, [Turn, Won, Y]), write(Stream2, '.'), nl(Stream2), file_search(Stream1, Stream2, [Flop, [], River], Win_or_loss)
    ; Hands == Turn, Win_or_loss == loss -> Loss is Y + 1, write(Stream2, [Turn, X, Loss]), write(Stream2, '.'), nl(Stream2), file_search(Stream1, Stream2, [Flop, [], River], Win_or_loss)
    ; Hands == River, Win_or_loss == win -> Won is X + 1, write(Stream2, [River, Won, Y]), write(Stream2, '.'), nl(Stream2), file_search(Stream1, Stream2, [Flop, Turn, []], Win_or_loss)
    ; Hands == River, Win_or_loss == loss -> Loss is Y + 1, write(Stream2, [River, X, Loss]), write(Stream2, '.'), nl(Stream2), file_search(Stream1, Stream2, [Flop, Turn, []], Win_or_loss)
    ; write(Stream2, [Hands, X, Y]), write(Stream2, '.'), nl(Stream2), file_search(Stream1, Stream2, [Flop, Turn, River], Win_or_loss)
    ).

file_search(S, Stream2, [Flop, Turn, River], Win_or_loss) :-
  at_end_of_stream(S),
  continued(S, Stream2, [Flop, Turn, River], Win_or_loss).

continued(_, _, [[],[],[]], _).  

continued(_, Stream2, [Flop, Turn, River], win) :-
  Flop \== [],
  write(Stream2, [Flop, 1, 0]), write(Stream2, '.'), nl(Stream2),
  continued(S, Stream2, [[], Turn, River], win).
continued(_, Stream2, [Flop, Turn, River], win) :-
  Turn \== [],
  write(Stream2, [Turn, 1, 0]), write(Stream2, '.'), nl(Stream2),
  continued(S, Stream2, [Flop, [], River], win).
continued(_, Stream2, [Flop, Turn, River], win) :-
  River \== [],
  write(Stream2, [River, 1, 0]), write(Stream2, '.'), nl(Stream2), !.


continued(_, Stream2, [Flop, Turn, River], loss) :-
  Flop \== [],
  write(Stream2, [Flop, 0, 1]), write(Stream2, '.'), nl(Stream2),
  continued(S, Stream2, [[], Turn, River], loss).
continued(_, Stream2, [Flop, Turn, River], loss) :-
  Turn \== [],
  write(Stream2, [Turn, 0, 1]), write(Stream2, '.'), nl(Stream2),
  continued(S, Stream2, [Flop, [], River], loss).
continued(_, Stream2, [Flop, Turn, River], loss) :-
  River \== [],
  write(Stream2, [River, 0, 1]), write(Stream2, '.'), nl(Stream2), !.


file_search(Stream1, Stream2, [[],[],[]], _) :-
  \+at_end_of_stream(Stream1),
  repeat,
  read(Stream1, X),
  write(Stream2, X), write(Stream2, '.'), nl(Stream2),
  X = end_of_file, !.
