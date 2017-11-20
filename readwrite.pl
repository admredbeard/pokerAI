:-module(readwrite, [add/3, file_name/2]).
:-use_module(library(file_systems)).
:-use_module(library(codesio)).

file_name(This, FileName) :-
  format_to_codes('~p.txt', [This], Codes),
  name(FileName, Codes).

add(Hand, Win_or_loss, From) :-
  file_exists(From),
  \+file_exists('temp.txt'),
  find_and_write(Hand, From, 'temp.txt', Win_or_loss),
  delete_file(From),
  rename_file('temp.txt', From).

add(Hand, Win_or_loss, From) :-
  \+file_exists(From),
  open(From, append, Stream1),
  format(Stream1, '~d.', [0]),
  close(Stream1),
  find_and_write(Hand, From, 'temp.txt', Win_or_loss),
  delete_file(From),
  rename_file('temp.txt', From).

find_and_write(Hand, First, Second, Win_or_loss) :-
  open(First, read, Stream1),
  open(Second, append, Stream2),
  read(Stream1, X),
  Total_hands is X + 1,
  write(Stream2, Total_hands), write(Stream2, '.'), nl(Stream2),
  file_search(Stream1, Stream2, Hand, Win_or_loss),
  close(Stream1),
  close(Stream2).

%file_search(+Stream, +Hand, -Lines) uses the stream to find the hand
file_search(Stream1, Stream2, Hand, Win_or_loss) :-
  \+at_end_of_stream(Stream1),
  read(Stream1, [Hands, X, Y]),
  (   Hands == Hand, Win_or_loss = win -> Won is X + 1, write(Stream2, [Hand, Won, Y]), write(Stream2, '.'), nl(Stream2), continued(Stream1, Stream2)
    ; Hands == Hand, Win_or_loss = loss -> Loss is Y + 1, write(Stream2, [Hand, X, Loss]), write(Stream2, '.'), nl(Stream2), continued(Stream1, Stream2)
    ; write(Stream2, [Hands, X, Y]), write(Stream2, '.'), nl(Stream2), file_search(Stream1, Stream2, Hand, Win_or_loss)
    ).

file_search(Stream1, Stream2, Hand, win) :-
  write(Stream2, [Hand, 1, 0]), write(Stream2, '.'), nl(Stream2).

file_search(Stream1, Stream2, Hand, loss) :-
  write(Stream2, [Hand, 0, 1]), write(Stream2, '.'), nl(Stream2).


continued(Stream1, Stream2) :-
  repeat,
  read(Stream1, X),
  write(Stream2, X), write(Stream2, '.'), nl(Stream2),
  X = end_of_file, !.
