:-module(noobBotOffline, [bot/4]).

bot(Turn, Last_to_act, [Firstact|Table], [Firstact|Table]):-
  Firstact == p1,
  Last_to_act = p2.
bot(Turn, Last_to_act, Table, Table):-
  Last_to_act = p1.
