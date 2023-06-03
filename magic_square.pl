:- use_module(library(clpfd)).

condition(List) :- sumlist(List, 15).

magic_square3(Rows) :-
  length(Rows, 3), 
  maplist(same_length(Rows), Rows),
  flatten(Rows, Numbers),
  Numbers ins 1..9,
  all_distinct(Numbers),
  transpose(Rows, Cols),
  label(Numbers),
  forall(member(Row, Rows), condition(Row)),
  forall(member(Col, Cols), condition(Col)),
  diagonal3(Numbers).


diagonal3([N1, _, N2, _, N3, _, N4, _, N5]) :- 
  condition([N1, N3, N5]), 
  condition([N2, N3, N4]).

