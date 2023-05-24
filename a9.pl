:- [idfs].

valid_boat(C, M) :- member((C, M), [(0, 1), (0, 2), (2, 0), (1, 0), (1, 1)]).
valid_coast(C, M) :- member((C, M), [(0, 0), (0, 1), (0, 2), (0, 3), (1, 1), (1, 2), (1, 3), (2, 2), (2, 3), (3, 3), (1, 0), (2, 0), (3, 0)]).
valid(C0, M0, C1, M1) :- valid_coast(C0, M0), valid_coast(C1, M1).

adj([0, C00, M00, C10, M10], [1, C01, M01, C11, M11]) :-
  valid_boat(TC, TM),
  valid(C00, M00, C10, M10),
  valid(C01, M01, C11, M11),
  C01 is C00 - TC,
  M01 is M00 - TM,
  C11 is C10 + TC,
  M11 is M10 + TM.

adj([1, C00, M00, C10, M10], [0, C01, M01, C11, M11]) :-
  valid_boat(TC, TM),
  valid(C00, M00, C10, M10),
  valid(C01, M01, C11, M11),
  C01 is C00 + TC,
  M01 is M00 + TM,
  C11 is C10 - TC,
  M11 is M10 - TM.

print_list(L) :- maplist(writeln, L), writeln(''). 

solution() :- 
  idfs([0, 3, 3, 0, 0], [1, 0, 0, 3, 3], 0, P), 
  length(P, PLength), 
  print_list(P),
  writeln(PLength).
