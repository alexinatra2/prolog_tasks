:- use_module(library(clpfd)).
:- use_module(library(clpb)).
:- [idfs].

% 0 - Kleinzschachwitz, 1 - Pillnitz
valid_side(F, Z, _, _) :- F == Z. 
valid_side(_, Z, K, W) :- Z \== K, Z \== W.

adj([F0, Z0, K0, W0], [F1, Z1, K1, W1]) :- 
  valid_side(F0, Z0, K0, W0),
  valid_side(F1, Z1, K1, W1),
  taut(F1, F0 # 1). 

print_list(L) :- maplist(writeln, L), writeln(''). 

solution() :- 
  idfs([0, 0, 0, 0], [1, 1, 1, 1], 0, P), 
  length(P, PLength), 
  print_list(P),
  writeln(PLength).
