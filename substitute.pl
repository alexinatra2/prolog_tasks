:-use_module(library(clpfd)).

variable(X) :- 
  not(number(X)), 
  atomic(X).

expand(Variable, Term, Expansion) :-
  Term = (A * Variable + B), Expansion = (A * Variable + B);
  Term = (A * Variable), expand(Variable, (A * Variable + 0), Expansion);
  Term = (Variable + B), expand(Variable, (1 * Variable + B), Expansion);
  Term = B, expand(Variable, (0 * Variable + B), Expansion).

expand_all(Variable, Term, Expansion) :- expand(Variable, Term, Expansion).
expand_all(Variable, Term + Rest, Expansion) :-
  expand(Variable, Term, TermExpansion),
  expand_all(Variable, Rest, RestExpansion),
  Expansion = TermExpansion + RestExpansion.

sub(A, _, _, A) :- atomic(A).

sub(Formula, Term, ToReplaceWith, Result) :-
  expand(Variable, Formula, FormulaExpansion),
  expand(Variable, Term, TermExpansion),
  FormulaExpansion = (M * Variable + N),
  TermExpansion = (A * Variable + B),
  variable(Variable), 
  maplist(number, [A, B, M, N]),
  0 #= mod(M, A),
  Factor is M // A,
  N #= Factor * B,
  Result = Factor * ToReplaceWith.
