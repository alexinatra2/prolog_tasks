% 4 
% eval(polynom, stelle, ergebnis)
eval(x, X0, X0) :- number(X0).
eval(C, _, C) :- number(C).
eval(x ^ N, X0, R) :- number(N), number(X0), R is X0 ^ N.
eval(C * P, X0, R) :- number(C), number(X0), eval(P, X0, R0), R is R0 * C.
eval(P1 + P2, X0, R) :- eval(P1, X0, R1), eval(P2, X0, R2), R is R1 + R2.
eval(P1 - P2, X0, R) :- eval(P1, X0, R1), eval(P2, X0, R2), R is R1 - R.

