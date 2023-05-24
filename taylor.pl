diff(X, X, 1).
diff(C, X, 0) :- atomic(C), C \== X.
diff(-F, X, -DF) :- diff(F, X, DF).
diff(C * F, X, C * DF) :- diff(C, X, 0), diff(F, X, DF).
diff(F + G, X, DF + DG) :- diff(F, X, DF), diff(G, X, DG).
diff(F - G, X, DF - DG) :- diff(F, X, DF), diff(G, X, DG).
diff(X ^ N, X, N * X ^ M) :- atomic(X), number(N), M is N - 1.
diff(sin(F), X, DF * cos(F)) :- diff(F, X, DF).
diff(cos(F), X, -DF * sin(F)) :- diff(F, X, DF).
diff(F ^ N, X, DF * N * F ^ M) :- diff(F, X, DF), M is N - 1.

diffSimp(F, X, SDF) :- diff(F, X, DF), simp(DF, SDF).

diffN(F, X, DF, 1) :- diff(F, X, DF).
diffN(F, X, DNF, N) :- M is N - 1, diffN(F, X, DF, M), diff(DF, X, DNF).

diffNSimp(F, X, SDF, N) :- diffN(F, X, DNF, N), simp(DNF, SDF).

rew(0 + A, A).
rew(A + 0, A).
rew(1 * A, A).
rew(A * 1, A).
rew(0 * _, 0).
rew(_ * 0, 0).
rew(A ^ 1, A). 
rew(A + B, C) :- number(A), number(B), C is A + B.
rew(A - B, C) :- number(A), number(B), C is A - B.
rew(A * B, C) :- number(A), number(B), C is A * B.
rew(A, A).

simp(A + B, C) :- simp(A, SA), simp(B, SB), rew(SA + SB, C).
simp(A * B, C) :- simp(A, SA), simp(B, SB), rew(SA * SB, C).
simp(A, B) :- rew(A, B).
simp(A, A).

eval(x, X0, X0) :- number(X0).
eval(C, _, C) :- number(C).
eval(x ^ N, X0, R) :- number(N), number(X0), R is X0 ^ N.
eval(C * P, X0, R) :- number(C), number(X0), eval(P, X0, R0), R is R0 * C.
eval(P1 + P2, X0, R) :- eval(P1, X0, R1), eval(P2, X0, R2), R is R1 + R2.
eval(P1 - P2, X0, R) :- eval(P1, X0, R1), eval(P2, X0, R2), R is R1 - R.

  
