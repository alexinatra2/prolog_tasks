% diff(polynom, variable, derivative)
% diff(F, X, DF)
% dF/dX = DF

diff(X, X, 1).
diff(C, X, 0) :- atomic(C), C \== X.
diff(-F, X, -DF) :- diff(F, X, DF).
diff(C * F, X, C * DF) :- diff(C, X, 0), diff(F, X, DF).
diff(F + G, X, DF + DG) :- diff(F, X, DF), diff(G, X, DG).
diff(F - G, X, DF - DG) :- diff(F, X, DF), diff(G, X, DG).
diff(X ^ N, X, N * X ^ M) :- atomic(X), number(N), M is N - 1.

% 3
% extra regeln
diff(sin(F), X, DF * cos(F)) :- diff(F, X, DF).
diff(cos(F), X, -DF * sin(F)) :- diff(F, X, DF).
diff(F ^ N, X, DF * N * F ^ M) :- diff(F, X, DF), M is N - 1.

rew(0 + A, A).
rew(A + 0, A).
rew(1 * A, A).
rew(A * 1, A).
rew(0 * _, 0).
rew(_ * 0, 0).
rew(A ^ 1, A). % extra regel
rew(A + B, C) :- number(A), number(B), C is A + B.
rew(A - B, C) :- number(A), number(B), C is A - B.
rew(A * B, C) :- number(A), number(B), C is A * B.
rew(A, A).

simp(A + B, C) :- simp(A, SA), simp(B, SB), rew(SA + SB, C).
simp(A * B, C) :- simp(A, SA), simp(B, SB), rew(SA * SB, C).
simp(A, B) :- rew(A, B).
simp(A, A).

diffsimp(F, X, DFS) :- diff(F, X, DF), simp(DF, DFS).
