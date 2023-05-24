% gegeben
fam(robert, wilhelm, frieda).
fam(sandra, kurt, lisa).
fam(thomas, winfried, elke).
fam(christine, hans, paula).
fam(eva, thomas, christine).
fam(jan, robert, sandra).
fam(jens, robert, sandra).
fam(julia, robert, sandra).
fam(jutta, robert, sandra).
fam(anne, jan, eva).
fam(anke, jan, eva).
fam(tim, jens, heike).

% Hilfsfunktionen
mann(X) :- fam(_, X, _).
mann(tim).

frau(X) :- fam(_, _, X).
frau(anne).
frau(anke).
frau(julia).
frau(jutta).

vater(Y, X) :- fam(X, Y, _).
mutter(Y, X) :- fam(X, _, Y).

geschwister(X, Y) :- fam(X, V, M), fam(Y, V, M).

elternteil(X, Y) :- fam(Y, X, _).
elternteil(X, Y) :- fam(Y, _, X).

% a) X ist Bruder von Y 
bruder(X, Y) :- geschwister(X, Y), mann(X), X \== Y. 

% b) X ist Schwester von Y
schwester(X, Y) :- geschwister(X, Y), frau(X), X \== Y.

% c) X ist Enkel von Y 
% enkel(X, Y) :- X \== Y, mann(X), fam(X, V, _), fam(V, Y, _).
% enkel(X, Y) :- X \== Y, mann(X), fam(X, V, _), fam(V, _, Y).
% enkel(X, Y) :- X \== Y, mann(X), fam(X, _, M), fam(M, Y, _).
% enkel(X, Y) :- X \== Y, mann(X), fam(X, _, M), fam(M, _, Y).

enkel(X, Y) :- elternteil(E, X), elternteil(Y, E), mann(X). 
	
% d) X ist Onkel von Y
% onkel(X, Y) :- X \== Y, fam(Y, V, _), bruder(X, V).
% onkel(X, Y) :- X \== Y, fam(Y, _, M), bruder(X, M).

onkel(X, Y) :- elternteil(E, Y), bruder(X, E).

% e) X ist Tante von Y
% tante(X, Y) :- X \== Y, fam(Y, V, _), schwester(X, V).
% tante(X, Y) :- X \== Y, fam(Y, _, M), schwester(X, M).

tante(X, Y) :- elternteil(E, Y), schwester(X, E).

% 2
vorfahr(P, V) :- elternteil(V, P).
vorfahr(P, V) :- elternteil(V, M), vorfahr(P, M). 
