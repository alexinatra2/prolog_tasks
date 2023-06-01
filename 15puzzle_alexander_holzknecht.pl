% Beleg KI Alexander Holzknecht
% Bibl. Nummer: : s83768
% Matrikel Nummer: 50947

% Aufgabe a:
% Zu zeigen ist, dass die durch h(s) definierte Funktion sich als
% Heuristik für das Lösen des 15 Puzzles eignet. Dazu muss gezeigt
% werden, dass h(s) stets kleiner als die tatsächliche Anzahl an
% Lösungsschritten ist.
% Für den Fall das h_c = 0, gilt h(s) = h_m(s). Die Zulässigkeit
% der Manhattan Distanz h_m als Heuristik ist in der Vorlesung wie
% folgt bewiesen:
% "(Die Manhattan Distanz ist die) Anzahl Verschiebungen, die nötig
% sind, um ein Plättchen auf direktem Weg zum Ziel zu schieben,
% summiert über alle Plättchen. Diese Heuristik ist zulässig, da
% für jedes Plättchen mindestens diese Anzahl an Verschiebungen
% nötig ist".
% Im Fall h_c = 2 betrachten wir den Fall, bei der die tatsächliche
% Entfernung so nah wie möglich ist. Dies ist dann der Fall, wenn p
% und q direkt nebeneinander liegen, und in der Zielposition des 
% jeweils anderen stehen. Um p und q zu vertauschen muss
% p vertikal verschoben werden, anschließend horizontal
% in Richtung Zielposition, q in seine Zielposition
% und p zurück vertikal in die ursprüngliche Reihe geschoben werden. 
% Dies erfordert 4 Schritte. Die beiden horizontalen Schritte werden
% bereits von der Manhattan Distanz geschätzt. Die weiteren 2
% Schritte werden von h_c erfasst. Somit ist h(s) maximal gleich groß
% wie die Anzahl Schritte der tatsächlichen schnellsten Lösung und 
% somit eine zulässige Heuristik.
%
%
% Aufgabe b:
:- use_module(library(clpfd)).

% Definition of final board
goal([[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]]).

% Cost function taking into account traversed path (with weights)
% and heuristical value.
f([H | T], Y) :- g([H | T], Y1), h(H, Y2), Y is Y1 + Y2.

% Path weight function calculating the sum of weights of edges
% in a path. Since all moves in the 15 puzzle take one step, 
% the graph is unweighted and the weight is assumed to be 1 
% for each. 1 is subtracted at the end, such as not to count the 
% initial state of the board as a move.
g(L, Y) :- length(L, Y0), Y is Y0 - 1.

% The heuristic function
h(Board, D) :- md(Board, MD), cd(Board, CD), D is MD + CD.
% the Manhattan distance performs way better. to use it 
% comment the code above and uncomment the code below.
% h(Board, D) :- md(Board, D).

% md calculates the Manhattan distance of the entire board 
% implied by the definition of md0 for a single tile.
% 16 is not considered a tile and is therefore not considered
% to have a Manhattan distance.
md0(16, _, 0).
md0(Position, Number, Distance) :- 
  ExpectedRow is div(Number - 1, 4),
  ActualRow is div(Position - 1, 4),
  ExpectedCol is mod(Number - 1, 4),
  ActualCol is mod(Position - 1, 4),
  Distance is abs(ExpectedRow - ActualRow) + abs(ExpectedCol - ActualCol).

md(Board, MD) :-
  flatten(Board, B),
  goal(GoalB),
  flatten(GoalB,G),
  maplist(md0, B, G, Diffs),
  sumlist(Diffs, MD), !.
  
incorrect_row(Row, Number) :- Number == 16; ActualRow is div(Number - 1, 4), Row \== ActualRow.

filter_incorrect_row(Row, L, Filtered) :- exclude(incorrect_row(Row), L, Filtered).

is_unsorted([A,B|Tail]) :- A > B; is_unsorted([B|Tail]).

cd0([Row|Board], RowNumber) :-
  filter_incorrect_row(RowNumber, Row, Filtered),
  is_unsorted(Filtered);
  NewRowNumber is RowNumber + 1,
  cd0(Board, NewRowNumber). 

cd(Board, Dist) :- cd0(Board, 0), Dist is 2; Dist is 0.
  
% Definition of all possible horizontal moves.
% 16 represents the empty tile, so only it 
% can change places.
sr([16,A,B,C], [A,16,B,C]).
sr([A,16,B,C], [A,B,16,C]).
sr([A,B,16,C], [A,B,C,16]).

% Shifts are bidirectional.
shiftr(X,Y) :- sr(X,Y); sr(Y,X).

% Each row of tiles has the option to shift 
% horizontally in the 3 (times 2) previously
% defined ways.
adjr([X, R2, R3, R4], [Y, R2, R3, R4]) :- shiftr(X, Y).
adjr([R1, X, R3, R4], [R1, Y, R3, R4]) :- shiftr(X, Y).
adjr([R1, R2, X, R4], [R1, R2, Y, R4]) :- shiftr(X, Y).
adjr([R1, R2, R3, X], [R1, R2, R3, Y]) :- shiftr(X, Y).

% Definition of vertical moves via transposition
% of matrix columns.
adjc(X, Y) :- 
  transpose(X, XT),
  adjr(XT, YT),
  transpose(YT, Y).

% The resulting graph contains of both horizontal
% and vertical shifts.
adj(Board1, Board2) :-
  adjr(Board1, Board2);
  adjc(Board1, Board2).

% A depth first search implementation that filters
% out heuristically impractical paths utilizing 
% the given Limit value as a threshold.
idfs(Node, Limit, Visited, Path) :-
  goal(Node),
  reverse(Visited, Path);
  adj(Node, NewNeighbor),
  f(Visited, FValue),
  FValue =< Limit,
  not(member(NewNeighbor, Visited)),
  idfs(NewNeighbor, Limit, [NewNeighbor | Visited], Path).

% IDA*-Search:
% Depth first search with iterative deepening. 
% The Limit parameter is gradually increased.
idas_search(Start, Limit, MaxLimit, Path) :- 
  idfs(Start, Limit, [Start], Path);
   writeln(Limit),
  Limit =< MaxLimit,
  NewLimit is Limit + 1,
  idas_search(Start, NewLimit, MaxLimit, Path).

idas_search(Start, MaxLimit, Path) :- 
  h(Start, HeuristicalEstimate),
  idas_search(Start, HeuristicalEstimate, MaxLimit, Path).
  % idas_search(Start, 1, MaxLimit, Path). 

print_board(Board) :- maplist(writeln, Board), writeln('').
print(Boards) :- maplist(print_board, Boards).

solve(Board, Limit) :- 
  idas_search(Board, Limit, Path),
  print(Path),
  length(Path, PLength),
  Steps is PLength - 1,
  writeln('Steps taken: '),
  writeln(Steps).

% according to the internet, 15 puzzles can all be solved
% within at most 80 moves. Hence, utilizing solve/2 is not
% necessary.
solve(Board) :- solve(Board, 80).

%%%%%%%%%%%%%%%%%%%%%%% test cases %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
solve0() :- solve([[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,16,15]]).
solve1() :- solve([[1,2,3,4],[5,6,7,8],[9,10,11,12],[16,13,14,15]]).
solve2() :- solve([[1,3,2,5],[6,4,16,10],[14,7,15,8],[12,9,13,11]]).
solve3() :- solve([[1,2,3,16],[5,6,7,4],[9,10,11,8],[13,14,15,12]]).
solve4() :- solve([[1,2,3,4],[13,9,5,10],[6,8,11,15],[7,12,14,16]]).

