:- use_module(library(clpfd)).

goal([[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]]).

f([H | T], Y) :- g([H | T], Y1), h(H, Y2), Y is Y1 + Y2.
g(L, Y) :- length(L, Y0), Y is Y0 - 1.
h(Board, D) :- md(Board, MD), cd(Board, CD), D is MD + CD.

% md calculates the manhattan distance of the entire board 
% implied by the definition of md0 for a single tile.
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
  
sr([16,A,B,C], [A,16,B,C]).
sr([A,16,B,C], [A,B,16,C]).
sr([A,B,16,C], [A,B,C,16]).
shiftr(X,Y) :- sr(X,Y); sr(Y,X).

adjr([X, R2, R3, R4], [Y, R2, R3, R4]) :- shiftr(X, Y).
adjr([R1, X, R3, R4], [R1, Y, R3, R4]) :- shiftr(X, Y).
adjr([R1, R2, X, R4], [R1, R2, Y, R4]) :- shiftr(X, Y).
adjr([R1, R2, R3, X], [R1, R2, R3, Y]) :- shiftr(X, Y).

adjc(X, Y) :- 
  transpose(X, XT),
  adjr(XT, YT),
  transpose(YT, Y).

adj(Board1, Board2) :-
  adjr(Board1, Board2);
  adjc(Board1, Board2).

idas_search(Start, MaxLimit, Path) :- 
  idas_search0(Start, 1, MaxLimit, Path).

idas_search0(Start, Limit, MaxLimit, Path) :- 
  idfs(Start, Limit, [Start], Path);
  Limit =< MaxLimit,
  NewLimit is Limit + 1,
  idas_search0(Start, NewLimit, MaxLimit, Path).

idfs(Node, Limit, Visited, Path) :-
  goal(Node),
  reverse(Visited, Path);
  adj(Node, NewNeighbor),
  f(Visited, FValue),
  FValue =< Limit,
  not(member(NewNeighbor, Visited)),
  idfs(NewNeighbor, Limit, [NewNeighbor | Visited], Path).

print_board(Board) :- maplist(writeln, Board), writeln('').
print(Boards) :- maplist(print_board, Boards).

solve(Board, Limit) :- 
  idas_search(Board, Limit, Path),
  print(Path),
  length(Path, PLength),
  writeln('Steps taken: '),
  writeln(PLength).

% according to the internet 15 puzzles can all be solved
% within at most 80 moves
solve(Board) :- solve(Board, 80).

% test cases
solve0() :- solve0([[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,16,15]]).
