:- use_module(library(clpfd)).

goal([[1, 2, 3], [4, 5, 6], [7, 8, 9]]).

% the cost of a path is evaluated by summing the 
% weight of all its edges and adding the heuristic
% value of the last node.
% Note, that the resulting value, should never exceed
% the minimum amount of steps.
% This however should be guaranteed by the heuristic.
f([H | T], Y) :- g([H | T], Y1), h(H, Y2), Y is Y1 + Y2.

% g calculates length of path up until this point
% 1 is subtracted from the result such as not to 
% count the last vertex.
% Length suffices as a means of calculating the 
% path weight, as each step of the puzzle takes
% exactly one move.
g(L, Y) :- length(L, Y0), Y is Y0 - 1.

h(Board, D) :- md(Board, MD), cd(Board, CD), D is MD + CD.

% hd calculates the hamming distance of the entire board 
% implied by the definition of hd0 for a single tile.
hd(Board, HD) :-
  flatten(Board, B),
  goal(GoalB),
  flatten(GoalB,G),
  maplist(hd0, B, G, Diffs),
  sumlist(Diffs, HD), !.

hd0(_, 9, 0).
hd0(B, G, Diff) :- B == G, Diff is 0; Diff is 1.

% md calculates the manhattan distance of the entire board 
% implied by the definition of md0 for a single tile.
md(Board, MD) :-
  flatten(Board, B),
  goal(GoalB),
  flatten(GoalB,G),
  maplist(md0, B, G, Diffs),
  sumlist(Diffs, MD), !.
  
md0(_, 9, 0).
md0(Position, Number, Distance) :- 
  ExpectedRow is div(Number - 1, 3),
  ActualRow is div(Position - 1, 3),
  ExpectedCol is mod(Number - 1, 3),
  ActualCol is mod(Position - 1, 3),
  Distance is abs(ExpectedRow - ActualRow) + abs(ExpectedCol - ActualCol).

incorrect_row(Row, Position) :- ActualRow is div(Position - 1, 3), Row \== ActualRow.
filter_incorrect_row(Row, L, Filtered) :- exclude(incorrect_row(Row), L, Filtered).
is_unsorted([A,B|Tail]) :- A > B; is_unsorted([B|Tail]).

cd(Board, Dist) :- 
  cd0(Board, 0),
  Dist is 2;
  Dist is 0.

cd0([Row|Board], RowNumber) :-
  filter_incorrect_row(RowNumber, Row, Filtered),
  is_unsorted(Filtered);
  NewRowNumber is RowNumber + 1,
  cd0(Board, NewRowNumber). 

% md0(16, _, 0).
% md0(Position, Number, Distance) :- 
%   ExpectedRow is div(Number - 1, 4),
%   ActualRow is div(Position - 1, 4),
%   ExpectedCol is mod(Number - 1, 4),
%   ActualCol is mod(Position - 1, 4),
%   Distance is abs(ExpectedRow - ActualRow) + abs(ExpectedCol - ActualCol).
  
sr([9,A,B], [A,9,B]).
sr([A,9,B], [A,B,9]).
shiftr(X,Y) :- sr(X,Y); sr(Y,X).

adjr([X, R2, R3], [Y, R2, R3]) :- shiftr(X, Y).
adjr([R1, X, R3], [R1, Y, R3]) :- shiftr(X, Y).
adjr([R1, R2, X], [R1, R2, Y]) :- shiftr(X, Y).

adjc(X, Y) :- 
  transpose(X, XT),
  adjr(XT, YT),
  transpose(YT, Y).

adj(Board1, Board2) :-
  adjr(Board1, Board2);
  adjc(Board1, Board2).

idas_search(Start, MaxLimit, Path) :- 
  idas_search0(Start, 0, MaxLimit, Path).

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
  Steps is PLength - 1,
  writeln('Steps taken: '),
  writeln(Steps).

solve(Board) :- solve(Board, 100).

solve0() :- solve([[1,2,3],[4,5,6],[7,9,8]]). % 1 step (1,095 inferences, 0.000s)
solve1() :- solve([[3,2,4],[7,9,6],[5,1,8]]). % 18 Steps )
