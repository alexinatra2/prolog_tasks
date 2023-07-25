:-use_module(library(clpfd)).

player(Symbol) :- member(Symbol, [x, o]).
free(Symbol) :- not(player(Symbol)).

equals(X,X).
player_won(Symbol, Board) :-
  Board = [X1, X2, X3, X4, X5, X6, X7, X8, X9],
  player(Symbol),
  (
    maplist(equals(Symbol), [X1, X2, X3]);
    maplist(equals(Symbol), [X4, X5, X6]);
    maplist(equals(Symbol), [X7, X8, X9]);
    maplist(equals(Symbol), [X1, X4, X7]);
    maplist(equals(Symbol), [X2, X5, X8]);
    maplist(equals(Symbol), [X3, X6, X9]);
    maplist(equals(Symbol), [X1, X5, X9]);
    maplist(equals(Symbol), [X3, X5, X7])
  ).

board_full([]).
board_full([Head|Tail]) :- player(Head), board_full(Tail).

count([],_,0).
count([X|T],X,Y):- count(T,X,Z), Y is 1+Z.
count([X1|T],X,Z):- X1\=X,count(T,X,Z).

counts(Board, XCount, OCount) :-
  count(Board, x, XCount),
  count(Board, o, OCount).

player_turn(Player, Board) :-
  counts(Board, XCount, OCount),
  (
    XCount #= OCount,
    Player = x;
    XCount #= OCount + 1,
    Player = o
  ).

place_at([HeadBefore|TailBefore], Player, Index, [HeadAfter|TailAfter]) :-
  Index #= 1,
  free(HeadBefore),
  HeadAfter = Player,
  TailBefore = TailAfter;
  HeadAfter = HeadBefore,
  NewIndex is Index - 1,
  place_at(TailBefore, Player, NewIndex, TailAfter), !.

free_spaces([], []).
free_spaces([Head|Tail], FreeSpaceIndices) :-
  free_spaces(Tail, FreeSpaceIndicesSoFar),
  (
    free(Head) -> (
      length(Tail, RemainingLength),
      Position is 9 - RemainingLength,
      FreeSpaceIndices = [Position|FreeSpaceIndicesSoFar]
    );
    FreeSpaceIndices = FreeSpaceIndicesSoFar
  ).

move(BoardState, PossibleNextBoardState) :-
  player_turn(Player, BoardState),
  free_spaces(BoardState, FreeSpacesIndices),
  member(FreeSpaceIndex, FreeSpacesIndices),
  place_at(BoardState, Player, FreeSpaceIndex, PossibleNextBoardState).
  
minimax(Board, MinimaxValue) :-
  player_won(x, Board),
  MinimaxValue = 1;
  player_won(o, Board),
  MinimaxValue = -1;
  board_full(Board),
  MinimaxValue = 0;
  player_turn(Player, Board),
  findall(PossibleNextBoardState, move(Board, PossibleNextBoardState), PossibleNextBoardStates),
  maplist(minimax, PossibleNextBoardStates, MinimaxValues),
  (
    Player = x -> list_max(MinimaxValues, MinimaxValue);
    list_min(MinimaxValues, MinimaxValue)
  ).

list_max([P|T], O) :- list_max(T, P, O).
list_max([], P, P).
list_max([H|T], P, O) :-
  (H > P -> 
    list_max(T, H, O);    
    list_max(T, P, O)
  ).


list_min([P|T], O) :- list_min(T, P, O).
list_min([], P, P).
list_min([H|T], P, O) :-
  (H < P -> 
    list_min(T, H, O);    
    list_min(T, P, O)
  ).

