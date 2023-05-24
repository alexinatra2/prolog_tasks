% builtins
% member(X, [X | _]).
% member(X, [_ | T]) :- member(X, T).

% append([ ], Y, Y).
% append([X|L1],L2,[X|L3]):-append(L1,L2,L3).

adj0(X, Y) :- member((X, Y), [(1, 2), (2, 4), (2, 5), (3, 4), (3, 6), (4, 5)]).
adj(X, Y) :- adj0(X, Y); adj0(Y, X).

% sucht alle X, für die P wahr ist und erzeugt daraus die Liste L
% findall(X, P, L)  
% not(P): negation by failure
% uninformierte Suchverfahren
bfs([H | T], Discovered) :- 
	goal(H);
	findall(Node, (adj(H ,Node), not(member(Node, Discovered))), NewNeighbors), 
	append(T, NewNeighbors, Queue),
	append(Discovered, NewNeighbors, Dc),
	bfs(Queue, Dc).

% dfs(Node) :-
%   goal(Node);
%   adj(Node, Neighbor),
%   dfs(Neighbor).

dfs(Node, Path) :-
	goal(Node);
	adj(Node, NewNeighbor),
	not(member(NewNeighbor, Path)),
	dfs(NewNeighbor, [NewNeighbor | Path]).

% iterative_dfs(StartNode, Goal, Limit, MaxLimit, Path) :-
%     dfs(StartNode, Goal, Limit, [StartNode], Path);
%     MaxLimit \== Limit,
%     NewLimit is Limit + 1,
%     iterative_dfs(StartNode, Goal, NewLimit, MaxLimit, Path).

% idfs(current, goal, path, depth)
idfs(Node, Node, [], 0).

