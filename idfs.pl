idfs(Startnode, Endnode, Limit, Path) :-
  dfs(Startnode, Endnode, Limit, [Startnode], Path);
  NewLimit is Limit + 1,
  idfs(Startnode, Endnode, NewLimit, Path).

dfs(Node, Goal, DepthRemaining, Visited, Path) :-
  Goal == Node,
  reverse(Visited, Path);
  DepthRemaining \== 0,
  NewDepth is DepthRemaining - 1,
  adj(Node, NewNeighbor),
  not(member(NewNeighbor, Visited)),
  dfs(NewNeighbor, Goal, NewDepth, [NewNeighbor | Visited], Path).
