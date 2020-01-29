/* Copyright(C) 1992, Swedish Institute of Computer Science */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   File   : WGRAPHS.PL							      %
%   Maintainer : Mats Carlsson						      %
%            New versions of transpose/2, reduce/2, top_sort/2 by Dan Sahlin  %
%   Updated: 3 September 1999						      %
%   Purpose: Weighted graph-processing utilities			      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*  Adapted from shared code written by Richard A O'Keefe. */

/*  A weighted directed graph (wgraph) is represented as a list of
    (vertex-edgelist) pairs, where the pairs are in standard order (as
    produced by keysort with unique keys), the edgelist is a list of
    (neighbor-weight) pair also in standard order (as produced by
    keysort with unique keys), every weight is a nonnegative integer,
    and every neighbor appears as a vertex even if it has no neighbors
    itself.

    An undirected graph is represented as a directed graph where for
    each edge (U,V) there is a symmetric edge (V,U).

    An edge (U,V) with weight W is represented as the term U-(V-W).

    A vertex can be any term.  Two vertices are distinct iff they are
    not identical (==).

    A path is represented as a list of vertices.
    No vertex can appear twice in a path.
*/

:- module(wgraphs, [
	wgraph_to_ugraph/2,
	ugraph_to_wgraph/2,
	vertices_edges_to_wgraph/3,
	vertices/2,
	edges/2,
	add_vertices/3,
	del_vertices/3,
	add_edges/3,
	del_edges/3,
	transpose/2,
	neighbors/3,
	neighbours/3,
	transitive_closure/2,
	symmetric_closure/2,
	top_sort/2,
	max_path/5,
	min_path/5,
	min_paths/3,
	path/3,
	reduce/2,
	reachable/3,
	random_wgraph/4,
	min_tree/3
   ]).

:- use_module(library(ugraphs), [
        vertices/2,
        edges/2,
	add_vertices/3,
	neighbors/3,
	neighbours/3
   ]).

:- use_module(library(ordsets), [
	ord_union/3
   ]).

:- use_module(library(heaps), [
	list_to_heap/2,
	add_to_heap/4,
	get_from_heap/4
  ]).

:- use_module(library(lists), [
	reverse/2
  ]).




%   key_union(+KeySet1, +KeySet2, -Union)
%   is true when Union is the union of KeySet1 and KeySet2.  

key_union(Set1, [], Set) :- !, Set = Set1.
key_union([], Set2, Set2).
key_union([Head1|Tail1], [Head2|Tail2], Union) :-
	Head1 = H1-_,
	Head2 = H2-_,
	compare(Order, H1, H2),
	key_union(Order, Head1, Tail1, Head2, Tail2, Union).

key_union(<, Head1, Tail1, Head2, Tail2, [Head1|Union]) :-
	key_union(Tail1, [Head2|Tail2], Union).
key_union(=, H-I,   Tail1, H-J, Tail2, [H-K|Union]) :-
	K is min(I,J),
	key_union(Tail1, Tail2, Union).
key_union(>, Head1, Tail1, Head2, Tail2, [Head2|Union]) :-
	key_union([Head1|Tail1],  Tail2, Union).



%   key_subtract(+KeySet1, +KeySet2, -Subtract)
%   is true when Subtract is the difference between KeySet1 and KeySet2.

key_subtract(Set1, [], Set) :- !, Set = Set1.
key_subtract([], _, []).
key_subtract([Head1|Tail1], [Head2|Tail2], Subtract) :-
	Head1 = H1-_,
	Head2 = H2-_,
	compare(Order, H1, H2),
	key_subtract(Order, Head1, Tail1, Head2, Tail2, Subtract).

key_subtract(<, Head1, Tail1, Head2, Tail2, [Head1|Subtract]) :-
	key_subtract(Tail1, [Head2|Tail2], Subtract).
key_subtract(=, _,   Tail1, _, Tail2, Subtract) :-
	key_subtract(Tail1, Tail2, Subtract).
key_subtract(>, Head1, Tail1, _, Tail2, Subtract) :-
	key_subtract([Head1|Tail1],  Tail2, Subtract).



%   wgraph_to_ugraph(+WeightedGraph, -Graph)
%   is true if Graph has the same vertices and edges as WeightedGraph,
%   except the edges of Graph are unweighted.

wgraph_to_ugraph([], []).
wgraph_to_ugraph([V-WNeibs|WGraph], [V-Neibs|Graph]) :-
	wneibs_to_neibs(WNeibs, Neibs),
	wgraph_to_ugraph(WGraph, Graph).

wneibs_to_neibs([], []).
wneibs_to_neibs([V-_|WNeibs], [V|Neibs]) :-
	wneibs_to_neibs(WNeibs, Neibs).



%   ugraph_to_wgraph(+Graph, -WeightedGraph)
%   is true if WeightedGraph has the same vertices and edges as Graph,
%   except the edges of WeightedGraph all have weight 1.

ugraph_to_wgraph([], []).
ugraph_to_wgraph([V-Neibs|Graph], [V-WNeibs|WGraph]) :-
	neibs_to_wneibs(Neibs, WNeibs),
	ugraph_to_wgraph(Graph, WGraph).

neibs_to_wneibs([], []).
neibs_to_wneibs([V|Neibs], [V-1|WNeibs]) :-
	neibs_to_wneibs(Neibs, WNeibs).



%   ugraph_to_wgraph(+SubGraph, +WeightedGraph, -WeightedSubGraph)
%   is true if WeightedSubGraph has the same vertices and edges as SubGraph
%   and the same weights as the corresponding edges in WeightedGraph.

%% [MC] 3.8.6: made determinate
ugraph_to_wgraph([], _, []).
ugraph_to_wgraph([V1-Neibs1|G1], [V2-Neibs2|G2], [V1-Neibs|G]) :-
	V1==V2, !,
	neibs_to_wneibs(Neibs1, Neibs2, Neibs),
	ugraph_to_wgraph(G1, G2, G).
ugraph_to_wgraph(G1, [_|G2], G) :-
	G1 = [_|_],
	ugraph_to_wgraph(G1, G2, G).

%% [MC] 3.8.6: made determinate
neibs_to_wneibs([], _, []).
neibs_to_wneibs([V1|N1], [V2-W|N2], [V2-W|N]) :-
	V1==V2, !,
	neibs_to_wneibs(N1, N2, N).
neibs_to_wneibs(N1, [_|N2], N) :-
	N1 = [_|_],
	neibs_to_wneibs(N1, N2, N).


%   vertices_edges_to_wgraph(+Vertices, +Edges, -WeightedGraph)
%   is true if Vertices is a list of vertices, Edges is a list of
%   edges, and WeightedGraph is a graph built from Vertices and Edges.
%   Vertices and Edges may be in any order.  The vertices mentioned in
%   Edges do not have to occur explicitly in Vertices.  Vertices may
%   be used to specify vertices that are not connected to any edges.

vertices_edges_to_wgraph(Vertices0, Edges, Graph) :-
	sort(Vertices0, Vertices1),
	keysort(Edges, EdgeSet),
	edges_vertices(EdgeSet, Bag),
	sort(Bag, Vertices2),
	ord_union(Vertices1, Vertices2, VertexSet),
	group_edges(VertexSet, EdgeSet, Graph).

edges_vertices([], []).
edges_vertices([From-(To-_)|Edges], [From,To|Vertices]) :-
	edges_vertices(Edges, Vertices).



%   del_vertices(+WeightedGraph1, +Vertices, -WeightedGraph2)
%   is true if WeightedGraph2 is WeightedGraph1 with Vertices and all
%   edges to and from Vertices removed from it.

del_vertices(Graph0, Vs0, Graph) :-
	sort(Vs0, Vs),
	vertex_noughts(Vs, Set),
	graph_del_vertices(Graph0, Vs, Set, Graph).

vertex_noughts([], []).
vertex_noughts([V|Vs], [V-0|Us]) :- vertex_noughts(Vs, Us).

graph_del_vertices(G1, [], Set, G) :- !,
	graph_del_vertices(G1, Set, G).
graph_del_vertices([], _, _, []).
graph_del_vertices([V1-N1|G1], [V2|Vs], Set, G) :-
	compare(C, V1, V2),
	graph_del_vertices(C, V1, N1, G1, V2, Vs, Set, G).

graph_del_vertices(<, V1, N1, G1, V2, Vs, Set, [V1-N|G]) :-
	key_subtract(N1, Set, N),
	graph_del_vertices(G1, [V2|Vs], Set, G).
graph_del_vertices(=, _, _, G1, _, Vs, Set, G) :-
	graph_del_vertices(G1, Vs, Set, G).
graph_del_vertices(>, V1, N1, G1, _, Vs, Set, G) :-
	graph_del_vertices([V1-N1|G1], Vs, Set, G).

graph_del_vertices([], _, []).
graph_del_vertices([V1-N1|G1], Set, [V1-N|G]) :-
	key_subtract(N1, Set, N),
	graph_del_vertices(G1, Set, G).




%   add_edges(+WeightedGraph1, +Edges, -WeightedGraph2)
%   is true if WeightedGraph2 is WeightedGraph1 with Edges and their "to"
%   and "from" vertices added to it.

add_edges(Graph0, Edges0, Graph) :-
	sort(Edges0, EdgeSet),
	edges_vertices(EdgeSet, Vs0),
	sort(Vs0, Vs),
	group_edges(Vs, EdgeSet, Graph1),
	graph_union(Graph0, Graph1, Graph).

graph_union(G0, [], G) :- !, G = G0.
graph_union([], G, G).
graph_union([V1-N1|G1], [V2-N2|G2], G) :-
	compare(C, V1, V2),
	graph_union(C, V1, N1, G1, V2, N2, G2, G).

graph_union(<, V1, N1, G1, V2, N2, G2, [V1-N1|G]) :-
	graph_union(G1, [V2-N2|G2], G).
graph_union(=, V, N1, G1, _, N2, G2, [V-N|G]) :-
	key_union(N1, N2, N),
	graph_union(G1, G2, G).
graph_union(>, V1, N1, G1, V2, N2, G2, [V2-N2|G]) :-
	graph_union([V1-N1|G1], G2, G).



%   del_edges(+WeightedGraph1, +Edges, -WeightedGraph2)
%   is true if WeightedGraph2 is WeightedGraph1 with Edges removed from
%   it.

del_edges(Graph0, Edges0, Graph) :-
	sort(Edges0, EdgeSet),
	edges_vertices(EdgeSet, Vs0),
	sort(Vs0, Vs),
	group_edges(Vs, EdgeSet, Graph1),
	graph_difference(Graph0, Graph1, Graph).


graph_difference(G0, [], G) :- !, G = G0.
graph_difference([], _, []).
graph_difference([V1-N1|G1], [V2-N2|G2], G) :-
	compare(C, V1, V2),
	graph_difference(C, V1, N1, G1, V2, N2, G2, G).

graph_difference(<, V1, N1, G1, V2, N2, G2, [V1-N1|G]) :-
	graph_difference(G1, [V2-N2|G2], G).
graph_difference(=, V, N1, G1, _, N2, G2, [V-N|G]) :-
	key_subtract(N1, N2, N),
	graph_difference(G1, G2, G).
graph_difference(>, V1, N1, G1, _, _, G2, G) :-
	graph_difference([V1-N1|G1], G2, G).


group_edges([], _, []).
group_edges([Vertex|Vertices], Edges, [Vertex-Neibs|G]) :-
	group_edges(Edges, Vertex, Neibs0, RestEdges),
	keysort(Neibs0, Neibs1),
	group_min_edges(Neibs1, Neibs),	
	group_edges(Vertices, RestEdges, G).

group_edges([V0-X|Edges], V, [X|Neibs], RestEdges) :- V0==V, !,
	group_edges(Edges, V, Neibs, RestEdges).
group_edges(Edges, _, [], Edges).

group_min_edges([], []).
group_min_edges([Key-W|L1], L2) :- group_min_edges(L1, Key, W, L2).

group_min_edges([Key1-W1|L1], Key, W, L2) :- Key1==Key, !,
	W2 is min(W1,W),
	group_min_edges(L1, Key, W2, L2).
group_min_edges(L1, Key, W, [Key-W|L2]) :-
	group_min_edges(L1, L2).



%   transpose(+WeightedGraph, -Transpose)
%   is true if Transpose is the graph computed by replacing each edge
%   (u,v) in WeightedGraph by its symmetric edge (v,u).  It can only
%   be used one way around.  The cost is O(N log N).

transpose(Graph, Transpose) :-
	transpose_edges(Graph, TEdges, []),
	sort(TEdges, TEdges2),
	vertices(Graph, Vertices),
	group_edges(Vertices, TEdges2, Transpose).

transpose_edges([]) --> [].
transpose_edges([Vertex-Neibs|G]) -->
        transpose_edges(Neibs, Vertex),
	transpose_edges(G).

transpose_edges([], _) --> [].
transpose_edges([Neib-W|Neibs], Vertex) --> [Neib-(Vertex-W)],
	transpose_edges(Neibs, Vertex).


%   transitive_closure(+WeightedGraph, -Closure)
%   computes Closure as the transitive closure of WeightedGraph in
%   O(N^3) time.  Uses Floyd's algorithm and fragments of Barney
%   Pell's code.

transitive_closure(Graph, Closure) :-
	floyd(Graph, Graph, Closure).

floyd([], Closure, Closure).
floyd([V-_|G], E, Closure) :-
	neighbors(V, E, Y),
	floyd(E, V, Y, NewE),
	floyd(G, NewE, Closure).

floyd([], _, _, []).
floyd([X-Neibs|G], V, Y, [X-NewNeibs|NewG]) :-
	neighbors(V, Neibs, W), !,		% edge from X to V, weight W
	inc_weights(Y, X, W, Y1),
	key_union(Neibs, Y1, NewNeibs),
	floyd(G, V, Y, NewG).
floyd([X-Neibs|G], V, Y, [X-Neibs|NewG]) :-
	floyd(G, V, Y, NewG).

inc_weights([], _, _, []).
inc_weights([V-I|L1], X, J, [V-K|L2]) :-
	V \== X, !,
	K is I+J,
	inc_weights(L1, X, J, L2).
inc_weights([_|L1], X, J, L2) :-
	inc_weights(L1, X, J, L2).



%   symmetric_closure(+WeightedGraph, -Closure)
%   computes Closure as the symmetric closure of WeightedGraph, i.e.
%   for each edge (u,v) in WeightedGraph, add its symmetric edge
%   (v,u).  Approx O(N log N) time.  This is useful for making a
%   directed graph undirected.

symmetric_closure(Graph, Closure) :-
	transpose(Graph, Transpose),
	symmetric_closure(Graph, Transpose, Closure).

symmetric_closure([], [], []).
symmetric_closure([V-Neibs1|Graph], [V-Neibs2|Transpose], [V-Neibs|Closure]) :-
	key_union(Neibs1, Neibs2, Neibs),
	symmetric_closure(Graph, Transpose, Closure).



%   top_sort(+Graph, -Sorted)
%   finds a topological ordering of a Graph and returns the ordering
%   as a list of Sorted vertices.  Fails iff no ordering exists, i.e.
%   iff the graph contains cycles.  Takes O(N log N) time.

top_sort(Graph, Sorted) :-
	wgraph_to_ugraph(Graph, UGraph),
	ugraphs:top_sort(UGraph, Sorted).



%   max_path(+V1, +V2, +WeightedGraph, -Path, -Cost)
%   is true if Path is a list of vertices constituting a longest path
%   of cost Cost from V1 to V2 in WeightedGraph, there being no cyclic
%   paths from V1 to V2.  Takes O(N^2) time.

max_path(Initial, Final, Graph, Path, Cost) :-
	transpose(Graph, TGraph),
	wgraph_to_ugraph(Graph, UGraph),
	wgraph_to_ugraph(TGraph, UTGraph),
	ugraphs:max_path_init(Initial, Final, UGraph, UTGraph, UGraph2, Order),
	ugraphs:max_path_init(UGraph2, Val0),
	ugraph_to_wgraph(UGraph2, TGraph, TGraph2),
	max_path(Order, TGraph2, Val0, Val),
	ugraphs:max_path_select(Val, Path, Cost).

max_path([], _, Val, Val).
max_path([V|Order], Graph, Val0, Val) :-
	neighbors(V, Graph, Neibs),
	neighbors(V, Val0, Item),
	max_path_update(Neibs, V-Item, Val0, Val1),
	max_path(Order, Graph, Val1, Val).

%% [MC] 3.8.6: made determinate
max_path_update([], _, Val, Val).
max_path_update([N-W|Neibs], Item, [Item0|Val0], Val) :-
	Item0 = V0-(_-Cost0),
	N==V0, !,
	Item = V-(Path-Cost),
	Cost1 is Cost+W,
	(   Cost1>Cost0 -> Val = [V0-([V|Path]-Cost1)|Val1]
	;   Val = [Item0|Val1]
	),
	max_path_update(Neibs, Item, Val0, Val1).
max_path_update(Neibs, Item, [X|Val0], [X|Val]) :-
	Neibs = [_|_],
	max_path_update(Neibs, Item, Val0, Val).



%   min_path(+V1, +V2, +WeightedGraph, -Path, -Cost)
%   is true if Path is a list of vertices constituting a shortest path
%   with total cost Cost from V1 to V2 in WeightedGraph.  Takes O(N^2)
%   time.

% derived from Dijkstra's algorithm
min_path(Initial, Final, Graph, Path, Cost) :-
	list_to_heap([0-[Initial]], H),
	min_path(H, [], Final, Graph, Rev, Cost),
	reverse(Rev, Path).

min_path(H0, Closed0, Final, Graph, Rev, Cost) :-
	get_from_heap(H0, C0, Sofar, H1),
	Sofar = [V|_],
	(   V==Final -> Rev = Sofar, Cost = C0
	;   key_subtract([V-0], Closed0, [])
	->  min_path(H1, Closed0, Final, Graph, Rev, Cost)
	;   neighbors(V, Graph, Neibs),
	    key_subtract(Neibs, Closed0, Neibs1),
	    add_all_to_heap(Neibs1, C0, Sofar, H1, H),
	    key_union(Closed0, [V-0], Closed),
	    min_path(H, Closed, Final, Graph, Rev, Cost)
	).

add_all_to_heap([], _, _, H, H).
add_all_to_heap([V-X|Neibs], Y, Sofar, H0, H) :-
	Cost is X+Y,
	add_to_heap(H0, Cost, [V|Sofar], H1),
	add_all_to_heap(Neibs, Y, Sofar, H1, H).



%   min_paths(+Vertex, +WeightedGraph, -Tree)
%   is true if Tree is a tree of all the shortest paths from Vertex to
%   every other vertex in WeightedGraph.  This is the single-source
%   shortest paths problem.  Using Dijkstra's algorithm.

min_paths(Vertex, Graph, Tree) :-
	list_to_heap([0-([]-(Vertex-0))], H),
	dijkstra(H, [], Graph, [_|Edges]),
	vertices_edges_to_wgraph([], Edges, Tree).

dijkstra(H0, Closed, Graph, Edges) :-
	get_from_heap(H0, Cost, Edge, H), !,
	dijkstra(H, Cost, Edge, Closed, Graph, Edges).
dijkstra(_, _, _, []).

dijkstra(H0, Cost, Edge, Closed0, Graph, [Edge|List]) :-
	Edge = _-(ToW),
	ToW = Vertex-_,
	\+key_subtract([ToW], Closed0, []), !,
	neighbors(Vertex, Graph, Neibs),
	key_subtract(Neibs, Closed0, Neibs1),
	enqueue_edges(Neibs1, Cost, Vertex, H0, H),
	key_union(Closed0, [ToW], Closed),
	dijkstra(H, Closed, Graph, List).
dijkstra(H, _, _, Closed, Graph, List) :-
	dijkstra(H, Closed, Graph, List).

enqueue_edges([], _, _, H, H).
enqueue_edges([V1-W|Es], Cost, V, H0, H) :-
	Sum is Cost+W,
	add_to_heap(H0, Sum, V-(V1-W), H1),
	enqueue_edges(Es, Cost, V, H1, H).



%   path(+Vertex, +WeightedGraph, -Path)
%   is given a WeightedGraph and a Vertex of that WeightedGraph, and
%   returns a maximal Path rooted at Vertex, enumerating more Paths on
%   backtracking.

path(Initial, Graph, Path) :-
	wgraph_to_ugraph(Graph, UGraph),
	ugraphs:path(Initial, UGraph, Path).



%   reduce(+WeightedGraph, -Reduced)
%   is true if Reduced is the reduced graph for WeightedGraph. The
%   vertices of the reduced graph are the strongly connected
%   components of WeightedGraph.  There is an edge in Reduced from u
%   to v iff there is an edge in WeightedGraph from one of the
%   vertices in u to one of the vertices in v. A strongly connected
%   component is a maximal set of vertices where each vertex has a
%   path to every other vertex.
%   Algorithm from "Algorithms" by Sedgewick, page 482, Tarjan's algorithm.
%   Approximately linear in the maximum of arcs and nodes (O(N log N)).

reduce(Graph, Reduced) :-
	wgraph_to_ugraph(Graph, UGraph),
	ugraphs:strong_components(UGraph, SCCS, Map),
	reduced_vertices_edges(Graph, Vertices, Map, Edges, []),
	sort(Vertices, Vertices1),
	keysort(Edges, Edges1),
	group_edges(Vertices1, Edges1, Reduced),
	sort(SCCS, Vertices1).

reduced_vertices_edges([], [], _) --> [].
reduced_vertices_edges([V-Neibs|Graph], [V1|Vs], Map) -->
	{ugraphs:get_assoc(V, Map, N), N=node(_,_,V1)},
	reduced_edges(Neibs, V1, Map),
	reduced_vertices_edges(Graph, Vs, Map).

reduced_edges([], _, _) --> [].
reduced_edges([V-W|Vs], V1, Map) -->
	{ugraphs:get_assoc(V, Map, N), N=node(_,_,V2)},
	({V1==V2} -> []; [V1-(V2-W)]),
	reduced_edges(Vs, V1, Map).


%   reachable(+Vertex, +WeightedGraph, -Reachable)
%   is given a WeightedGraph and a Vertex of that WeightedGraph, and
%   returns the set of vertices that are Reachable from that Vertex.
%   Takes O(N^2) time.

reachable(Initial, Graph, Reachable) :-
	wgraph_to_ugraph(Graph, UGraph),
	ugraphs:reachable(Initial, UGraph, Reachable).



%   random_wgraph(+P, +N, +W, -WeightedGraph)
%   where P is a probability, unifies WeightedGraph with a random
%   graph with vertices 1..N where each possible edge is included with
%   probability P and random weight in 1..W.

random_wgraph(P, N, W, Graph) :-
	(   integer(W), W >= 1 -> true
	;   prolog:illarg(domain(integer,>=(1)),
	                  random_wgraph(P,N,W,Graph), 3)
	),
	ugraphs:random_ugraph(P, N, UGraph),
	random_wgraph(UGraph, W, Graph).

random_wgraph([], _, []).
random_wgraph([V-UNeibs|UGraph], W, [V-WNeibs|WGraph]) :-
	random_wneibs(UNeibs, W, WNeibs),
	random_wgraph(UGraph, W, WGraph).

random_wneibs([], _, []).
random_wneibs([N|Us], W, [N-Y|Vs]) :-
	ugraphs:random(X),
	Y is 1+integer(W*X),
	random_wneibs(Us, W, Vs).



%   min_tree(+WeightedGraph, -Tree, -Cost)
%   is true if Tree is a minimum-Cost spanning tree of an *undirected*
%   WeightedGraph with cost Cost, if it exists.  Using Kruskal's
%   algorithm.

min_tree(Graph, Tree, Cost) :-
	kruskal_init(Graph, E1, Map1),
	keysort(Map1, Map2),
	keymerge(Map2),
	length(Graph, N),
	keysort(E1, E2),
	kruskal(N, E2, E3, 0, Cost),
	vertices_edges_to_wgraph([], E3, Tree).

kruskal_init([], [], []).
kruskal_init([V-Neibs|Graph], Edges, Map) :-
	kruskal_init(Neibs, V, Graph, Edges, Map).

kruskal_init([B-W|Neibs], A, Graph, [W-f(A,B,U,V)|Edges], [A-U,B-V|Map]) :-
	A @> B, !,
	kruskal_init(Neibs, A, Graph, Edges, Map).
kruskal_init(_, _, Graph, Edges, Map) :-
	kruskal_init(Graph, Edges, Map).

kruskal(1, _, [], Cost, Cost) :- !.
kruskal(N, [W-f(A,B,U,V)|Queue], Edges, Cost0, Cost) :-
	(   U==V -> kruskal(N, Queue, Edges, Cost0, Cost)
	;   U=V,
	    Edges = [A-(B-W),B-(A-W)|Edges1],
	    M is N-1,
	    Cost1 is Cost0+W,
	    kruskal(M, Queue, Edges1, Cost1, Cost)
	).

keymerge([]).
keymerge([A-X|Map]) :- keymerge(Map, A, X).

keymerge([A0-X|Map], A, X) :- A0==A, !, keymerge(Map, A, X).
keymerge(Map, _, _) :- keymerge(Map).
