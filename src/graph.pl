/**
 * Graph Spanning Tree
 * Obtain all spanning trees of a graph.
 * @file            graph.pl
 * @version         1.0
 * @author          Daniel Konecny (xkonec75)
 * @organisation    Brno University of Technology - Faculty of Information Technologies
 * @date            25. 04. 2021
 */

:- module(graph, [
	save_all_vertices/2,
	get_trees/2,
	filter_tree_all_vertices/2,
	filter_tree_discontinuous/2,
	sort_trees/2,
	print_solutions/1
]).

:- dynamic accessibles/1.
:- dynamic tree/1.
:- dynamic vertices/1.
:- dynamic temp_vertices/1.


get_trees([], []) :- !.
get_trees([P|PS], [T|TS]) :-
	get_tree(P, T),
	retractall(tree(_)),
	get_trees(PS, TS).

/**
 * Filters spanning tree candidates from permutations of edges.
 */
get_tree([], Tree) :- bagof(Edges, tree(Edges), Tree), !.
get_tree([E|ES], Tree) :-
	has_cycle(E) ->
	(
		retractall(accessibles(_)),
		bagof(Edges, tree(Edges), Tree),
		!
	);
	(
		assertz(tree(E)),
		retractall(accessibles(_)),
		get_tree(ES, Tree)
	).
/**
 * Adds edges to tree until cycle is formed.
 * It does that by saving currently accessible vertices.
 * First it saves one vertex of the new edge and then it tries
   to access the other vertex using edges from the tree.
 * If it is accessible, then there is a cycle, this edge is not added
   and the edge insertion is stopped.
 */
has_cycle([[E1], [E2]]) :-
	bagof(Edges, tree(Edges), Tree),
	assertz(accessibles(E1)),
	has_cycle(Tree, E2).
has_cycle(Tree, Goal) :-
	bagof(Vertex, accessibles(Vertex), Previous),
	get_accesibles(Tree),
	bagof(Vertex, accessibles(Vertex), Current),
	Current == Previous ->
	(
		accessibles(Goal), !
	);
	(
		has_cycle(Tree, Goal)
	).

get_accesibles([]).
get_accesibles([Edge | Edges]) :-
	new_accessible(Edge, New),
	assertz(accessibles(New)),
	get_accesibles(Edges).
get_accesibles([Edge | Edges]) :-
	\+ new_accessible(Edge, _),
	get_accesibles(Edges).

new_accessible([[V1], [V2]], V2) :- accessibles(V1), \+ accessibles(V2), !.
new_accessible([[V1], [V2]], V1) :- accessibles(V2), \+ accessibles(V1), !.

/**
 * Used for storing the number of vertices in a graph for
   analyzing whether all vertices are in a graph candidate.
 */
save_all_vertices(_, []) :- !.
save_all_vertices(Location, [[[V1], [V2]] | Edges]) :-
	Vertex =.. [Location, V1],
	\+ Vertex,
	call(assertz, Vertex),
	save_all_vertices(Location, [[[V1], [V2]] | Edges]).
save_all_vertices(Location, [[[_], [V2]] | Edges]) :-
	Vertex =.. [Location, V2],
	\+ Vertex,
	call(assertz, Vertex),
	save_all_vertices(Location, Edges).
save_all_vertices(Location, [_|Edges]) :-
	save_all_vertices(Location, Edges).

get_vertex_count(Location, Count) :-
	Vertices =.. [Location, _],
    findall(t, Vertices, L),
    length(L, Count).

/**
 * Filters out tree that do not cover all vertices.
 */
filter_tree_all_vertices(Trees, Filtered) :-
	get_vertex_count(vertices, GraphCount),
	filter_tree_all_vertices(Trees, Filtered, GraphCount).
filter_tree_all_vertices([], [], _) :- !.
filter_tree_all_vertices([Tree | Trees], [Tree | Filtered], GraphCount) :-
	get_temp_vertex_count(temp_vertices, Tree, TreeCount),
	TreeCount == GraphCount,
	filter_tree_all_vertices(Trees, Filtered, GraphCount).
filter_tree_all_vertices([Tree | Trees], Filtered, GraphCount) :-
	get_temp_vertex_count(temp_vertices, Tree, TreeCount),
	TreeCount \= GraphCount,
	filter_tree_all_vertices(Trees, Filtered, GraphCount).

get_temp_vertex_count(Location, Edges, Count) :-
	Vertices =.. [Location, _],
	call(retractall, Vertices),
	save_all_vertices(Location, Edges),
	get_vertex_count(Location, Count).

/**
 * Filters out tree that are not continuous.
 */
filter_tree_discontinuous(Trees, Filtered) :-
	get_vertex_count(vertices, GraphCount),
	filter_tree_discontinuous(Trees, Filtered, GraphCount).
filter_tree_discontinuous([], [], _) :- !.
filter_tree_discontinuous([Tree | Trees], [Tree | Filtered], GraphCount) :-
	get_accessible_vertex_count(Tree, TreeCount),
	TreeCount == GraphCount,
	filter_tree_discontinuous(Trees, Filtered, GraphCount).
filter_tree_discontinuous([Tree | Trees], Filtered, GraphCount) :-
	get_accessible_vertex_count(Tree, TreeCount),
	TreeCount \= GraphCount,
	filter_tree_discontinuous(Trees, Filtered, GraphCount).

get_accessible_vertex_count([[[E1], [E2]] | Edges], Count) :-
	assertz(accessibles(E1)),
	assertz(accessibles(E2)),
	get_accesibles(Edges),
	findall(t, accessibles(_), L),
    length(L, Count),
    retractall(accessibles(_)).

/**
 * Sorts trees by:
	* vertex order in an edge,
	* edge order in a tree,
	* tree order in given list.
 */
sort_trees(Trees, SortedTrees) :-
	sort_edges(Trees, SortedEdges),
	sort(SortedEdges, SortedTrees).

sort_edges([], []).
sort_edges([Edge | Edges], [SortedEdge | SortedEdges]) :-
	sort_vertices(Edge, SortedVertices),
	sort(SortedVertices, SortedEdge),
	sort_edges(Edges, SortedEdges).
	
sort_vertices([], []).
sort_vertices([Vertex | Vertices], [SortedVertex | SortedVertices]) :-
	sort(Vertex, SortedVertex),
	sort_vertices(Vertices, SortedVertices).

/**
 * Prints the solution to stdout in format defined by the assignment.
 */
print_solutions([]) :- !.
print_solutions([T|TS]) :- print_tree(T), print_solutions(TS).

print_tree([E]) :- print_edge(E), write('\n').
print_tree([E|ES]) :- print_edge(E), write(' '), print_tree(ES).

print_edge([[V1], [V2]]) :-	write(V1), write('-'), write(V2).
