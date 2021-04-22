/**
 * Graph Spanning Tree
 * Obtain all spanning trees of a graph.
 * @file            graph.pl
 * @version         1.0
 * @author          Daniel Konecny (xkonec75)
 * @organisation    Brno University of Technology - Faculty of Information Technologies
 * @date            21. 04. 2021
 */

:- module(graph, [get_tree/2, print_tree/1, tree/1]).

:- dynamic accessibles/1.
:- dynamic tree/1.


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
get_accesibles([T|TS]) :-
	new_accessible(T, New),
	assertz(accessibles(New)),
	get_accesibles(TS).
get_accesibles([_|TS]) :-
	get_accesibles(TS).

new_accessible([[V1], [V2]], V2) :- accessibles(V1), \+ accessibles(V2), !.
new_accessible([[V1], [V2]], V1) :- accessibles(V2), \+ accessibles(V1), !.


print_tree([E]) :- print_edge(E), write("\n").
print_tree([E|ES]) :- print_edge(E), write(" "), print_tree(ES).

print_edge([[V1], [V2]]) :-	write(V1), write("-"), write(V2).
