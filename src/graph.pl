/**
 * Graph Spanning Tree
 * Obtain all spanning trees of a graph.
 * @file            graph.pl
 * @version         1.0
 * @author          Daniel Konecny (xkonec75)
 * @organisation    Brno University of Technology - Faculty of Information Technologies
 * @date            21. 04. 2021
 */

:- module(graph, [get_tree/2, print_tree/1]).


get_tree([], []).
get_tree([E|ES], [E|TS]) :-
	adds_no_cycle(E, TS),
	get_tree(ES, TS).

adds_no_cycle(Edge, Tree).


print_tree([E]) :-
	print_edge(E),
	write("\n").
print_tree([E|ES]) :-
	print_edge(E),
	write(" "),
	print_tree(ES).

print_edge(Edge) :-
	nth0(0, Edge, E1), nth0(1, Edge, E2),
	nth0(0, E1, V1), nth0(0, E2, V2),
	write(V1), write("-"), write(V2).
