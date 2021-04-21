/**
 * Graph Spanning Tree
 * Obtain all spanning trees of a graph.
 * @file            main.pl
 * @version         1.0
 * @author          Daniel Konecny (xkonec75)
 * @organisation    Brno University of Technology - Faculty of Information Technologies
 * @date            21. 04. 2021
 */

:- use_module(input).
:- use_module(graph).


process_perms([]) :- !.
process_perms([P|PS]) :-
	get_tree(P, Tree),
	print_tree(Tree),
	process_perms(PS).


main :-
	load(Edges),
	bagof(Perm, permutation(Edges, Perm), Perms),
	process_perms(Perms),
	halt.
