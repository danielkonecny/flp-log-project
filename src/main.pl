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

:- dynamic solutions/1.


process_perms([]) :- !.
process_perms([P|PS]) :-
	get_tree(P, Tree),
	assertz(solutions(Tree)),
	retractall(tree(_)),
	process_perms(PS).


print_solutions([]) :- !.
print_solutions([T|TS]) :- print_tree(T), print_solutions(TS).



main :-
	load(Edges),
	bagof(Perm, permutation(Edges, Perm), Perms),
	process_perms(Perms),

	setof(Tree, solutions(Tree), Solutions),
	
	% Seradit kazdou hranu od nizsiho pismene.
	% Seradit hrany podle prvniho pismene.
	% Seradit hrany podle druheho pismene.
	% Odstranit duplicity.
	% Odstranit grafy nepokryvajici vsechny vrcholy.
	% Odstranit nespojite grafy.

	print_solutions(Solutions),

	halt.
