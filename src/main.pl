/**
 * Graph Spanning Tree
 * Obtain all spanning trees of a graph.
 * @file            main.pl
 * @version         1.0
 * @author          Daniel Konecny (xkonec75)
 * @organisation    Brno University of Technology - Faculty of Information Technologies
 * @date            25. 04. 2021
 */

:- use_module(input).
:- use_module(graph).


main :-
	% Loads all edges from input file.
	load(Edges),

	% Saves vertices to a dynamic list for later checks.
	save_all_vertices(vertices, Edges),

	% Gets all permutations of edges of the graph.
	bagof(Permutation, permutation(Edges, Permutation), Permutations),

	% Minimizes permutations to candidates for spanning trees.
	get_trees(Permutations, Candidates),

	% Filters out trees that do not cover all vertices.
	filter_tree_all_vertices(Candidates, AllVertices),

	% Filters out trees that are not continuous.
	filter_tree_discontinuous(AllVertices, Continuous),

	% Sorts trees, edges and vertices and filters out duplicates.
	sort_trees(Continuous, Sorted),

	print_solutions(Sorted),

	halt.
