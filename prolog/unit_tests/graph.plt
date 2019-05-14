% graph.plt
/*  
    
    Author:        Yibin Zhao
    E-mail:        yibin.zhao@intel.com, yibin.joseph.zhao@gmail.com
    Copyright (C): 

*/

:- begin_tests(graph). 

:- load_files('../CSoG/graph.pl', [imports(all)]).

% setup a simple database for testing needs
% graph: 
%   vertices: v1, v0, (v1), v2
%   edges: v0 -> v1, v0 -> v2
setup_database_simple() :- 
    asserta(graph:vertex(graph,v0)), 
    asserta(graph:vertex(graph,v1)),
    assertz(graph:vertex(graph,v1)),
    assertz(graph:vertex(graph,v2)),
    assertz(graph:edge(graph,[v0,v1])),
    assertz(graph:edge(graph,[v0,v2])),
    asserta(graph:edge(graph,[v0,v2])).

setup_database_cyclic() :- 
    asserta(graph:vertex(graph,v0)), 
    asserta(graph:vertex(graph,v1)),
    assertz(graph:vertex(graph,v2)),
    assertz(graph:vertex(graph,v3)),
    assertz(graph:edge(graph,[v0,v1])),
    assertz(graph:edge(graph,[v0,v2])),
    assertz(graph:edge(graph,[v3,v0])),
    assertz(graph:edge(graph,[v1,v3])).

cleanup_database() :- 
    retractall(graph:vertex(_,_)),
    retractall(graph:edge(_,_)). 

test(insert_vertex_valid, [ setup(setup_database_simple()), 
                            cleanup(cleanup_database())
                        ]) :- 
    graph:insert_vertex(graph,v3), 
    graph:vertex(graph,v3).

test(insert_vertex_nonatomic, [ setup(setup_database_simple()), 
                                cleanup(cleanup_database()),
                                fail
                            ]) :- 
    graph:insert_vertex(G,v3). 

test(is_vertex_fact, [   setup(setup_database_simple()), 
                        cleanup(cleanup_database())
                    ]) :- 
    graph:is_vertex(graph,v0). 

test(is_vertex_multi, [ setup(setup_database_simple()), 
                        cleanup(cleanup_database()),
                        nondet
                    ]) :- 
    graph:is_vertex(graph,v1). 

test(is_vertex_set, [   setup(setup_database_simple()), 
                        cleanup(cleanup_database()),
                        set(V == [v0,v1,v2])
                    ]) :- 
    graph:is_vertex(graph,V).

test(is_vertex_nonvertex, [ setup(setup_database_simple()),
                            cleanup(cleanup_database()),
                            fail
                        ]) :- 
    graph:is_vertex(graph, v3). 

test(remove_vertex_valid, [ setup(setup_database_simple()), 
                            cleanup(cleanup_database()) 
                        ]) :- 
    graph:remove_vertex(graph, v0), 
    \+ graph:vertex(graph, v0). 

test(remove_vertex_mulpi, [ setup(setup_database_simple()),
                            cleanup(cleanup_database())
                        ]) :- 
    graph:remove_vertex(graph,v1), 
    \+ graph:vertex(graph,v1). 

test(remove_vertex_nonvertex, [ setup(setup_database_simple()),
                                cleanup(cleanup_database()), 
                                fail
                            ]) :- 
    graph:remove_vertex(graph, v3). 

test(all_vertices_none, [true(Vs == [])]) :- 
    graph:all_vertices(graph,Vs).

test(all_vertices_valid, [  setup(setup_database_simple()),
                            cleanup(cleanup_database()),
                            true(Vs == [v0, v1, v2])
                        ]) :- 
    graph:all_vertices(graph,Vs).

test(validate_edge_valid, [ setup(setup_database_simple()), 
                            cleanup(cleanup_database())
                        ]) :-
    graph:validate_edge(graph,[v1,v0]). 

test(validate_edge_nonatomic, [ setup(setup_database_simple()), 
                                cleanup(cleanup_database()),
                                fail
                            ]) :-
    graph:validate_edge(graph,[v0,T]). 

test(validate_edge_invalid, [   setup(setup_database_simple()), 
                                cleanup(cleanup_database()),
                                fail
                            ]) :-
    graph:validate_edge(graph,[v0,v3]). 

test(insert_edge_valid, [   setup(setup_database_simple()), 
                            cleanup(cleanup_database())
                        ]) :- 
    graph:insert_edge(graph, [v1,v2]), 
    graph:edge(graph,[v1,v2]).

test(insert_edge_nonatomic, [   setup(setup_database_simple()), 
                                cleanup(cleanup_database()),
                                fail
                            ]) :- 
    graph:insert_edge(G,[v1,v2]). 

test(is_edge_fact, [    setup(setup_database_simple()), 
                        cleanup(cleanup_database())
                    ]) :- 
    graph:is_edge(graph,[v0,v1]). 

test(is_edge_multi, [   setup(setup_database_simple()), 
                        cleanup(cleanup_database()),
                        nondet
                    ]) :- 
    graph:is_edge(graph,[v0,v2]). 

test(is_edge_set, [ setup(setup_database_simple()), 
                    cleanup(cleanup_database()),
                    set(E == [[v0,v1],[v0,v2]])
                ]) :- 
    graph:is_edge(graph,E).

test(is_edge_nonedge, [ setup(setup_database_simple()),
                        cleanup(cleanup_database()),
                        fail
                    ]) :- 
    graph:is_edge(graph,[v1,v2]). 

test(is_edge_reverse, [ setup(setup_database_simple()),
                        cleanup(cleanup_database()),
                        fail
                    ]) :- 
    graph:is_edge(graph,[v1,v0]). 

test(remove_edge_valid, [   setup(setup_database_simple()), 
                            cleanup(cleanup_database()) 
                        ]) :- 
    graph:remove_edge(graph,[v0,v1]), 
    \+ graph:edge(graph,[v0,v1]). 

test(remove_edge_mulpi, [   setup(setup_database_simple()),
                            cleanup(cleanup_database())
                        ]) :- 
    graph:remove_edge(graph,[v0,v2]), 
    \+ graph:edge(graph,[v0,v2]). 

test(remove_edge_nonedge, [ setup(setup_database_simple()),
                            cleanup(cleanup_database()), 
                            fail
                        ]) :- 
    graph:remove_edge(graph,[v0,v3]). 

test(remove_edge_reverse, [ setup(setup_database_simple()),
                            cleanup(cleanup_database()), 
                            fail
                        ]) :- 
    graph:remove_edge(graph,[v1,v0]). 

test(all_edges_none, [true(Es == [])]) :- 
    graph:all_edges(graph,Es).

test(all_edges_valid, [  setup(setup_database_simple()),
                            cleanup(cleanup_database()),
                            true(Es == [[v0,v1],[v0,v2]])
                        ]) :- 
    graph:all_edges(graph,Es).

test(get_directed_edges_valid, [    setup(setup_database_simple()), 
                                    cleanup(cleanup_database()),
                                    true(Edges == [[v0,v1],[v0,v2]])
                                ]) :- 
    graph:get_directed_edges(graph,v0,Edges). 

test(get_directed_edges_empty, [    setup(setup_database_simple()), 
                                    cleanup(cleanup_database()),
                                    true(Edges == [])
                                ]) :- 
    graph:get_directed_edges(graph,v1,Edges).

test(get_directed_edges_invalid, [  setup(setup_database_simple()), 
                                    cleanup(cleanup_database()),
                                    fail 
                                ]) :- 
    graph:get_directed_edges(graph,v3,_). 

test(graph_sources_none, [true(S == [])]) :- 
    graph:graph_sources(graph,S).

test(graph_sources_valid, [ setup(setup_database_simple()), 
                            cleanup(cleanup_database()),
                            true(S == [v0])
                        ]) :-
    graph:graph_sources(graph,S).

test(graph_nonsources_none, [true(NS == [])]) :- 
    graph:graph_nonsources(graph,NS).

test(graph_nonsources_valid, [  setup(setup_database_simple()), 
                                cleanup(cleanup_database()),
                                true(NS == [v1,v2])
                            ]) :-
    graph:graph_nonsources(graph,NS).

test(graph_terminals_none, [true(T == [])]) :- 
    graph:graph_terminals(graph,T).

test(graph_terminals_valid, [   setup(setup_database_simple()), 
                                cleanup(cleanup_database()),
                                true(T == [v1,v2])
                            ]) :-
    graph:graph_terminals(graph,T).

test(graph_nonterminals_none, [true(NT == [])]) :- 
    graph:graph_nonterminals(graph,NT).

test(graph_nonterminals_valid, [    setup(setup_database_simple()), 
                                    cleanup(cleanup_database()),
                                    true(NT == [v0])
                                ]) :-
    graph:graph_nonterminals(graph,NT).

test(cycle_from_vertex_acyclic, [   setup(setup_database_simple()),
                                    cleanup(cleanup_database()),
                                    fail
                                ]) :- 
    graph:cycle_from_vertex(graph,v0).

test(cycle_from_vertex_none, [fail]) :-
    graph:cycle_from_vertex(graph,v0).

test(cycle_from_vertex_cycle, [ setup(setup_database_cyclic()),
                                cleanup(cleanup_database())
                            ]) :- 
    graph:cycle_from_vertex(graph,v0).

test(cycle_from_vertex_weak, [  setup(setup_database_cyclic()),
                                cleanup(cleanup_database()),
                                fail
                            ]) :- 
    graph:cycle_from_vertex(graph,v2).

test(acyclic_acyclic, [ setup(setup_database_simple()), 
                        cleanup(cleanup_database())
                    ]) :- 
    graph:acyclic(graph). 

test(acyclic_nonatomic, [   setup(setup_database_simple()),
                            cleanup(cleanup_database()),
                            fail
                        ]) :-
    graph:acyclic(G). 

test(acyclic_empty, [fail]) :- graph:acyclic(graph). 

test(acyclic_cyclic, [  setup(setup_database_cyclic()), 
                        cleanup(cleanup_database()),
                        fail
                    ]) :- 
    graph:acyclic(graph). 

:- end_tests(graph).
