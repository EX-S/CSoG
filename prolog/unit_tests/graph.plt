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
setup_database() :- 
    asserta(graph:vertex(graph,v0)), 
    asserta(graph:vertex(graph,v1)),
    assertz(graph:vertex(graph,v1)),
    assertz(graph:vertex(graph,v2)),
    assertz(graph:edge(graph,[v0,v1])),
    assertz(graph:edge(graph,[v0,v2])),
    asserta(graph:edge(graph,[v0,v2])).

cleanup_database() :- 
    retractall(graph:vertex(_,_)),
    retractall(graph:edge(_,_)). 

test(insert_vertex_valid, [ setup(setup_database()), 
                            cleanup(cleanup_database())
                        ]) :- 
    graph:insert_vertex(graph,v3), 
    graph:vertex(graph,v3).

test(insert_vertex_nonatomic, [ setup(setup_database()), 
                                cleanup(cleanup_database()),
                                fail
                            ]) :- 
    graph:insert_vertex(G,v3). 

test(is_vertex_fact, [   setup(setup_database()), 
                        cleanup(cleanup_database())
                    ]) :- 
    graph:is_vertex(graph,v0). 

test(is_vertex_multi, [ setup(setup_database()), 
                        cleanup(cleanup_database()),
                        nondet
                    ]) :- 
    graph:is_vertex(graph,v1). 

test(is_vertex_set, [   setup(setup_database()), 
                        cleanup(cleanup_database()),
                        set(Vertices == [[v0,v1,v2]])
                    ]) :- 
    setof(V,graph:is_vertex(graph,V),Vertices).

test(is_vertex_nonvertex, [ setup(setup_database()),
                            cleanup(cleanup_database()),
                            fail
                        ]) :- 
    graph:is_vertex(graph, v3). 

test(remove_vertex_valid, [ setup(setup_database()), 
                            cleanup(cleanup_database()) 
                        ]) :- 
    graph:remove_vertex(graph, v0), 
    \+ graph:vertex(graph, v0). 

test(remove_vertex_mulpi, [ setup(setup_database()),
                            cleanup(cleanup_database())
                        ]) :- 
    graph:remove_vertex(graph,v1), 
    \+ graph:vertex(graph,v1). 

test(remove_vertex_nonvertex, [ setup(setup_database()),
                                cleanup(cleanup_database()), 
                                fail
                            ]) :- 
    graph:remove_vertex(graph, v3). 

test(validate_edge_valid, [ setup(setup_database()), 
                            cleanup(cleanup_database())
                        ]) :-
    graph:validate_edge(graph,[v1,v0]). 

test(validate_edge_nonatomic, [ setup(setup_database()), 
                                cleanup(cleanup_database()),
                                fail
                            ]) :-
    graph:validate_edge(graph,[v0,T]). 

test(validate_edge_invalid, [   setup(setup_database()), 
                                cleanup(cleanup_database()),
                                fail
                            ]) :-
    graph:validate_edge(graph,[v0,v3]). 

test(insert_edge_valid, [   setup(setup_database()), 
                            cleanup(cleanup_database())
                        ]) :- 
    graph:insert_edge(graph, [v1,v2]), 
    graph:edge(graph,[v1,v2]).

test(insert_edge_nonatomic, [   setup(setup_database()), 
                                cleanup(cleanup_database()),
                                fail
                            ]) :- 
    graph:insert_edge(G,[v1,v2]). 

test(is_edge_fact, [    setup(setup_database()), 
                        cleanup(cleanup_database())
                    ]) :- 
    graph:is_edge(graph,[v0,v1]). 

test(is_edge_multi, [   setup(setup_database()), 
                        cleanup(cleanup_database()),
                        nondet
                    ]) :- 
    graph:is_edge(graph,[v0,v2]). 

test(is_edge_set, [ setup(setup_database()), 
                    cleanup(cleanup_database()),
                    set(Edges == [[[v0,v1],[v0,v2]]])
                ]) :- 
    setof(E,graph:is_edge(graph,E),Edges).

test(is_edge_nonedge, [ setup(setup_database()),
                        cleanup(cleanup_database()),
                        fail
                    ]) :- 
    graph:is_edge(graph,[v1,v2]). 

test(is_edge_reverse, [ setup(setup_database()),
                        cleanup(cleanup_database()),
                        fail
                    ]) :- 
    graph:is_edge(graph,[v1,v0]). 

test(remove_edge_valid, [   setup(setup_database()), 
                            cleanup(cleanup_database()) 
                        ]) :- 
    graph:remove_edge(graph,[v0,v1]), 
    \+ graph:edge(graph,[v0,v1]). 

test(remove_edge_mulpi, [   setup(setup_database()),
                            cleanup(cleanup_database())
                        ]) :- 
    graph:remove_edge(graph,[v0,v2]), 
    \+ graph:edge(graph,[v0,v2]). 

test(remove_edge_nonedge, [ setup(setup_database()),
                            cleanup(cleanup_database()), 
                            fail
                        ]) :- 
    graph:remove_edge(graph,[v0,v3]). 

test(remove_edge_reverse, [ setup(setup_database()),
                            cleanup(cleanup_database()), 
                            fail
                        ]) :- 
    graph:remove_edge(graph,[v1,v0]). 

test(get_directed_edges_valid, [    setup(setup_database()), 
                                    cleanup(cleanup_database()),
                                    set(Edges == [[[v0,v1],[v0,v2]]])
                                ]) :- 
    get_directed_edges(graph,v0,Edges). 

test(get_directed_edges_empty, [    setup(setup_database()), 
                                    cleanup(cleanup_database()),
                                    set(Edges == [])
                                ]) :- 
    get_directed_edges(graph,v1,Edges).

test(get_directed_edges_invalid, [  setup(setup_database()), 
                                    cleanup(cleanup_database()),
                                    fail 
                                ]) :- 
    get_directed_edges(graph,v3,_). 

:- end_tests(graph).
