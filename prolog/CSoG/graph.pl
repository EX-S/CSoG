% graph.pl
/*  
    
    Author:        Yibin Zhao
    E-mail:        yibin.zhao@intel.com, yibin.joseph.zhao@gmail.com
    Copyright (C): 

*/

:- module(graph,
    [   insert_vertex/2,
        is_vertex/2,
        remove_vertex/2,
        insert_edge/2,
        is_edge/2,
        remove_edge/2,
        get_directed_edges/3,
        set_vertex_data/3,
        get_vertex_data/3
    ]).

% :- meta_predicate. 

:- dynamic
    vertex/2, 
    edge/2.

insert_vertex(G,V) :- 
    atomic(G), atomic(V),
    asserta(vertex(G,V)).

is_vertex(G,V) :- 
    vertex(G,V).

% TODO: check if there are edges from/to V
remove_vertex(G,V) :- 
    is_vertex(G,V), !, 
    retractall(vertex(G,V)).

validate_edge(G,[S,T]) :- 
    atomic(G), atomic(S), atomic(T),
    is_vertex(G,S), !,
    is_vertex(G,T), !.

insert_edge(G,E) :- 
    validate_edge(G,E), !,
    assertz(edge(G,E)).

is_edge(G,E) :- 
    edge(G,E).

remove_edge(G,E) :- 
    is_edge(G,E), !,
    retractall(edge(G,E)). 

get_directed_edges(G,V,Es) :- 
    is_vertex(G,V), 
    setof([V,T],edge(G,[V,T]),Es).

set_vertex_data(G,V,D) :- 
    is_vertex(G,V), 
    assertz(vertex_data(G,V,D)).

get_vertex_data(G,V,D) :- 
    vertex_data(G,V,D).


