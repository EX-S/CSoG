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
        all_vertices/2,
        insert_edge/2,
        is_edge/2,
        remove_edge/2,
        all_edges/2,
        get_directed_edges/3,
        get_adjacent_vertices/3,
        set_vertex_data/3,
        get_vertex_data/3,
        graph_sources/2,
        graph_nonsources/2,
        graph_terminals/2,
        graph_nonterminals/2
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

all_vertices(G,Vs) :- 
    findall(V,is_vertex(G,V),Va),
    sort(Va,Vs).

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

all_edges(G,Es) :- 
    findall(E,is_edge(G,E),Ea),
    sort(Ea,Es).

get_directed_edges(G,V,Es) :- 
    is_vertex(G,V), !, 
    findall([V,T],edge(G,[V,T]),Ea),
    sort(Ea,Es).

get_adjacent_vertices(G,V,Ts) :- 
    is_vertex(G,V),
    findall(T,edge(G,[V,T]),Ta),
    sort(Ta,Ts).

set_vertex_data(G,V,D) :- 
    is_vertex(G,V), 
    assertz(vertex_data(G,V,D)).

get_vertex_data(G,V,D) :- 
    vertex_data(G,V,D).


graph_sources(G,Ss) :-
    atomic(G),
    all_vertices(G,Vs),
    graph_nonsources(G,NSs),
    ord_subtract(Vs,NSs,Ss).

graph_nonsources(G,NSs) :- 
    atomic(G),
    all_vertices(G,Vs),
    graph_nonsources_(G,Vs,[],NSs). 

graph_nonsources_(_,[],A,NSs) :- sort(A,NSs), !. 
graph_nonsources_(G,[V|Vt],A,NSs) :- 
    get_adjacent_vertices(G,V,Ts), !,
    append([Ts,A],Aappd),
    sort(Aappd,Anew),
    graph_nonsources_(G,Vt,Anew,NSs).

graph_terminals(G,Ts) :- 
    atomic(G),
    all_vertices(G,Vs), 
    graph_terminals_(G,Vs,[],Ts). 

graph_terminals_(_,[],A,Ts) :- sort(A,Ts), !.
graph_terminals_(G,[V|Vt],A,Ts) :- 
    get_adjacent_vertices(G,V,Vs), !,
    (   Vs == []
        ->  Anew = [V|A]
        ;   Anew = A
    ),
    graph_terminals_(G,Vt,Anew,Ts).

graph_nonterminals(G,NTs) :-
    atomic(G),
    all_vertices(G,Vs),
    graph_nonsources(G,Ts),
    ord_subtract(Vs,Ts,NTs).
