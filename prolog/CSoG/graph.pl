% graph.pl
/*  
    
    Author:        Yibin Zhao
    E-mail:        yibin.zhao@intel.com, yibin.joseph.zhao@gmail.com
    Copyright (C): 

*/

:- module(graph,
    [   insert_vertex/2,
        insert_vertices/2,
        is_vertex/2,
        remove_vertex/2,
        remove_vertex_force/2,
        remove_vertices/2,
        all_vertices/2,
        remove_all_vertices/1,
        insert_edge/2,
        insert_edges/2,
        is_edge/2,
        remove_edge/2,
        remove_edge_force/2,
        remove_edges/2,
        all_edges/2,
        remove_all_edges/1,
        get_directed_edges/3,
        remove_directed_edges/2,
        get_adjacent_vertices/3,
        set_vertex_data/3,
        get_vertex_data/3,
        graph_sources/2,
        graph_nonsources/2,
        graph_terminals/2,
        graph_nonterminals/2,
        acyclic/1,
        cycle_from_vertex/2,
        reachable/3,
        graph_is_null/1,
        induced_subgraph/2,
        induced_subgraph/3
    ]).

% :- meta_predicate. 

:- dynamic
    vertex/2, 
    edge/2.

insert_vertex(G,V) :- 
    atomic(G), atomic(V),
    asserta(vertex(G,V)).

insert_vertices(G,[]) :- atomic(G), !.
insert_vertices(G,[V|Vt]) :-
    insert_vertex(G,V),
    insert_vertices(G,Vt).

is_vertex(G,V) :- 
    vertex(G,V).

% TODO: check if there are edges from/to V
remove_vertex(G,V) :- 
    is_vertex(G,V), !, 
    retractall(vertex(G,V)).

remove_vertex_force(G,V) :- 
    atomic(G),atomic(V),
    retractall(vertex(G,V)).

remove_vertices(G,[]) :- atomic(G), !.
remove_vertices(G,[V|Vt]) :- 
    remove_vertex(G,V),
    remove_vertices(G,Vt).

all_vertices(G,Vs) :- 
    findall(V,is_vertex(G,V),Va),
    sort(Va,Vs).

remove_all_vertices(G) :-
    atomic(G),
    retractall(vertex(G,_)).

validate_edge(G,[S,T]) :- 
    atomic(G), atomic(S), atomic(T),
    is_vertex(G,S), !,
    is_vertex(G,T), !.

insert_edge(G,E) :- 
    validate_edge(G,E), !,
    assertz(edge(G,E)).

insert_edges(G,[]) :- atomic(G), !.
insert_edges(G,[E|Et]) :- 
    insert_edge(G,E),
    insert_edges(G,Et).

is_edge(G,E) :- 
    edge(G,E).

remove_edge(G,E) :- 
    is_edge(G,E), !,
    retractall(edge(G,E)). 

remove_edge_force(G,[U,V]) :- 
    atomic(G),atomic(U),atomic(V),
    retractall(edge(G,[U,V])).

remove_edges(G,[]) :- atomic(G), !.
remove_edges(G,[E|Et]) :- 
    remove_edge(G,E),
    remove_edges(G,Et).

all_edges(G,Es) :- 
    findall(E,is_edge(G,E),Ea),
    sort(Ea,Es).

remove_all_edges(G) :-
    atomic(G),
    retractall(edge(G,_)).

get_directed_edges(G,V,Es) :- 
    is_vertex(G,V), !, 
    findall([V,T],edge(G,[V,T]),Ea),
    sort(Ea,Es).

remove_directed_edges(G,V) :- 
    get_directed_edges(G,V,Es),
    remove_edges(G,Es).

get_adjacent_vertices(G,V,Ts) :- 
    is_vertex(G,V), !,
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

acyclic(G) :- 
    atomic(G),
    graph_sources(G,Ss), !, 
    (   Ss == [] 
    ->  fail
    ;   acyclic_(G,Ss)).

acyclic_(_,[]) :- !.
acyclic_(G,[S|St]) :- 
    \+ cycle_from_vertex(G,S),
    acyclic_(G,St).

cycle_from_vertex(G,V) :- 
    atomic(G), atomic(V),
    cycle_from_vertex_(G,[V],[]).

cycle_from_vertex_(_,[V|_],A) :- 
    member(V,A), !.

cycle_from_vertex_(G,[V|T],A) :- 
    get_adjacent_vertices(G,V,Vs),
    sort([V|A],Anew),
    append(Vs,T,VS),
    list_to_set(VS,VSnew),
    cycle_from_vertex_(G,VSnew,Anew). 

reachable(G,S,T) :-
    atomic(G),atomic(S),atomic(T),
    is_vertex(G,S), !, 
    is_vertex(G,T), !,
    reachable_(G,[S],[],T), !.

reachable(G,S,T) :- 
    atomic(G),atomic(S),var(T),
    is_vertex(G,S), !,
    reachable_(G,[S],[],T).

reachable_(_,_,[V|_],V).

reachable_(G,[V|VT],A,T) :- 
    get_adjacent_vertices(G,V,Vs),
    append(Vs,VT,VS),
    list_to_set(VS,VStmp),
    subtract(VStmp,A,VSnew),
    reachable_(G,VSnew,[V|A],T).

graph_is_null(G) :- 
    atomic(G),
    all_vertices(G, []),
    all_edges(G,[]).

induced_subgraph(G,Us) :-
    atomic(G),
    all_vertices(G,Vs), 
    subset(Us,Vs),
    subtract(Vs,Us,Ws),
    induced_subgraph_rev(G,Vs,Ws).

induced_subgraph_rev(_,_,[]) :- !.
induced_subgraph_rev(G,Vs,[V|T]) :- 
    induced_subgraph_rev_(G,V,Vs),
    remove_vertex(G,V),
    induced_subgraph_rev(G,Vs,T).

induced_subgraph_rev_(_,_,[]) :- !.
induced_subgraph_rev_(G,U,[V|T]) :-
    remove_edge_force(G,[U,V]),
    remove_edge_force(G,[V,U]),
    induced_subgraph_rev_(G,U,T).

induced_subgraph(G,Us,Gn) :- 
    atomic(G),atomic(Gn),
    graph_is_null(Gn),
    all_vertices(G,Vs),
    subset(Us,Vs),
    insert_vertices(Gn,Us),
    findall([U,V],(member(U,Us),member(V,Us),is_edge(G,[U,V])),Es),
    sort(Es,Enew),
    insert_edges(Gn,Enew).

