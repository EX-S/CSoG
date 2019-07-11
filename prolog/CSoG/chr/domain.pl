% domain.pl
/*  
    
    Author:        Yibin Zhao
    E-mail:        yibin.zhao@intel.com, yibin.joseph.zhao@gmail.com
    Copyright (C): 

*/

:- module(domain, 
    [
    ]).

:- use_module(library(chr)).

% :- meta_predicate.

% handler domain.

% for domain constraints
% operator(700,xfx,'::').
% operator(600,xfx,'..').
% operator(600,xfx,':').  % clash with module operator?

% for inequality constraints
operator(700,xfx,lt).
operator(700,xfx,le).
operator(700,xfx,gt).
operator(700,xfx,ge).
operator(700,xfx,ne).


%%% Enum Domain %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- chr_constraint domain(?any,+dom_type,+list(any)).
:- chr_type list(T) ---> [] ; [T|list(T)].
:- chr_type dom_type ---> int; dense_int; float; number; natural; any.

%---general enum domain rules---------------------------------------------------

domain(X,_,[]) <=> fail.
domain(X,_,[Y]) <=> X = Y.
domain(X,_,D) <=> nonvar(X) | memberchk(X,D).
domain(X,T,[H|L]) <=> list_to_ord_set([H|L],SL), SL\==[H|L] | domain(X,T,SL).
domain(X,T,D1), domain(X,T,D2) <=> ord_intersection(D1,D2,D3), domain(X,T,D3). 
domain(X,S,D1), domain(X,T,D2) <=> S\==T | fail.

% use one domain for a list of variables
domain([X|L],T,D) <=> makedom([X|L],T,D). 
makedom([],_,D) :- true.
makedom([X|L],T,D) :- 
    nonvar(L),
    domain(X,T,D),
    makedom(L,T,D).

% intersection of domains for the same variable
domain(X,T,[A1|L1]) \ domain(X,T,[A2|L2]) <=>
    ord_intersection([A1|L1],[A2|L2],L), L \== [A2|L2] | domain(X,T,L).

%-------------------------------------------------------------------------------

%---number enum domain rules----------------------------------------------------

% allow arithmetic expressions in domain
domain(X,T,[H|L]) <=> 
        memberchk(T,[int,float,number,natural]), member(X,[H|L]), \+ number(X) | 
        eval_list([H|L],L1), list_to_ord_set(L1,L2), domain(X,T,L2). 

eval_list([],[]).
eval_list([X|L1],[Y|L2]) :- 
    Y is X, 
    eval_list(L1,L2).

% TODO: propagate int, float, natural, number to general number constraints.


%-------------------------------------------------------------------------------


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Inequality constraints %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- chr_constraint lt(?dom_type,?dom_type),
        le(?dom_type,?dom_type),
        ne(?dom_type,?dom_type).


%---general number constraint rules---------------------------------------------

A gt B :- B lt A.
A ge B :- B le A.
% simplifications
A lt A <=> fail.
A le A <=> true.
A ne A <=> fail.
A lt B, B lt A <=> fail.
A le B,B le A <=> A=B.
A ne B \ B ne A <=> true.
% for number domain, allow arithmetic expressions in the arguments
A lt B <=> ground(A),\+ number(A) | A1 is A, A1 lt B.
B lt A <=> ground(A),\+ number(A) | A1 is A, B lt A1.
A le B <=> ground(A),\+ number(A) | A1 is A, A1 le B.
B le A <=> ground(A),\+ number(A) | A1 is A, B le A1.
A ne B <=> ground(A),\+ number(A) | A1 is A, A1 ne B.
B ne A <=> ground(A),\+ number(A) | A1 is A, B ne A1.
% use built-ins to solve the predicates if arguments are known
A lt B <=> ground(A),ground(B) | ((domain(A,number,_), domain(B,number,_)) -> A < B ; A @< B).
A le B <=> ground(A),ground(B) | ((domain(A,number,_), domain(B,number,_)) -> A =< B ; A @=< B).
A ne B <=> ground(A),ground(B) | ((domain(A,number,_), domain(B,number,_)) -> A =\= B ; A \== B).
% TODO: propagate int, float, natural, number

%-------------------------------------------------------------------------------

%---cases for enum domains------------------------------------------------------

domain(X,T,[H|L]) \ X ne Y <=> ground(Y), remove(Y,[H|L],L1) | domain(X,T,L1).
domain(X,T,[H|L]) \ Y ne X <=> ground(Y), remove(Y,[H|L],L1) | domain(X,T,L1).

domain(X,T,[H|L]) \ Y le X ==> 
    ground(Y), remove_lower(Y,[H|L],L1) | domain(X,T,L1).
domain(X,T,[H|L]) \ X le Y ==> 
    ground(Y), remove_higher(Y,[H|L],L1) | domain(X,T,L1).
domain(X,T,[H|L]) \ Y lt X ==> 
    ground(Y), remove_lower(Y,[H|L],L1), remove(Y,L1,L2) | domain(X,T,L1).
domain(X,T,[H|L]) \ X lt Y ==> 
    ground(Y), remove_higher(Y,[H|L],L1) | domain(X,T,L1).

%-------------------------------------------------------------------------------


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Auxiliary predicates %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

delete(X, [X|Xs], Xs).
delete(Y, [X|Xs], [X|Xt]) :-
    delete(Y, Xs, Xt).

remove(A,B,C) :- 
    delete(A,B,C) -> true ; B=C. 

remove_list(_,[],T):- !, T=[].
remove_list([],S,T):- S=T.
remove_list([X|R],[Y|S],T):- remove(X,[Y|S],S1),remove_list(R,S1,T).

remove_lower(_,[],L1):- !, L1=[].
remove_lower(Min,[X|L],L1):-
    X@<Min,
    !,
    remove_lower(Min,L,L1).
remove_lower(Min,[X|L],[X|L1]):-
    remove_lower(Min,L,L1).

remove_higher(_,[],L1):- !, L1=[].
remove_higher(Max,[X|L],L1):-
    X@>Max,
    !,
    remove_higher(Max,L,L1).
remove_higher(Max,[X|L],[X|L1]):-
    remove_higher(Max,L,L1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
