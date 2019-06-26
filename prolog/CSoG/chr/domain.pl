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

%%%%Enum Domain%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

%-------------------------------------------------------------------------------


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


