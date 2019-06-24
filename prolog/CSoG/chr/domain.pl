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

% general enum domain rules
domain(X,_,[]) <=> fail.
domain(X,_,[Y]) <=> X = Y.
domain(X,_,L) <=> nonvar(X) | memberchk(X,L).
domain(X,_,L1), domain(X,L2) <=> intersection(L1,L2,L3), domain(X,L3). 

% integer domain

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


