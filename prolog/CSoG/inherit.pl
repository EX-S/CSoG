% inherit.pl
/*  
    
    Author:        Yibin Zhao
    E-mail:        yibin.zhao@intel.com, yibin.joseph.zhao@gmail.com
    Copyright (C): 

*/

:- module(inherit,
    []).

:- meta_predicate
    inherit_rule_update(2, +, +, -),
    inherit_value_update_(2, +, +, -).

compare_inherit(iht_nk,iht_k).
compare_inherit(iht_sk,iht_k).
compare_inherit(iht_k,iht_hk).
compare_inherit(X,Y) :- 
    compare_inherit(X,Z),
    compare_inherit(Z,Y).

compare_passdown(pad_nf,pad_f).
compare_passdown(pad_sf,pad_f).
compare_passdown(pad_f,pad_hf).
compare_passdown(X,Y) :-
    compare_passdown(X,Z),
    compare_passdown(Z,Y).

compare_inhNpad(iht_nk,pad_nf).
compare_inhNpad(iht_k,pad_f).
compare_inhNpad(iht_sk,pad_sf).
compare_inhNpad(iht_hk,pad_hf).
compare_inhNpad(Iht,Pad) :- 
    compare_inhNpad(Iht_even,Pad),
    !,
    compare_inherit(Iht,Iht_even).


inherit_rule_update(Comp, Self_p_curr, Parent_p, Self_p_new) :- 
    (   call(Comp, Self_p_curr, Parent_p) 
        ->  Self_p_new = Parent_p
        ;   Self_p_new = Self_p_curr
    ). 

inherit_value_update(Self_iht, Parent_pad, Self_v_curr, 
        Parent_v, Self_v_new) :- 
    inherit_value_update_(compare_inhNpad, Self_iht, Parent_pad, Self_v_curr, 
            Parent_v, Self_v_new).

inherit_value_update_(Comp, Self_iht, Parent_pad, Self_v_curr, Parent_v, 
        Self_v_new) :- 
    (   call(Comp, Self_p_curr, Parent_p)
        ->  Self_v_new = Self_v_curr
        ;   Self_v_new = Parent_v
    ).
