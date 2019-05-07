% inherit.plt
/*  
    
    Author:        Yibin Zhao
    E-mail:        yibin.zhao@intel.com, yibin.joseph.zhao@gmail.com
    Copyright (C): 

*/

:- begin_tests(inherit).

:- load_files('../CSoG/inherit', [imports(all)]).

test(compare_inherit_fact) :- 
    inherit:compare_inherit(iht_nk,iht_k).

test(compare_inherit_transtive, [nondet]) :- 
    inherit:compare_inherit(iht_nk,iht_hk),
    inherit:compare_inherit(iht_sk,iht_hk).

test(compare_passdown_fact, [nondet]) :- 
    inherit:compare_passdown(pad_nf,pad_f).

test(compare_passdown_transitive, [nondet]) :- 
    inherit:compare_passdown(pad_sf,pad_hf),
    inherit:compare_passdown(pad_nf,pad_hf).

test(compare_inhNpad_fact, [nondet]) :- 
    inherit:compare_inhNpad(iht_sk,pad_sf).

test(compare_inhNpad_transitive, [nondet]) :- 
    inherit:compare_inhNpad(iht_sk,pad_f),
    inherit:compare_inhNpad(iht_nk,pad_hf).

:- end_tests(inherit).
