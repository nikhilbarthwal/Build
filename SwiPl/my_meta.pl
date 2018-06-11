:-module(my_meta,[for_each/3, try_out/4]).

:- meta_predicate for_each(:,+,+), try_out(:,+,+,?).

:-use_module(my_list). 

for_each(_,_,[]) :- !.
for_each(G,M,[H|T]) :- G =.. [:,U,G1], G1 =.. [GH|GT1], list_replace_all(H,M,GT1,GT2), G2 =.. [GH|GT2], !, GG =.. [:,U,G2], call(GG), for_each(G,M,T).

try_out(G,M,L,X) :- \+ var(G), \+ var(M), \+ var(L), G =.. [:,U,G1], G1 =.. [H|T], !, list_member(L,X), list_replace_all(X,M,T,TT), G2 =.. [H|TT], GG =.. [:,U,G2], call(GG).

