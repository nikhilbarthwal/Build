:-module(mylist,[list_init/3,array/3,list_verify/2,list_replace/4,list_replace_all/4,list_substitute/4,list_extract/4,	list_get/3,
                 list_put/4, list_include/4, list_exclude/4, list_gen/3, list_gen/4, list_multiplex/3, list_mult/2, list_mult/3]).

:-use_module(library(lists)). 

list_init_tmp(_,0,L,L).
list_init_tmp(C,N,L1,L3) :- L2=[C|L1], NN is N-1, list_init_tmp(C,NN,L2,L3).
list_init(C,N,L) :- integer(N), N>=0, var(L), list_init_tmp(C,N,[],L), !.

array_init_tmp(C,[H],L) :- list_init(C,H,L).
array_init_tmp(C,[H|T],L) :- array_init_tmp(C,T,LL), list_init(LL,H,L).
array(C,L,LL) :- is_list(L), var(LL), \+ L=[], array_init_tmp(C,L,LL), !.

list_verify(_,[]) :- !.
list_verify(L,[H|T]) :- is_list(L), length(L,N), H =< N, nth1(H,L,LL), list_verify(LL,T).

list_replace_tmp(X,1,[_|T],[X|T]).
list_replace_tmp(X,N,[H|T],L) :- NN is N-1, list_replace_tmp(X,NN,T,TT), L = [H|TT].
list_replace(X,N,L1,L2) :- length(L1,P), integer(N), P>0, N>0, P>=N, list_replace_tmp(X,N,L1,L2), !.

list_replace_all(_,[],L,L) :- !.
list_replace_all(X,NL,L1,L2) :- NL=[N|T], list_replace(X,N,L1,LL), list_replace_all(X,T,LL,L2).

list_substitute(_,_,[],[]) :- !.
list_substitute(S1,S2,[H1|T1],[H2|T2]) :- ( H1 = S1 -> H2 = S2 ; H2 = H1 ), list_substitute(S1,S2,T1,T2).

list_extract_tmp(Y,Y,[H|_],TT,[H|TT]).
list_extract_tmp(X,Y,[H|T],TT,L) :- XX is X+1, LL=[H|TT], list_extract_tmp(XX,Y,[H|T],LL,L).
list_extract(X,Y,L1,L2) :- length(L1,N), X>0, Y>0, N>=X, N>=Y, Y>=X, is_list(L1), list_extract_tmp(X,Y,L1,[],L), reverse(L,L2), !.

list_get(L,[],L) :- !.
list_get(L,[H|T],Z) :- length(L,N), integer(H), H =< N, is_list(L), nth1(H,L,LL), list_get(LL,T,Z).

list_put(X,[],_,X) :- !.
list_put(X,[H|T],L1,L2) :- length(L1,N), integer(H), H =< N, is_list(L1), nth1(H,L1,LL1), list_put(X,T,LL1,LL2), list_replace(LL2,H,L1,L2).

list_include_tmp(_,_,[],L,L).
list_include_tmp(G,M,[H|T],L1,L3) :- G =.. [GH|GT1], list_replace_all(H,M,GT1,GT2), GG =.. [GH|GT2],( call(GG) -> L2 = [H|L1] ; L2 = L1 ), list_include_tmp(G,M,T,L2,L3).
list_include(G,M,L,LL) :-  is_list(L),  list_include_tmp(G,M,L,[],LLL), reverse(LLL,LL), !.

list_exclude_tmp(_,_,[],L,L).
list_exclude_tmp(G,M,[H|T],L1,L3) :- G =.. [GH|GT1], list_replace_all(H,M,GT1,GT2), GG =.. [GH|GT2], ( call(GG) -> L2 = L1 ; L2 = [H|L1] ), list_exclude_tmp(G,M,T,L2,L3).
list_exclude(G,M,L,LL) :- is_list(L), list_exclude_tmp(G,M,L,[],LLL), reverse(LLL,LL), !.

list_gen_tmp1(X,Y,L,L) :- X > Y.
list_gen_tmp1(X,Y,T,L) :- YY is Y-1, TT=[Y|T], list_gen_tmp1(X,YY,TT,L).

list_gen_tmp2(X,Y,_,L,L) :- X > Y.
list_gen_tmp2(X,Y,Z,T,L) :- XX is X + Z, TT=[X|T], list_gen_tmp2(XX,Y,Z,TT,L).

list_gen_tmp3([],[],L,L).
list_gen_tmp3([H1|T1],[H2|T2],R,L) :- list_gen(H1,H2,H), TT=[H|R], list_gen_tmp3(T1,T2,TT,L).

list_gen_tmp4([],[],[],L,L).
list_gen_tmp4([H1|T1],[H2|T2],[H3|T3],R,L) :- list_gen(H1,H2,H3,H), TT=[H|R], list_gen_tmp4(T1,T2,T3,TT,L).

list_gen(X,Y,L) :- integer(X), integer(Y), Y >= X, !, list_gen_tmp1(X,Y,[],L), !.
list_gen(X,Y,L) :- is_list(X), is_list(Y), length(X,N), length(Y,N), !, list_gen_tmp3(X,Y,[],LL), reverse(L,LL), !.
list_gen(X,Y,Z,L) :- integer(X), integer(Y), integer(Z), Y >= X, !, list_gen_tmp2(X,Y,Z,[],LL), reverse(L,LL), !.
list_gen(X,Y,Z,L) :- is_list(X), is_list(Y), is_list(Z), length(X,N), length(Y,N), length(Z,N), !, list_gen_tmp4(X,Y,Z,[],LL), reverse(L,LL), !.

list_multiplex_tmp([],[],[]).
list_multiplex_tmp([H1|T1],[H2|T2],[[H1,H2]|PP]) :- list_multiplex(T1,T2,PP).
list_multiplex(L1,L2,P) :- is_list(L1), is_list(L2), length(L1,N), length(L2,N), list_multiplex_tmp(L1,L2,P), !.
	
list_mult_tmp1(_,[],[]).
list_mult_tmp1(X,[H|T],[[X,H]|TT]) :- list_mult_tmp1(X,T,TT).
list_mult_tmp2([],_,[]).
list_mult_tmp2([H|T],L,LL) :- list_mult_tmp2(T,L,LL1), list_mult_tmp1(H,L,LL2), append(LL2,LL1,LL).
list_mult_tmp3(_,[],[]) :- !.
list_mult_tmp3(X,[H|T],L) :- L1=[X|H], list_mult_tmp3(X,T,L2), L=[L1|L2].
list_mult_tmp4([],_,[]).
list_mult_tmp4([H|T],L,LL) :- list_mult_tmp3(H,L,L1), list_mult_tmp4(T,L,L2), append(L1,L2,LL).
list_mult(L1,L2,L) :- is_list(L1), is_list(L2), list_mult_tmp2(L1,L2,L), !.
list_mult([X],X) :- !.
list_mult([X,Y],Z) :- list_mult(X,Y,Z), !.
list_mult(LL,L):- LL=[HL|TL], list_mult(TL,L1), list_mult_tmp4(HL,L1,L), !.


