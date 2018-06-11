% Check Variables, Linking etc.
% Enhance Loop mult funtion

:- set_prolog_flag(optimise, true).
:- set_prolog_flag(debug, false).
:- set_prolog_flag(last_call_optimisation, true).

:- ignore(retractall(nikhil_var(_,_))), ignore(retractall(nikhil_link(_,_,_))), asserta(nikhil_var(default,default)), asserta(nikhil_link(default,default,default)).

println(L) :- ( L=[] -> nl ; L=[H|T], write(H), println(T) ).

list_size_tmp([],N,N).
list_size_tmp([_|T],I,N) :- J is I+1, list_size_tmp(T,J,N).
list_size(L,N) :- list_size_tmp(L,0,N), !.

list_init_tmp(_,0,L,L).
list_init_tmp(C,N,L1,L3) :- L2=[C|L1], NN is N-1, list_init_tmp(C,NN,L2,L3).
list_init(C,N,L) :- integer(N), N>=0, var(L), list_init_tmp(C,N,[],L), !.

array_init_tmp(C,[H],L) :- list_init(C,H,L).
array_init_tmp(C,[H|T],L) :- array_init_tmp(C,T,LL), list_init(LL,H,L).
array_init(C,L,LL) :- is_list(L), var(LL), \+ L=[], array_init_tmp(C,L,LL), !.

list_verify(_,[]) :- !.
list_verify(L,[H|T]) :- is_list(L), list_size(L,N), H =< N, nth1(H,L,LL), list_verify(LL,T).

list_replace_tmp(X,1,[_|T],[X|T]).
list_replace_tmp(X,N,[H|T],L) :- NN is N-1, list_replace_tmp(X,NN,T,TT), L = [H|TT].
list_replace(X,N,L1,L2) :- list_size(L1,P), integer(N), P>0, N>0, P>=N, list_replace_tmp(X,N,L1,L2), !.

list_replace_all(_,[],L,L) :- !.
list_replace_all(X,NL,L1,L2) :- NL=[N|T], list_replace(X,N,L1,LL), list_replace_all(X,T,LL,L2).

list_substitute(_,_,[],[]) :- !.
list_substitute(S1,S2,[H1|T1],[H2|T2]) :- ( H1 = S1 -> H2 = S2 ; H2 = H1 ), list_substitute(S1,S2,T1,T2).

list_extract_tmp(Y,Y,[H|_],TT,[H|TT]).
list_extract_tmp(X,Y,[H|T],TT,L) :- XX is X+1, LL=[H|TT], list_extract_tmp(XX,Y,[H|T],LL,L).
list_extract(X,Y,L1,L2) :- list_size(L1,N), X>0, Y>0, N>=X, N>=Y, Y>=X, is_list(L1), list_extract_tmp(X,Y,L1,[],L), reverse(L,L2), !.

list_get(L,[],L) :- !.
list_get(L,[H|T],Z) :- list_size(L,N), integer(H), H =< N, is_list(L), nth1(H,L,LL), list_get(LL,T,Z).

list_put(X,[],_,X) :- !.
list_put(X,[H|T],L1,L2) :- list_size(L1,N), integer(H), H =< N, is_list(L1), nth1(H,L1,LL1), list_put(X,T,LL1,LL2), list_replace(LL2,H,L1,L2).

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
list_gen(X,Y,L) :- is_list(X), is_list(Y), list_size(X,N), list_size(Y,N), !, list_gen_tmp3(X,Y,[],LL), reverse(L,LL), !.
list_gen(X,Y,Z,L) :- integer(X), integer(Y), integer(Z), Y >= X, !, list_gen_tmp2(X,Y,Z,[],LL), reverse(L,LL), !.
list_gen(X,Y,Z,L) :- is_list(X), is_list(Y), is_list(Z), list_size(X,N), list_size(Y,N), list_size(Z,N), !, list_gen_tmp4(X,Y,Z,[],LL), reverse(L,LL), !.

list_multiplex_tmp([],[],[]).
list_multiplex_tmp([H1|T1],[H2|T2],[[H1,H2]|PP]) :- list_multiplex(T1,T2,PP).
list_multiplex(L1,L2,P) :- is_list(L1), is_list(L2), list_size(L1,N), list_size(L2,N), list_multiplex_tmp(L1,L2,P), !.
	
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

for_each(_,_,[]) :- !.
for_each(G,M,[H|T]) :- G =.. [GH|GT1], list_replace_all(H,M,GT1,GT2), GG =.. [GH|GT2], !, call(GG), for_each(G,M,T).

try_out(G,M,L,X) :- \+ var(G), \+ var(M), \+ var(L), G =.. [H|T], !, member(X,L), list_replace_all(X,M,T,TT), GG =.. [H|TT], call(GG).

var_del(P) :- ignore(retractall(nikhil_var(P,_))), ignore(retractall(nikhil_link(P,_,_))), ignore(retractall(nikhil_link(_,P,_))), !.

var_set(P,Z) :-  \+ var(Z), P =.. [H1|T1],
	( var_resolve(H1,T1,H2,T2) -> var_get(H2,V), var_del(H2), list_put(Z,T2,V,VV), var_set(H2,VV) ; T1=[], asserta(nikhil_var(H1,Z)) ).

var_link(P,H,T) :- var_resolve(H,T,HH,TT), nikhil_var(HH,V), list_verify(V,TT), var_del(P), asserta(nikhil_link(P,HH,TT)).

var_resolve(H1,T1,H1,T1) :-  nikhil_var(H1,V), list_verify(V,T1), !.
var_resolve(H1,T1,H2,T2) :-  nikhil_link(H1,H2,T), append(T,T1,T2), !.

var_get(P,V) :- P =.. [H|T], var_resolve(H,T,HH,TT), nikhil_var(HH,VV), list_get(VV,TT,V), !.

var_exists(P) :- P =.. [H|T], var_resolve(H,T,_,_), !.

clean :- ignore(retractall(nikhil_var(_,_))), ignore(retractall(nikhil_link(_,_,_))), asserta(nikhil_var(default,default)), asserta(nikhil_link(default,default,default)).


:-op(700,fx, ~ ).
~ X :- var_del(X).

:-op(700,xfx, <= ).
X <= Y :- X =< Y.

:-op(700,xfx, => ).
X => Y :- X >= Y.

:-op(700,xfx, :: ).
X :: G :- call(G,X).

:-op(700,xfx, in ).
X in L :- member(X,L).

:-op(700,xfx, <- ).
X <- P :- var_get(P,X), !.

:-op(700,xfx, := ).
P := Z :- \+ var(Z), var_set(P,Z), !.

:-op(700,xfx, += ).
P += Z :- \+ var(Z), var_get(P,V), number(V), ZZ is V + Z, P =..[H|T], var_get(H,X), list_put(ZZ,T,X,Y), var_set(H,Y), !.
        
:-op(700,xfx, -= ).
P -= Z :- \+ var(Z), var_get(P,V), number(V), ZZ is V - Z, P =..[H|T], var_get(H,X), list_put(ZZ,T,X,Y), var_set(H,Y), !.

:-op(700,xfx, *= ).
P *= Z :- \+ var(Z), var_get(P,V), number(V), ZZ is V * Z, P =..[H|T], var_get(H,X), list_put(ZZ,T,X,Y), var_set(H,Y), !.

:-op(700,xfx, /= ).
P /= Z :- \+ var(Z), var_get(P,V), number(V), ZZ is V / Z, P =..[H|T], var_get(H,X), list_put(ZZ,T,X,Y), var_set(H,Y), !.

:-op(900,xfx, <-> ).
P <-> G :- G =.. [H|T], var_link(P,H,T), !.

 /*
 
 do_loop
 while_loop
 for_loop
 */

% -------------------------------------------------------------------------------

lnk(X,Y) :-
	( X = o , Y = a) ;
	( X = o , Y = b) ;
	( X = o , Y = c) ;
	( X = o , Y = d) ;
	( X = a , Y = e) ;
	( X = a , Y = f) ;
	( X = b , Y = g) ;
	( X = b , Y = h) ;
	( X = c , Y = i) ;
	( X = d , Y = j).
	

edge(X,Y) :- lnk(X,Y) ; lnk(Y,X).

add(H,T) :- ( dis(H,_) -> true ; asserta(dis(H,[H|T])) ).

search(G,N,L) :-
              bagof(NN,edge(N,NN),LL),
              NL :: list_exclude(dis(*,_),[1],LL),
              not(NL=[]),
              ( G in NL ->
                             add(G,[N|L])
              ;
                             for_each(add(*,[N|L]),[1],NL),!,
                             try_out(search(G,*,[N|L]),[2],NL)
              ).

find(Y,X,L) :-
          ignore(retractall(dis(_,_))),
          ( X=Y -> L=[X]
          ;
                asserta(dis(X,[X])),
                ( search(Y,X,[]) -> dis(Y,L); L=[] ),
                ignore(retractall(dis(_,_))),
                not(L=[])
          ).

seperate(N1,M1,N2,M2) :-
                  D1 is abs(N1 - N2),
                  D2 is abs(M1 - M2),
                  N1 =\= N2,
                  M1 =\= M2,
                  D1 =\= D2.
 

check(L,[N1,N2]) :-
	( N2 >= N1 -> true ;
		M1 :: nth1(N1,L),
		M2 :: nth1(N2,L),
		seperate(N1,M1,N2,M2)
	).


display_line(N,X) :-
	L1 :: list_init(' ',N),
	L2 :: list_replace('X',X,L1),
	L :: append(['|'],L2,['|']),
	println(L).

display1(N,M) :-
	L :: list_init('-',N),
	LL :: append([['+'],L,['+']]),
	nl,
	println(LL),
	for_each(display(N,*),[2],M),
	println(LL),
	nl, !.

	
queen(N) :-
	L :: list_gen(1,N),
	permutation(L,M),
	P :: list_mult1(L,L),
	for_each(check(M,*),[2],P),
	display1(N,M), nl.
	

test(A,B) :- ( A>B -> println([A,B]); true ).

test(X) :- number(X), !, println(['Number']).
test(X) :- var(X), !, println(['Free Var']).



display_list([]).
display_list([H|T]) :- write(H), nl, display_list(T).



