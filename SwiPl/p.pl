:-use_module(my_list). 

:-use_module(library(lists)).

:-use_module(my_meta).

:-[my_misc].

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
                             try_out(search(G,*,[N|L]),[2],NL,_)
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



queen(N) :-
	L :: list_gen(1,N),
	permutation(L,M),
	P :: list_mult(L,L),
	for_each(check(M,*),[2],P),write(M),nl.
	

test(A,B) :- ( A>B -> println([A,B]); true ).

test(X) :- number(X), !, println(['Number']).
test(X) :- var(X), !, println(['Free Var']).


test1(X):- nl,nl,try_out(user:edge(o,*),[2],[a,b,c,d],X), println(['Finally ',X]),nl.

test2:- edge(o,a), edge(o,b), edge(o,c), edge(o,d).


