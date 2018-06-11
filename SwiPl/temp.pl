permutations([],[]) :- !.

permutations(L,LL) :-
	size(L,N),
	list_gen(1,N,R),
	random(X,R),
	select(L,X,H), % Selecet Rth element in L and return as H
	delete(L,X,T), % Delete Rth element in L and return the resulting list a T
	permutations(T,TT),
	LL=[H|TT].


list_gen(X,Y,_) :- X>Y, !, fail.
list_gen(X,X,[X]) :- !.
list_gen(X,Y,L) :-
	XX=X+1,
	list_gen(XX,Y,T),
	L=[X|T].

list_gen(X,Y,L) :- Y>=X, !, list_gen_(X,Y,[],L).

list_gen_(X,X,L1,L1).
list_gen_(X,Y,L1,L2) :-
	YY=Y-1,
	list_gen_(X,YY,[Y|L1],L2).


size_([],N,N).
size_([_|T],N0,N) :- N1=N0+1, size_(T,N1,N).

size(L,N) :-
	size_(L,0,N).

