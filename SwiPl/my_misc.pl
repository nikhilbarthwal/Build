:-use_module(library(lists)).

println(L) :- ( L=[] -> nl ; L=[H|T], write(H), println(T) ).

:-op(700,xfx, <= ).
X <= Y :- X =< Y.

:-op(700,xfx, => ).
X => Y :- X >= Y.

:-op(700,xfx, :: ).
X :: G :- call(G,X).

:-op(700,xfx, in ).
X in L :- member(X,L).

