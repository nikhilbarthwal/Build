:- op(300, fx, start).

:- op(800, xfx, ---> ).

:- op(300, xfx, <-).
Z <- X :- println([Z,' throws ', X]).

println([]) :-
	nl, !.

println([H|L]) :-
	write(H), println(L).

cosmos_log(L) :-
	write('Cosmos: '),
	println(L).

process :-
	X ---> Y,
	cosmos_log(['Handler: ', X, ' / List: ',Y]),
	fail.

process :-
	!.

seperate( (X,Y) ) :- !,	seperate(X), seperate(Y).

seperate(Z) :- 	!, write(Z), nl, nl.

process2(L) :-
	clause(L,B),
	seperate(B).


init :-
	asserta(cosmos2([],[])),
	asserta(cosmos3([],[],[])),
	findall(X, start X, L),
	assertz(cosmos2(start_list,L)),
	cosmos_log(['Start List is detected as: ',L]),
	process,
	process2(nikhil3),
	nikhil3.

run :- init.
