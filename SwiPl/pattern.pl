:- op(800,xfx, ---> ).

% An initial Database
init :-
	asserta(nikhil(25)),
	asserta(nikhil(10)),
	asserta(nikhil(15)),
	asserta(nikhil(30)).

[nikhil(X), nikhil(Y), X > Y] ---> [NewX is X-Y, replace(nikhil(X), nikhil(NewX)) ].
[nikhil(X)] ---> [ write(X), stop ].

run :-
	Condition ---> Action,
	test(Condition),
	execute(Action).

test([]).
test([First | Rest] ) :-
	call(First),
	test(Rest).

execute([stop]) :- !.
execute([]) :- run.

execute([A|B]) :-
	call(A),
	execute(B).

replace(A,B) :-
	retract(A),
	!,
	asserta(B).


go:-
	init,
	run.

