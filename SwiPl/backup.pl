:- op(300, fx, start).

:- op(800, xfx, --> ).

:- op(300, xfx, @@).
X @@ Y :-
	nonvar(X), ground(Y), !, assert(dataflow(X,Y)).

% TO BE DONE
%get_component(Name, Description, Slots) :-
%	( cosmos_component(Name, D, S) ->
%		D = Description,
%		S = Slots,
%	assert(cosmos_component(Name, Description, Slots)).

cosmos_print([]) :-
	nl, !.

cosmos_print([H|L]) :-
	write(H), cosmos_print(L).

cosmos_log(N, L) :-
	cosmos_settings(log, C),
	( N > C -> true; cosmos_print(L) ).

% Start all start tasks
cosmos_startup([]) :- !.
cosmos_startup([H|T]) :-
	% call(H),
	cosmos_log(3,[" Executing startup task: ",H]),
	cosmos_startup(T).

% Get all start tasks
cosmos_start :-
	bagof(X, start X, L),
	cosmos_log(3,[" Retrieved startup list: ", L]),
	\+ L = [],
	cosmos_log(3, L),
	cosmos_startup(L).

cosmos_goal :-
	assert(cosmos_settings(log,0)),
	cosmos_start,
	cosmos_run.




/*
Initialization:

1. Get List of all startup tasks - Cosmos_SL
2. Get all the Handler's List in update the info in Database
3. Get all the List events from each HAndler
4. For each event, create depenendies handlers - dependency(E,HL)

loop Execute:
	cosmos_ready_execute(X) is false, then exit with error
	execute X and remove from database
	if target is present then exit success exit
	else loop again Execute


cosmos_execute_ready(X) :-
	X is in Handler's List in Database
	M is HL excluding X
	L is the list of Variables in X
	for each l in L,
	for each m in M,
		dependency(l,Z)
		fail if member(m,Z)



You cannot execute handler untill all the dependent variables are present
Each predicate query is tabled for no repeats
Each handler should end with firing Events

*/
/*
 -------------------------------------------------------------------------------------

% Example of Rule for forward chaining
% nikhil(X) nikhil(Y)
% X > Y]  [NewX is X-Y, replace(nikhil(X), nikhil(NewX)) ].

*/


run :-
	Condition --> Action,
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


go :- halt.


