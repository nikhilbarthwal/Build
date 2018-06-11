
% PROJECT STRUCTURE


GLOBAL PREDICATES
	determ interpret_project(string, statementslist, pstructure) - (i,i,o)

PREDICATES
	determ read_headline(statementslist, statementslist, identifier)	- (i,o,o)
	determ read_modules(statementslist, statementslist, text)			- (i,o,o)
	determ read_modules0(statementslist, statementslist, text)			- (i,o,o)
	determ read_target(statementslist, statementslist, target)			- (i,o,o)
	determ read_targets(statementslist, statementslist, targetlist)		- (i,o,o)
	determ read_module(statementslist, statementslist, string)			- (i,o,o)
	read_target_statement(statementslist, statementslist,
		statementslist,statementslist, statementslist,statementslist)	- (i,o,i,i,o,o)
		
CLAUSES
	interpret_project(F, S, P) :-
		read_headline(S, S0, N),
		read_modules(S0, S1, M),
		read_targets(S1, _, T),
		P = project(N, F, M, T), !.
	
	interpret_project(F, _, _) :-
		add_error1(F, "Unable to understand project file"), 
		fail.
	
	read_headline(X, T, S) :-
		X=[H|T],
		H=p(_, _, project(S)), !.
	
	read_headline(X, Y, _) :-
		Y = X,
		X = [H|_],
		H = p(F, N, _),
		add_error2(F, N, "Expecting Project Defination here"), 
		fail.
		
	read_modules(S0, S2, L) :-
		S0 = [H1|S1],
		H1 = p(_, _, begin ),
		read_modules0(S1, S2, L).
	
	read_modules0(S0, S1, []) :-
		S0 = [H|S1],
		H = p(_, _, end), !.
		
	read_modules0(S0, S2, [H|T]) :-
		read_module(S0, S1, H), !,
		read_modules0(S1, S2, T).
		
	read_module(Z, Y, S) :-
		Z=[H|Y],
		H = p(_, _, pinclude(str(S,_))), !. % TBD: Replace by Module reader
		
	read_module(X, Y, S) :-
		X=Y,
		X = [H|_],
		H = p(F, N, _), !,
		add_error2(F, N, "Expecting Include definations here"), 
		S="dummy", !.

	read_targets([], [], []) :- !.
	read_targets(X, Z, [H|T]) :-
		!, read_target(X, Y, H),
		read_targets(Y, Z, T).

	read_target(S0, S2, Z) :-
		S0 = [H1, H2|S1],
		H1 = p(_, _, target(N)),
		H2 = p(_, _, begin ),
		read_target_statement(S1, S2, [], [], A, R), !,
		Z = target(N, A, R).
	
	read_target(S, _, _) :-
		S = [H|_],
		H = p(F, N, _),
		add_error2(F, N, "Unable to read the target block"), !,
		fail.

	read_target_statement(L, T, A, R, A, R) :-
		L = [H|T],
		H = p(_, _, end), !.
	
	read_target_statement(L0, L2, A0, R0, A2, R2) :-
		L0 = [H|L1],
		H = p(_, _, set(_, _)),
		A1 = [H|A0],
		read_target_statement(L1, L2, A1, R0, A2, R2), !.
	
	read_target_statement(L0, L2, A0, R0, A2, R2) :-
		L0 = [H|L1],
		H = p(_, _, report(_, _, _)),
		R1 = [H|R0],
		read_target_statement(L1, L2, A0, R1, A2, R2), !.

	read_target_statement(L, L, A, R, A, R) :-
		L = [H|_],
		H = p(F, N, _), !,
		add_error2(F, N, "Expecting assigment/report variable"), !.
		
		
	
	
		
	
	
	
	
		
	
	
	
		