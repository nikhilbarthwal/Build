% All DB related functions!

GLOBAL DATABASE
	err1(string filename, string message)
	err2(string filename, integer line, string message)
	err3(string filename, integer line, integer position, string message)
	source(string filename, integer line, string text)
	

GLOBAL PREDICATES
	determ error
	%determ print_error
	determ add_error1(string, string) - (i,i)
	determ add_error2(string, integer, string) - (i,i,i)
	determ add_error3(string, integer, integer, string) - (i,i,i,i)

CLAUSES
	error :- err1(_, _), !.
	error :- err2(_, _, _), !.
	error :- err3(_, _, _, _), !.
			
	%print_error.

	add_error1(S, M) :-
		write(S," : ",M), nl,
		assert(err1(S, M)).
		
	add_error2(S, N, M) :-
		source(S, N, Z), !,
		write(S," (",N,") ",Z," : ",M), nl,
		assert(err2(S, N, M)).
		
	add_error3(S, N, P, M) :-
		source(S, N, Z), !,
		write(S," (",N,",",P,") ",Z," : ",M), nl,
		assert(err3(S, N, P, M)).
