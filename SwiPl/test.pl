% ***** Prayog Test File *****

:- use_module(utils).
:- use_module(tokenize).
:- use_module(parser).
:- use_module(process).
:- use_module(library(lists)).


test :- test1, test2, test3.

% ******************** TEST 1: Test Tokenizer ******************** 
test1 :-
	TestFile = 'test1.txt',
	reset_error,
	read_file(TestFile, FileCodes),!, 
	tokenize_file(TestFile, FileCodes, FileTokens), !,
	( merge_check(FileTokens) ->
		merge_join(FileTokens, Tokens),
		debug(Tokens)
	;
		debug(Tokens),
		writeln(' MERGE CHECK FAILS')
	),
	
	nb_getval(error, Errors),
	nl, write(Errors),
	writeln(' Errors encountered'), nl,
	reset_error.

% ******************** TEST 2: Test Elements ******************** 
test2 :-
	TestFile = 'test2.txt',
	reset_error,
	read_file(TestFile, FileCodes),!, 
	tokenize_file(TestFile, FileCodes, FileTokens), !,
	( merge_check(FileTokens) ->
		merge_join(FileTokens, Tokens),
		test2(Tokens)	
	;
		true
	),
	nb_getval(error, Errors),
	nl, write(Errors),
	writeln(' Errors encountered'), nl,
	reset_error.

	% ----- Test 2: Check Elements -----
	/*test2(L) :- maplist(test2_, L).
	test2_([H|T]) :- read_number([H|T], Z), !, print_token(H), writeln(Z).
	test2_([H|T]) :- read_data([H|T], Z), !, print_token(H), writeln(Z).
	test2_([H|T]) :- read_header([H|T], Z), !, print_token(H), writeln(Z).
	test2_([H|T]) :- read_filename([H|T], Z), !, print_token(H), writeln(Z).
	test2_([H|T]) :- read_membership([H|T], Z), !, print_token(H), writeln(Z).
	test2_([H|T]) :- read_expression([H|T], Z), !, print_token(H), writeln(Z).
	test2_([H|T]) :- read_condition([H|T], Z), !, print_token(H), writeln(Z).
	test2_([H|T]) :- read_exprorlist([H|T], Z), !, print_token(H), writeln(Z).
	test2_([H|_]) :- print_token(H), writeln(' ERROR!').*/

% ******************** TEST 3: Test Module Statements ******************** 
test3 :-
	TestFile = 'test3.txt',
	reset_error,
	read_file(TestFile, FileCodes),!, 
	tokenize_file(TestFile, FileCodes, FileTokens), !,
	( merge_check(FileTokens) ->
		merge_join(FileTokens, Tokens),
		test3__(Tokens)	
	;
		true
	),
	nb_getval(error, Errors),
	nl, write(Errors),
	writeln(' Error(s) encountered'), nl,
	reset_error.

	test3__([]).
	test3__([H|T]) :- test3_(H), test3__(T).
	test3_([H|T]) :-
		print_token(H),
		( parser:read_statement([H|T], Z) -> writeln(Z) ; writeln(' ********* ERROR!')).

mtest(TestFile, Z) :-
	read_file(TestFile, FileCodes),!, 
	tokenize_file(TestFile, FileCodes, FileTokens), !,
	( merge_check(FileTokens) ->
		merge_join(FileTokens, Tokens),
		mtest__(Tokens, Z)	
	;
		true
	),
	nb_getval(error, 0).

mtest__([], []).
mtest__([H|T], [ZH|ZT]) :- mtest_(H, ZH), mtest__(T, ZT).
mtest_(L, Z) :-
( parser:read_statement(L, Z) -> true ; writeln(' ********* ERROR!')).


% ******************** TEST 4: Test Project Statements ******************** 
test4 :-
	TestFile = 'test4.txt',
	run(TestFile).

% ******************** SCRATCH PAD ********************

debug([]) :- nl.
debug([H|T]) :- writeln(H), debug(T).


