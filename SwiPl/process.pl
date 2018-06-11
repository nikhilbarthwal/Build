:- module(process, [run/1]).
% ***** Prayog Main File *****

:- use_module(utils).
:- use_module(tokenize).
:- use_module(parser).
:- use_module(library(lists)).


%
% ******************** Project Execution ******************** 
%

% run(+Project) : Load & Execute Project and display error
run(Project) :- project(Project).
run(_) :-
	nb_getval(error, Errors),
	nl, write(Errors),
	writeln(' Error(s) encountered'),
	nl.

% process(+Project) : Process the Project File
project(Project) :- 
	reset_error, !,
	read_file(Project, FileCodes),!, 
	tokenize_file(Project, FileCodes, FileTokens), !,
	merge_join(FileTokens, Tokens), !,
	parse_project(Tokens, Statements), !,
	nb_getval(error, 0),
	structure(Statements, _Structure), !.
	%source_xml(Structure).
	%nb_getval(error, 0).
	%% Verify Structure
	%% Display Structure into XML File
	%% Execute Structure


% project_parse(Tokens, Statements) : Convert List of TokenList to Statement List
parse_project([],[]) :- !.
parse_project([H|T], [SH|ST]) :- parse_pstatement(H, SH), parse_project(T, ST).

% project_struct(Tokens, Structure) : Convert Token List to Structure
structure(L, Structure) :-
	header(L, L1, Name), !,
	read_includes(L1, L2, Includes), !, 
	read_all_targets(L2, Targets), !,
	Modules = Includes, % Read modules here
	Structure = project(Name, Modules, Targets).
	
structure(_, _) :-
	writeln('ERROR: Unexpected end File'),
	count_error,
	fail.	


% header(Statements, Name) : Read statements and extract Name and Include files
header([H|T], T, Name) :-
	H = statement(prayog(Name), _).
header([H|T], [H|T], 0) :-
	H = statement(_, Z),
	print_token(Z),
	writeln('Master Build defination should be declared in the first line'),
	count_error.	


% read_includes(Statements, Leftovers, Includes) : Read the entire Include block
read_includes([H|T], L, I) :-
	H = statement(begin, _),
	read_includes_(T, L, I).

read_includes([H|T], T, []) :-
	H = statement(_, Z),
	print_token(Z),
	writeln('Expecting start of Include files block'),
	count_error.	

read_includes_([H|T], R, [IH|IT]) :-
	H = statement(pinclude(IH), _),
	read_includes_(T, R, IT).

read_includes_([H|T], T, []) :-
	H = statement(end, _).

read_includes_([H|T], R, I) :-
	H = statement(_, Z),
	print_token(Z),
	writeln('Only include statements are allowed in the first block'),
	count_error,
	read_includes_(T, R, I).


% read_target(Statements, LeftOvers, Target) : Read the entire Target defination block
read_target([H|T], R, Z) :-
	H = statement(target(Name), _),
	read_target1(T, R, Input, Output),
	Z=target(Name, Input, Output).

read_target([H|T], T, error) :-
	H = statement(_, Z),
	print_token(Z),
	writeln('Expecting Target defination'),
	count_error.	

read_target1([H|T], R, Input, Output) :-
	H = statement(begin, _),
	read_target2(T, R, Input, Output).

read_target1([H|T], T, [], []) :-
	H = statement(_, Z),
	print_token(Z),
	writeln('Expecting start of Target defination block'),
	count_error.	

read_target2([H|T], R, [InputHead|InputTail], Output) :-
	H = statement(InputHead, _),
	InputHead = set(_, _),
	read_target2(T, R, InputTail, Output).

read_target2([H|T], R, Input, [OutputHead|OutputTail]) :-
	H = statement(OutputHead, _),
	OutputHead = report(_, _),
	read_target2(T, R, Input, OutputTail).

read_target2([H|T], T, [], []) :-
	H = statement(end, _).

read_target2([H|T], R, Input, Output) :-
	H = statement(_, Z),
	print_token(Z),
	writeln('Only Build Parameters or Report Variables are allowed'),
	count_error,
	read_target2(T, R, Input, Output).


% read_all_targets(Statements, Leftovers, Targets) : Read all the targets
read_all_targets([], []).
read_all_targets(L, [H|T]) :-
	read_target(L, R, H),
	read_all_targets(R, T).


	
source_xml(Structure) :-
	Structure = project(identifier(Name, _),  Includes, Targets),
	write('<project name=\"'),
	write(Name),
	writeln('\">'),!, 
	
	length(Includes, N1),
	( N1 = 0 ->
		writeln('<includes/>')
	;
		writeln('<includes>'),
		source_xml_includes(Includes),
		writeln('</includes>')
	), !, 
	
	length(Targets, N2),
	( N2 = 0 ->
		writeln('<targets/>')
	;
		writeln('<targets>'),
		source_xml_targets(Targets),
		writeln('</targets>')
	), !, 
	write('</project').

source_xml_includes([]).
source_xml_includes([str(H, _)|T]) :-
	write('<file name=\"'),
	write(H),
	writeln('\"/>'),
	source_xml_includes(T).


source_xml_targets([]).
source_xml_targets([H|T]) :-
	H = target(identifier(N, _), D, R),
	length(D, N1),
	length(R, N2),
	( N1 = 0, N2 = 0 ->
		write('<target name=\"'),
		write(N),
		writeln('\"/>')
	; true),
	
	( N1 > 0, N2 = 0 ->
		write('<target name=\"'),
		write(N),
		writeln('\">'),
		writeln('<parameters>'),
		source_xml_target_params(D),
		writeln('</parameters>'),
		writeln('<reports/>'),
		writeln('</target>')
	; true),
	
	( N1 = 0, N2 > 0 ->
		write('<target name=\"'),
		write(N),
		writeln('\">'),
		writeln('<parameters/>'),
		writeln('<reports>'),
		source_xml_target_reports(R),
		writeln('</reports>'),
		writeln('</target>')
	; true),
	
	( N1 > 0, N2 > 0 ->
		write('<target name=\"'),
		write(N),
		writeln('\">'),
		writeln('<parameters>'),
		source_xml_target_params(D),
		writeln('</parameters>'),
		writeln('<reports>'),
		source_xml_target_reports(R),
		writeln('</reports>'),
		writeln('</target>')
	; true),	
	source_xml_targets(T).

source_xml_target_params([]) :- !.
source_xml_target_params([H|T]) :-
	H = set(identifier(V, _), D),
	write('<param name=\"'),
	write(V),
	write('\" value=\"'),
	write(D),
	writeln('\"/>'),
	source_xml_target_params(T).
	
source_xml_target_reports([]) :- !.
source_xml_target_reports([H|T]) :-
	H = report(identifier(V, _), D),
	write('<report name=\"'),
	write(V),
	write('\" value=\"'),
	write(D),
	writeln('\"/>'),
	source_xml_target_reports(T).


