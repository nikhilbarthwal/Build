% LEXER.PRO

GLOBAL DOMAINS
	text = string*

GLOBAL PREDICATES
	determ read_file(string filename, text filetext) - (i,o)

DOMAINS
	file = project
	charlist = char*

PREDICATES
	determ str_rev(string, string) - (i,o)
	determ str_rev0(string, string, string) - (i,i,o)
	
	determ read_file0(string filename, text dummy)
	determ read_file1(string filename, text filetext) - (i, o)
	determ read_file2(string filename, file, integer line, text filetext) - (i, i, i, o)
	
	determ trim(string, string)- (i,o)
	determ trim1(string, string)- (i,o)
	
	determ white(char) - (i)
	determ digit(char) - (i)
	determ letter(char) -(i)
	determ alphanum(char) -(i)

CLAUSES

	white(' ').
	white('\t').
	white('\n').
	%white('\v').
	%white('\f').
	white('\r').

	digit(C) :- '0' <= C, C <= '9', !.
	
	letter(C):- 'a' <= C, C <= 'z', !.
	letter(C):-	'A' <= C, C <= 'Z', !.

	alphanum(C) :- digit(C), !.
	alphanum(C) :- letter(C), !.
	
	str_rev(S1, S2) :- !, str_rev0(S1, "", S2).
	
	str_rev0(S1, S2, Z) :-
		frontchar(S1, C, S3), !,
		str_char(S4, C),
		concat(S4, S2, S5),
		str_rev0(S3, S5, Z).
	
	str_rev0("", Z, Z).
	
	trim(S1, S2) :-
		trim1(S1, S3),
		str_rev(S3, S4),
		trim1(S4, S5),
		str_rev(S5, S2).
	
	trim1(S1, S2) :-
		frontchar(S1, C, S3),
		white(C), !,
		trim1(S3, S2).
	trim1(S1, S1).
	
	read_file(FileName, FileText) :-
		trap(read_file1(FileName, FileText), _, read_file0(FileName, FileText)).
	
	read_file0(FileName, []) :-
		add_error1(FileName, "Unable to read file").
	
	read_file1(FileName, FileText) :-
		openread(project, FileName),
		readdevice(project),
		read_file2(FileName, project, 1, FileText),
		closefile(project).
	
	read_file2(_, project, _, []) :-
		eof(project), !.
	
	read_file2(FileName, project, N, [S|T]) :-
		not(eof(project)),
		readln(S), !,
		trim(S, H),
		NN = N+1,
		assert(source(FileName, N, H)),
		read_file2(FileName, project, NN, T).
	
	read_file2(FileName, project, N, L) :-
		not(eof(project)), !,
		str_int(S, N),
		concat("Unable to read file at line ", S, M),
		add_error2(FileName,N,M),
		NN = N+1,
		read_file2(FileName, project, NN, L).
