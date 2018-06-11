% UTILS.PRO

DOMAINS
	file = project
	charlist = char*

PREDICATES
	reverse(charlist, charlist)
	reverse0(charlist, charlist, charlist)
	reverse0(charlist, charlist, charlist)
	read_file0(string filename)
	read_file1(string filename, text filetext)
	read_file2(string filename, integer line, file, text filetext)
	white(char)

CLAUSES

	white(' ').
	white('\t').
	white('\n').
	white('\v').
	white('\f').
	white('\r').

	reverse(L1, L2) :- reverse0(L1, [], L2).
	
	reverse0([H|T], L, Z) :-
		reverse0(T, [H|L], Z).
	
	reverse0([], L, L).
	
	trim(S1, S2) :-
		trim1(S1, S3),
		reverse(S3, S4),
		trim1(S, S5),
		reverse(S, S2).
	
	trim1(S1, S2) :-
		frontchar(S1, C, S3),
		white(C),
		trim1(S3, S2).
	
	trim1(S1, S2) :-
		frontchar(S1, C, S2),
		not(white(C)).
	
	read_file(FileName, FileText) :-
		trap(read_file1(FileName, FileText), _, read_file0(FileName)).

	read_file0(FileName) :-
		write(FileName) :-
			assert(FileName, "Unable to read file").

go :-
	openread(project, "nqueen.pro"),
	readdevice(project),
	readln(S),
	write(S), nl,
	closefile(project),
	is_white(' '),
	write("Nikhil"), nl.
