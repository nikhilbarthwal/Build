:- module(print, [print/1]).

:- use_module(library(lists)).


print(Structure) :-
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


