/*****************************************************************************

		Copyright (c) My Company

 Project:  PRAYOG - MAIN FILE, inculduig test cases
 FileName: PARSER.PRO
 Purpose: No description
 Written by: Visual Prolog
 Comments:
******************************************************************************/

include "DBase.pro"
include "Tokens.pro"
include "Read.pro"
include "Lexer.pro"
include "Definations.pro"
include "Parser.pro"
include "Project.pro"



PREDICATES
	determ start(string) -(i)
	determ debug(filetokens) -(i)

CLAUSES
	start(FileName) :-
		read_file(FileName, FileText), !,
		tokenize_file(FileName, FileText, FileTokens),
		%debug(FileTokens).
		%write(" -> ",FileTokens), nl,
		parse_project(FileTokens, Statements),
		interpret_project(FileName, Statements, _PStructure).
		
	start(" n").

	debug([]) :- !.
	debug([H|T]) :- write(H), nl, !, debug(T).
	
GOAL
	start("test1.txt").
