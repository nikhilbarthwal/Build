
% Picat> L=findall(X,member(X,[1,2,3])).
% L=[1,2,3]
% membchk(T erm, List)

:- use_module(parser).
:- use_module(library(lists)).

run :- main_implement, !.


uniq(X,Y) :- uniq0(X, [], Y).
 
uniq0([],L, L) :- !.
uniq0(X,L,Y) :-	X=[H|T], ( member(H,L) -> uniq0(T,L,Y) ; append(L,[H],LL), uniq0(T,LL,Y) ).

define_list(Z) :- findall(X, define(X, _), L), uniq(L, Z).
parse_list(Z)  :- findall([X,Y], parse(X,Y,_,_), L), uniq(L, Z).

main_class :-
	class(Name, Main,Libs, Desc),
	write('% ***** class '), write(Name), write(' : '), write(Desc), write(' *****'), nl,nl,
	write('class '), write(Name), nl,
	( Libs = [] -> true ; write('\topen '), print_libs(Libs)),
	write('domains'), nl,
	define_list(Z),
	print_typedefs(Z),
	write('predicates\n\tparse_'),
	write(Main), write('(token* Input, token* Output, '), write(Main), write(' Result) - determ (i,o,o).'), nl, nl,
	write('end class '), write(Name), nl.

print_libs([H])   :- write(H), nl.
print_libs([H|T]) :- write(H), write(', '), print_libs(T).

print_typedefs([]) :- nl, !.
print_typedefs([H|T]) :-
	findall(X,define(H, X), L),
	write('\t'), write(H), write(' = '), nl,
	print_types(L),
	print_typedefs(T).
	

print_types([]) :- nl, !.
print_types([H|T]) :- write('\t\t'), print_types0(H), ( T=[] -> write('.') ; write(' ;')), nl, print_types(T).
	
print_types0(Z) :- Z=term(N,L), !, write(N), write('('), print_types1(L), write(')').
print_types0(Z) :- Z=type(N,L), !, write(N), write(' '), write(L).
print_types0(Z) :- Z=list(N,L), !, write(N), write('* '), write(L).

print_types1([]) :- !.
print_types1([H|T]) :- print_types0(H), ( T=[] -> true ; write(', ')), print_types1(T), !.

print_preds([], _) :-  nl, !.
print_preds([[H,_]|T], Main) :-
	H=Main,
	print_preds(T, Main).
	
print_preds([[H1,H2]|T], Main) :- 
	write('\tparse_'),
	write(H1), write(': (token* Input, token* Output, '), write(H2), write(' Result) - determ (i,o,o).'), nl, print_preds(T, Main).

main_implement :- 
	class(Name, Main,Libs, Desc),
	parse_list(Z),
	write('% ***** implementation '), write(Name),	write(' : '), write(Desc), write(' *****'), nl,nl,
	write('implement '), write(Name), nl,
	( Libs = [] -> true ; write('\topen '), print_libs(Libs)), nl,
	write('class predicates'), nl,
	print_preds(Z, Main),
	write('clauses'), nl,
	print_clauses(Z),
	write('end implement '), write(Name), nl.

print_clauses([]) :-  nl, !.
print_clauses([H|T]) :- 
	print_clause(H) , nl,
	print_clauses(T).

print_clause([H1,_]) :-
	findall([L,R], parse(H1, _, L, R), LL),
	print_clause_instance_list(H1, LL),
	fail.
print_clause(_) :- !.

print_clause_instance_list(_, [], _) :- !.
print_clause_instance_list(H1, [[H,R]|T]) :- print_clause_instance(H1,H,R), print_clause_instance_list(H1, T, R).    
%nl, !.
%print_clause_instance(H, L, R) :-

print_clause_instance(H, L, R) :-
	write('\tparse_'), write(H), write('(L, L0, R):-'), nl,
	print_clause_statements(N, 0, L),!,
	atom_number(S,N),
	string_concat('L', S, SS),
	write('\t\tL = '), write(SS), write(','),nl,
	write('\t\tR = '), write(R), write('.'),
	nl.


print_clause_statements(N, N, []) :- !.
print_clause_statements(N, N0, [H|T]) :- print_clause_statement(N1, N0, H), print_clause_statements(N, N1, T).

print_clause_statement(N1, N0, Z) :- Z=cut, !, N1=N0.
print_clause_statement(N1, N0, optmatch('white')) :-
	N1 is N0 + 1,
	atom_number(S0,N0), string_concat('L', S0, SS0),
	atom_number(S1,N1), string_concat('L', S1, SS1),
	write('\t\ttrim_white('),
	write(SS0),
	write(', '),
	write(SS1),
	write('),'),
	nl.
	
print_clause_statement(N1, N0, match(X)) :-
	N1 is N0 + 1,
	atom_number(S0,N0), string_concat('L', S0, SS0),
	atom_number(S1,N1), string_concat('L', S1, SS1),
	write('\t\tmatch_'), write(X), write('('),
	write(SS0),
	write(', '),
	write(SS1),
	write('),'),
	nl.
	
print_clause_statement(N1, N0, match(X, Y)) :-
	N1 is N0 + 1,
	atom_number(S0,N0), string_concat('L', S0, SS0),
	atom_number(S1,N1), string_concat('L', S1, SS1),
	write('\t\tmatch_'), write(X), write('('),
	write(SS0),
	write(', '),
	write(SS1),
	write(', '),
	write(Y),
	write('),'),
	nl.
	
print_clause_statement(N1, N0, readtokens(X, Y)) :-
	N1 is N0 + 1,
	atom_number(S0,N0), string_concat('L', S0, SS0),
	atom_number(S1,N1), string_concat('L', S1, SS1),
	write('\t\tparse_'), write(X), write('('),
	write(SS0),
	write(', '),
	write(SS1),
	write(', '),
	write(Y),
	write('),'),
	nl.

print_clause_statement(N, N, _) :- !.

/*main_type(L) =>
[match('identifier0','feature'), match('white'), readtokens('namespace', 'L'), match('open1'), readtokens('build_vars', 'A'), optmatch('white'),readtokens('close1'), match('white'), match('identifier0','in'), match('white'), readtokens('str', 'F')], ['Output = feature(F, L, A)']).

define('p_statement', term('feature', [ type('token', 'FileName'), list('token', 'NameSpace'), list('p_varvalue', 'ArgsList')])).

	write('domain'), nl,
	
	foreach (I in L)
		write('\t'),
		write(I),
		write(' = '),
		nl,
		gen_type(I),
		nl
	end.



class lexer
    open core
domains
	token =
		identifier0(string Data, string Filename, unsigned Linenum, unsigned Position) ;
        identifier1(string Data, string Filename, unsigned Linenum, unsigned Position) ;
		str(string Data, string Filename, unsigned Linenum, unsigned Position) ;
		num(unsigned Data, string Filename, unsigned Linenum, unsigned Position) ;
		white(string Filename, unsigned Linenum, unsigned Position) ;
		symbols(string Data, string Filename, unsigned Linenum, unsigned Position) ;
		open1(string Filename, unsigned Linenum, unsigned Position) ;
		open2(string Filename, unsigned Linenum, unsigned Position) ;
		open3(string Filename, unsigned Linenum, unsigned Position) ;
		close1(string Filename, unsigned Linenum, unsigned Position) ;
		close2(string Filename, unsigned Linenum, unsigned Position) ;
		close3(string Filename, unsigned Linenum, unsigned Position) ;
		endline(string Filename, unsigned Linenum, unsigned Position) ;
        continue(string Filename, unsigned Linenum, unsigned Position).

predicates
    process: (string Filename) -> token*.

end class lexer

	
gen_type(X) =>
	L=findall(Y, define(X, Y)),
	N=length(L),
	I=1,
	foreach (Z in L)
		%print_type(Z),
		write(' '),
		if (I=N) then
			write('.')
		else
			write(';')
		end,
		nl,
		I := I+1
	end.

%print_type(I) :-

*/