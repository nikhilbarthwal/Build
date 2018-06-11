:-module(generator, [main_class/0, main_implement/0]).

%:- use_module(parser).
:- use_module(library(lists)).


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
	write('predicates\n\tparse: (string, token*) -> '), write(Main), write('.'), nl, nl,
	write('end class '), write(Name), nl.

print_libs([H])   :- write(H), nl.
print_libs([H|T]) :- write(H), write(', '), print_libs(T).

print_typedefs([]) :- nl, !.
print_typedefs([H|T]) :-
	findall(X,define(H, X), L),
	write('\t'), write(H), write(' =  dummy ;'), nl,
	print_types(L),
	print_typedefs(T).
	

print_types([]) :- nl, !.
print_types([H|T]) :- write('\t\t'), print_types0(H), ( T=[] -> write('.') ; write(' ;')), nl, print_types(T).
	
print_types0(Z) :- Z=term(N,L), !, write(N), write('('), print_types1(L), write(')').
print_types0(Z) :- Z=type(N,L), !, write(N), write(' '), write(L).
print_types0(Z) :- Z=list(N,L), !, write(N), write('* '), write(L).

print_types1([]) :- !.
print_types1([H|T]) :- print_types0(H), ( T=[] -> true ; write(', ')), print_types1(T), !.

print_preds([]) :-  nl, !.
print_preds([[H1,H2]|T]) :- 
	write('\tparse_'),
	write(H1), write(': (token* Input, token* Output, '), write(H2), write(' Result) determ (i,o,o).'), nl, print_preds(T).

main_implement :- 
	class(Name, Main,Libs, Desc),
	parse_list(Z),
	write('% ***** implementation '), write(Name),	write(' : '), write(Desc), write(' *****'), nl,nl,
	write('implement '), write(Name), nl,
	( Libs = [] -> true ; write('\topen '), print_libs(Libs)), nl,
	write('class facts'), nl,
	write('\teof:unsigned :=0.'), nl, nl,
	write('class predicates'), nl,
	print_preds(Z),
	write('clauses'), nl,
	print_main_clause(Main), 
	print_clauses(Z),
	write('end implement '), write(Name), nl.

print_main_clause(Main) :- 
	write('\tparse(F,L) = Z :-'),nl,
	write('\t\tif (parse_'), write(Main), write('(L, _, R)) then'), nl,
	write('\t\t\tZ=R'), nl,
	write('\t\telseif (eof=1) then'), nl,
	write('\t\t\terror1(F,\"Unexpected end of file\"),'), nl,
	write('\t\t\tZ=dummy'), nl,
	write('\t\telse'), nl,
	write('\t\t\tZ=dummy'), nl,
	write('\t\tend if.'), nl,
	nl.
	
print_clauses([]) :-  nl, !.
print_clauses([H|T]) :- 
	print_clause(H) , nl,
	print_clauses(T).

print_clause([H1,_]) :-
	findall([L,R], parse(H1, _, L, R), LL),
	print_clause_instance_list(H1, LL),
	fail.
print_clause(_) :- !.

print_clause_instance_list(_, []) :- !.
print_clause_instance_list(H1, [[L,R]|T]) :- print_clause_instance(H1,L,R), print_clause_instance_list(H1, T).    

print_clause_instance(H, L, R) :-
	L=readtokens_end, !,
	write('\tparse_'), write(H), write('(L0, L, R):-'), nl,
write('stdio::write(\" Start :'), write(H), write('\"), stdio::nl,'),  % ***
	write('\t\tL0 = [],'), nl,
	write('\t\tL = [],'), nl,
write('stdio::write(\" End :'), write(H), write(' : '), write(R), write('\"), stdio::nl,'),  % ***
	write('\t\tR = '), write(R), write(', !.'),
	nl,	nl.

print_clause_instance(H, L, R) :-
	L=error1(M), !,
	write('\tparse_'), write(H), write('(L0, L, R):-'), nl,
write('stdio::write(\" Error1 :'), write(H), write('\"), stdio::nl,'),  % ***
	write('\t\tif (L0=[]) then'), nl,
	write('\t\t\teof := 1, fail'), nl,
	write('\t\telse'), nl,
	write('\t\tparse_error3(L0, \"'),
	write(M), write('\", L), '), nl,
	write('\t\t\tR = '), write(R), nl,
	write('\t\tend if, !.'),
	nl, nl.

print_clause_instance(H, L, R) :-
	L=error2(M), !,
	write('\tparse_'), write(H), write('(L0, L, R):-'), nl,
write('stdio::write(\" Error2 :'), write(H), write('\"), stdio::nl,'), nl, % ***
	write('\t\tif (L0=[]) then'), nl,
	write('\t\t\teof := 1, fail'), nl,
	write('\t\telse'), nl,
	write('\t\t!, parse_error2(L0, \"'),
	write(M), write('\", L), '), nl,
	write('\t\t\tR = '), write(R), nl,
	write('\t\tend if, !.'),
	nl, nl.

	
print_clause_instance(H, L, R) :-
	L=error3(M), !,
	write('\tparse_'), write(H), write('(L0, L, R):-'), nl,
write('stdio::write(\" Error3 :'), write(H), write('\"), stdio::nl,'), nl, % ***
	write('\t\tif (L0=[]) then'), nl,
	write('\t\t\teof := 1, fail'), nl,
	write('\t\telse'), nl,
	write('\t\t!, parse_error3(L0, \"'),
	write(M), write('\", L), '), nl,
	write('\t\t\tR = '), write(R), nl,
	write('\t\tend if, !.'),
	nl, nl.

print_clause_instance(H, L, R) :-
	write('\tparse_'), write(H), write('(L0, L, R):-'), nl,
write('stdio::write(\" Start :'), write(H), write('\"), stdio::nl,'), nl, % ***
	write('\t\tif (L0=[]) then'), nl,
	write('\t\t\teof := 1, fail'), nl,
	write('\t\telse'), nl,
	print_clause_statements(N, 0, L),!,
	atom_number(S,N),
	string_concat('L', S, SS),
	write('\t\t\tL = '), write(SS), write(','),nl,
write('stdio::write(\" End :'), write(H), write(' : '), write(R), write('\"), stdio::nl,'), nl,  % ***
	write('\t\t\tR = '), write(R), nl,
	write('\t\tend if, !.'),
	nl, nl.


print_clause_statements(N, N, []) :- !.
print_clause_statements(N, N0, [H|T]) :- print_clause_statement(N1, N0, H), print_clause_statements(N, N1, T).

print_clause_statement(N, N, readtokens_end) :- 
	atom_number(S,N), string_concat('L', S, SS),
	write('\t\t\t'),
	write(SS),
	write(' =[],'),
	nl.

	
print_clause_statement(N1, N0, optmatch('white')) :-
	N1 is N0 + 1,
	atom_number(S0,N0), string_concat('L', S0, SS0),
	atom_number(S1,N1), string_concat('L', S1, SS1),
	write('\t\t\ttrim_white('),
	write(SS0),
	write(', '),
	write(SS1),
	write('),'),
	nl.
	
print_clause_statement(N1, N0, match(X)) :-
	N1 is N0 + 1,
	atom_number(S0,N0), string_concat('L', S0, SS0),
	atom_number(S1,N1), string_concat('L', S1, SS1),
	write('\t\t\tmatch_'), write(X), write('('),
	write(SS0),
	write(', '),
	write(SS1),
	write('),'),
	nl.
	
print_clause_statement(N1, N0, match(X, Y)) :-
	N1 is N0 + 1,
	atom_number(S0,N0), string_concat('L', S0, SS0),
	atom_number(S1,N1), string_concat('L', S1, SS1),
	write('\t\t\tmatch_'), write(X), write('('),
	write(SS0),
	write(', '),
	write(SS1),
	write(', \"'),
	write(Y),
	write('\"),'),
	nl.
	
print_clause_statement(N1, N0, readtokens(X, Y)) :-
	N1 is N0 + 1,
	atom_number(S0,N0), string_concat('L', S0, SS0),
	atom_number(S1,N1), string_concat('L', S1, SS1),
	write('\t\t\tparse_'), write(X), write('('),
	write(SS0),
	write(', '),
	write(SS1),
	write(', '),
	write(Y),
	write('),'),
	nl.

print_clause_statement(N, N, _) :- !.

