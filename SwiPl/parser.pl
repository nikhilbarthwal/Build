:- module(parser, [parse_statement/2, parse_pstatement/2]).

%
% ******************** PARSE: Error control for Read ******************** 
%

% Identifier
parse_identifier(Z, Value) :-
	( read_identifier(Z, Value) -> true ;
	print_token(Z),
	write('Unable to understand identifier '),
	tokens_atom([Z], X), writeln(X),
	Value = error,
	count_error ).

% Number
parse_number([H|T], Value) :-
	( read_number([H|T], Value) -> true ;
	print_token(H),
	write('Unable to understand number '),
	tokens_atom([H|T], X), writeln(X),
	Value = error,
	count_error ).

% String
parse_string([H|T], Value) :-
	( read_string([H|T], Value) -> true ;
	print_token(H),
	write('Unable to understand string '),
	tokens_atom([H|T], X), writeln(X),
	Value = error,
	count_error ).

% List
parse_list([H|T], Value) :-
	( read_list([H|T], Value) -> true ;
	print_token(H),
	write('Unable to understand list '),
	tokens_atom([H|T], X), writeln(X),
	Value = error,
	count_error ).

% Data
parse_data([H|T], Value) :-
	( read_data([H|T], Value)  -> true ;
	print_token(H),
	write('Unable to understand '),
	tokens_atom([H|T], X), writeln(X),
	Value = error,
	count_error ).

% Variable
parse_variable(Z, Value) :-
	( read_variable(Z, Value) -> true ;
	print_token(Z),
	write('Unable to understand variable '),
	tokens_atom(Z, X), writeln(X),
	Value = error,
	count_error ).

% Header
parse_header([H|T], Value) :-
	( read_header([H|T], Value) -> true ;
	print_token(H),
	write('Unable to understand header '),
	tokens_atom([H|T], X), writeln(X),
	Value = error,
	count_error ).

% Call
parse_call([H|T], Value) :-
	( read_call([H|T], Value) -> true ;
	print_token(H),
	write('Unable to understand the call '),
	tokens_atom([H|T], X), writeln(X),
	Value = error,
	count_error ).

% Memebership
parse_membership([H|T], Value) :-
	( read_membership([H|T], Value)  -> true ;
	print_token(H),
	write('Unable to understand the iterator '),
	tokens_atom([H|T], X), writeln(X),
	Value = error,
	count_error ).
	
% Expression:
parse_expression([H|T], Value) :-
	( read_expression([H|T], Value) -> true ;
	print_token(H),
	write('Unable to understand the expression '),
	tokens_atom([H|T], X), writeln(X),
	Value = error,
	count_error ).

% Condition:
parse_condition([H|T], Value) :-
	( read_condition([H|T], Value) -> true ;
	print_token(H),
	write('Unable to understand the condition '),
	tokens_atom([H|T], X), writeln(X),
	Value = error,
	count_error ).

% Filename:
parse_filename([H|T], Value) :-
	( read_filename([H|T], Value) -> true ;
	print_token(H),
	write('Unable to understand the file name '),
	tokens_atom([H|T], X), writeln(X),
	Value = error,
	count_error ).

% Module:
parse_module([H|T], Value) :-
	( read_module([H|T], Value) -> true ;
	print_token(H),
	write('Unable to understand module name '),
	tokens_atom([H|T], X), writeln(X),
	Value = error,
	count_error ).

% Statement:
parse_statement([H|T], statement(V, H)) :- read_statement([H|T],V).
parse_statement([H|_], statement(error, H)) :-
	print_token(H),
	writeln('Unable to understand the statement '),
	count_error.


% PStatement:
parse_pstatement([H|T], statement(V, H)) :- read_pstatement([H|T],V).
parse_pstatement([H|_], statement(error, H)) :-
	print_token(H),
	writeln('Unable to understand the statement '),
	count_error.
%
% ******************** READ: Extract info from Tokens ******************** 
%

%
% ----- Identifier ----- 
%

% Identifier --> identifier(Value, Token)
read_identifier(H, identifier(V, H)) :- 
	token_data(H, identifier, C),
	atom_codes(V, C).

%
% ----- Identifier List -----
%

% Identifier List: I1 I2 I3 ..  --> [ I1, I2, I2 .. ]
read_identifierlist(L, []) :-
	trim_first_space(L, []).

read_identifierlist(L, [A|B]) :-
	trim_first_space(L, [H|T]),
	read_identifier(H, A),
	read_identifierlist(T, B).

%
% ----- Number -----
%

% +xxxxx --> num(Value, Token)
read_number([H|T], num(V, H)) :-
	token_data(H, symbols, "+"),
	read_number(T, num(V, _)).

% -xxxxx --> num(Value, Token)
read_number([H|T], num(V2, H)) :-
	token_data(H, symbols, "-"), !,
	read_number(T, num(V1, _)),
	V2 is -1*V1.

% xxxxx --> num(Value, Token)
read_number([H], num(V, H)) :-
	token_data(H, int, C),
	number_codes(V, C).

% xxx.xxx --> num(Value, Token)
read_number([H1, H2, H3], num(V, H1)) :-
	token_data(H1, int, C1),
	token_data(H2, symbols, "."),
	token_data(H3, int, C3),
	append(C1, ".", C2),
	append(C2, C3, C),
	number_codes(V, C).

%
% ----- Variable -----
%

% Identifier --> variable(Name, Token)
read_variable(H, variable(V, H)) :-
	token_data(H, identifier, C),
	atom_codes(V, C).

% Identifier --> variable(Name, Token)
read_variable([H], variable(V, H)) :-
	token_data(H, identifier, C),
	atom_codes(V, C).

%
% ----- String -----
%

% "xx xx xx" --> str(Value, Token)
read_string([H], str(V, H)) :-
	token_data(H, identifier, C),
	atom_codes(V, C).

read_string([H], str(V, H)) :-
	token_data(H, str, C),
	atom_codes(V, C).

	
%
% ----- List -----
%

% [data1 data2 data3] --> list(Value, Token)
read_list([H|T], list(V, H)) :-
	last(T, L),
	token_data(H, open2, _),
	token_data(L, close2, _), !,
	append(R, [L], T),
	trim_space(R, RR),
	read_datalist(RR, V).

read_datalist(L, []) :- 
	trim_first_space(L, []).

read_datalist(L, [DH|DT]) :-
	space_partition(L, H, T),!,
	read_data(H, DH),
	read_datalist(T, DT).

%
% ----- Data -----
%

% Data: String / List / Number
read_data(L, V) :- read_variable(L, V), !.
read_data(L, V) :- read_number(L, V), !.
read_data(L, V) :- read_list(L, V), !.

%
% ----- Header -----
%

read_header_([H1, H2|T], (V, I)  ) :-
	last(T, L),
	token_data(H2, open1, _),
	token_data(L, close1, _),
	read_identifier(H1, V),
	append(R, [L], T),
	read_identifierlist(R, I).

read_header_([H], (V, 0)  ) :-
	read_identifier(H, V).

% name(x y z) : a b c --> header(Name, I, O)
read_header(L, header(V, I, O)) :-
	token_partition(L, ":", L1, L2), !,
	read_header_(L1, (V, I)),
	read_identifierlist(L2, O).

% name(x y z) --> header(Name, I, 0)
read_header(L, header(V, I, 0)) :-
	read_header_(L, (V, I)).

%
% ----- FCall -----
%

read_call_([H1, H2|T], (V, E)) :-
	last(T, L),
	read_identifier(H1, V),
	token_data(H2, open1, _),
	token_data(L, close1, _),
	append(R, [L], T),
	read_exprlist(R, E).

read_call_([H1, H2|T], ((M, V), E)) :-
	read_identifier(H1, M),
	token_data(H2, symbols, "."),
	read_call_(T, (V, E)).


% {module.}name(exp1 exp2 exp3) --> fcall(V, E, 0)         E is the expression List
read_call(L, fcall(V, E, 0)) :-
	read_call_(L, (V, E)).
	
% name(exp1 exp2 exp3) --> fcall(Name, E, R)     
% module.name(exp1 exp2 exp3) --> fcall( (Module, Name), E, R)     
read_call(L, fcall(V, E, R)) :-
	tight_token_partition(L, ":", L1, [L2]),
	read_call_(L1, (V, E)),
	read_identifier(L2, R).
%
% ----- PCall -----
%

read_pcall([H1, H2|T], (V, E)) :-
	last(T, L),
	read_identifier(H1, V),
	token_data(H2, open1, _),
	token_data(L, close1, _),
	append(R, [L], T),
	read_pexprlist(R, E).

read_pcall([H1, H2|T], ((M, V), E)) :-
	read_identifier(H1, M),
	token_data(H2, symbols, "."),
	read_pcall(T, (V, E)).


%
% ----- Expression List -----
%

% exp1 exp2 exp3 --> [ ]
read_exprlist([], []).
read_exprlist(L, [H|T]) :-
	space_partition(L, L1, L2),
	read_expression(L1, H), !,
	read_exprlist(L2, T).
read_exprlist(L, [H]) :-
	read_expression(L, H).


%
% ----- Port Expression List -----
%

% exp1 exp2 exp3 --> [ ]
read_pexprlist([], []).
read_pexprlist(L, [H|T]) :-
	space_partition(L, L1, L2),
	read_pexpression(L1, H), !,
	read_pexprlist(L2, T).
read_pexprlist(L, [H]) :-
	read_pexpression(L, H).
read_pexprlist([H1,H2], [H]) :-
	read_identifier(H2, identifier(V,_)),
	token_data(H1, symbols, "@"),
	H=port(V, H1).

	
%
% ----- Major List -----
%

% MinorList1 ; MinorList2 ; MinorList3  --> [ M1 M2 M3]
read_majorlist([], []). 
read_majorlist(L, [H|T]) :-
	token_partition(L, ";", L1, L2),
	read_minorlist(L1, H), !,
	read_majorlist(L2, T).
read_majorlist(L, [H]) :-
	trim_space(L, LL),
	read_minorlist(LL, H).

%
% ----- Minor List -----
%

% Membership1 ; Membership2 ; Membership3  --> [ M1 M2 M3]
read_minorlist([], []).
read_minorlist(L, [H|T]) :-
	token_partition(L, ",", L1, L2),
	read_membership(L1, H), !,
	read_minorlist(L2, T).
read_minorlist(L, [H]) :-
	trim_space(L, LL),
	read_membership(LL, H).

	
%
% ----- Membership -----
%

% Var in List --> mem(Var, List, Token)
read_membership([H|T], mem(V, E)) :-
	spaced_token_partition([H|T], "in", [L1], L2),
	read_identifier(L1, V),
	read_expression(L2, E).


%
% ----- Expression -----
%

% if Condition then Expression else Expression --> expr_if(Cond, Exp1, Exp2)
read_expression([H1, H2 |T], expr_if(C, E1, E2)) :-
	token_data(H1, identifier, "if"),
	white_token(H2),
	spaced_token_partition(T, "then", CC, R),
	spaced_token_partition(R, "else", L1, L2),
	read_condition(CC, C),
	read_expression(L1, E1),
	read_expression(L2, E2).

% Exp1 + Exp2 --> add(Exp1, Exp2)
read_expression(L, add(V1, V2)) :-
	token_partition(L, "+", L1, L2),
	read_expression(L1, V1),
	read_expression(L2, V2).
	
% Exp1 - Exp2 --> sum(Exp1, Exp2)
read_expression(L, sub(V1, V2)) :-
	token_partition(L, "-", L1, L2),
	read_expression(L1, V1),
	read_expression(L2, V2).

% Exp1 * Exp2 --> multiply(Exp1, Exp2)
read_expression(L, multiply(V1, V2)) :-
	token_partition(L, "*", L1, L2),
	read_expression(L1, V1),
	read_expression(L2, V2).

% Exp1 / Exp2 --> divide(Exp1, Exp2)
read_expression(L, divide(V1, V2)) :-
	token_partition(L, "/", L1, L2),
	read_expression(L1, V1),
	read_expression(L2, V2).

% Exp1 // Exp2 --> int_divide(Exp1, Exp2)
read_expression(L, int_divide(V1, V2)) :-
	token_partition(L, "//", L1, L2),
	read_expression(L1, V1),
	read_expression(L2, V2).

% Exp1 % Exp2 --> modulus(Exp1, Exp2)
read_expression(L, modulus(V1, V2)) :-
	token_partition(L, "%", L1, L2),
	read_expression(L1, V1),
	read_expression(L2, V2).

% Exp1 ^ Exp2 --> concat(Exp1, Exp2)
read_expression(L, concat(V1, V2)) :-
	token_partition(L, "^", L1, L2),
	read_expression(L1, V1),
	read_expression(L2, V2).

% Exp1 & Exp2 --> join(Exp1, Exp2)
read_expression(L, join(V1, V2)) :-
	token_partition(L, "&", L1, L2),
	read_expression(L1, V1),
	read_expression(L2, V2).

% Exp1 ++ Exp2 --> add_list(Exp1, Exp2)
read_expression(L, add_list(V1, V2)) :-
	token_partition(L, "++", L1, L2),
	read_expression(L1, V1),
	read_expression(L2, V2).

% Exp1 .. Exp2 --> gen(Exp1, Exp2)
read_expression(L, generate(V1, V2)) :-
	token_partition(L, "..", L1, L2),
	read_expression(L1, V1),
	read_expression(L2, V2).

% map[ MajorList : Expr ?-  Cond ] --> map2(MajorList, Expr, Cond)
read_expression([H1,H2|T], map2(M, E, C)) :-
	token_data(H1, identifier, "map"),
	token_data(H2, open2, _),
	last(T, L),
	token_data(L, close2, _),
	append(L1, [L], T), 
	token_partition(L1, ":", MT, L2),
	read_majorlist(MT, M), 
	spaced_token_partition(L2, "?-", ET, CT),
	read_expression(ET, E),
	read_condition(CT, C).

% map[ MajorList : Expression ] --> map1(MajorList, Expr)
read_expression([H1,H2|T], map1(M, E)) :-
	token_data(H1, identifier, "map"),
	token_data(H2, open2, _),
	last(T, L),
	token_data(L, close2, _),
	append(R, [L], T), !,
	spaced_token_partition(R, ":", MT, ET),
	read_majorlist(MT, M),
	read_expression(ET, E).

% { Expression }
read_expression([H|T], enum(V)) :-
	token_data(H, symbols, "{"),
	last(T, L),
	token_data(L, symbols, "}"),
	append(R, [L], T),
	trim_space(R, E),
	read_expression(E, V).

% Call: fcall(N, I, 0)
read_expression(L, V) :-
 	read_call(L, V).
 
% Data --> variable(Name, Token) ; Num(V, H) ; list(L, H)
read_expression(L, V) :-
 	trim_space(L, Z),
	read_data(Z, V).

% (Expression) --> Expression
read_expression([H|T], V) :-
	token_data(H, open1, _),
	last(T, L),
	token_data(L, close1, _),
	append(R, [L], T), !,
	trim_space(R, E),
	read_expression(E, V).


%
% ----- Condition -----
%

% Membership
read_condition(Tokens, Value) :- 
	read_membership(Tokens, Value).

% if C1 then C2 --> cond_if1(C1, C2)
read_condition([H1, H2|T], cond_if1(C1, C2)) :-
	token_data(H1, identifier, "if"),
	white_token(H2),
	spaced_token_partition(T, "then", L1, L2),
	read_condition(L1, C1),
	read_condition(L2, C2).

% if C1 then C2 else C3 --> cond_if2(C1, C2, C2)
read_condition([H1, H2|T], cond_if2(C1, C2, C3)) :-
	token_data(H1, identifier, "if"),
	white_token(H2),
	spaced_token_partition(T, "then", L1, L),
	spaced_token_partition(L, "else", L2, L3),
	read_condition(L1, C1),
	read_condition(L2, C2),
	read_condition(L3, C3).

% A and B --> and(A, B)
read_condition(L, and(Value1, Value2)) :-
	spaced_token_partition(L, "and", List1, List2),
	read_condition(List1, Value1),
	read_condition(List2, Value2).

% A or B --> or(A, B)
read_condition(L, or(Value1, Value2)) :-
	spaced_token_partition(L, "or", List1, List2),
	read_condition(List1, Value1),
	read_condition(List2, Value2).

% A < B --> lt(A, B)
read_condition(L, lt(Value1, Value2)) :-
	token_partition(L, "<", List1, List2),
	read_expression(List1, Value1),
	read_expression(List2, Value2).
	
% A > B --> gt(A, B)
read_condition(L, gt(Value1, Value2)) :-
	token_partition(L, ">", List1, List2),
	read_expression(List1, Value1),
	read_expression(List2, Value2).

% A => B --> ge(A, B)
read_condition(L, ge(Value1, Value2)) :-
	token_partition(L, "=>", List1, List2),
	read_expression(List1, Value1),
	read_expression(List2, Value2).

% A >= B --> ge(A, B)
read_condition(L, ge(Value1, Value2)) :-
	token_partition(L, ">=", List1, List2),
	read_expression(List1, Value1),
	read_expression(List2, Value2).

% A <= B --> le(A, B)
read_condition(L, le(Value1, Value2)) :-
	token_partition(L, "<=", List1, List2),
	read_expression(List1, Value1),
	read_expression(List2, Value2).

% A =< B --> le(A, B)
read_condition(L, le(Value1, Value2)) :-
	token_partition(L, "=<", List1, List2),
	read_expression(List1, Value1),
	read_expression(List2, Value2).

% A = B --> eq(A, B)
read_condition(L, eq(Value1, Value2)) :-
	token_partition(L, "=", List1, List2),
	read_expression(List1, Value1),
	read_expression(List2, Value2).

% A != B --> ne(A, B)
read_condition(L, ne(Value1, Value2)) :-
	token_partition(L, "!=", List1, List2),
	read_expression(List1, Value1),
	read_expression(List2, Value2).

% Condition Call
read_condition(L, cond(N, A)) :- 
	read_call(L, Value),
	Value = fcall(N, A, 0).

% not(C) --> inverse(C)
read_condition([H1,H2|T], inverse(V)) :-
	token_data(H1, identifier, "not"),
	token_data(H2, open1, _),
	last(T, L),
	token_data(L, close1, _),
	append(R, [L], T),
	trim_space(R, C),
	read_condition(C, V).

% (C) --> C
read_condition([H|T], V) :-
	token_data(H, open1, _),
	last(T, L),
	token_data(L, close1, _),
	append(R, [L], T),
	trim_space(R, C),
	read_condition(C, V).


%
% ----- Statement -----
%

% [ --> begin(Token)
read_statement([Z], begin(Z)) :-
	token_data(Z, open2, _).

% ] --> end(Token)
read_statement([Z], end(Z)) :-
	token_data(Z, close2, _).

% | --> alt(Token)
read_statement([Z], alt(Z)) :-
	token_data(Z, symbols, "|").

% Function Header --> def_func(Name, Input, Ouput)
read_statement(Tokens, def_func(Name, I, O)) :-
	Tokens = [ Head1, Head2 | Tail],
	token_data(Head1, identifier, "Function"),
	white_token(Head2),
	read_header(Tail, header(Name, I, O)).

% Condition {Header:Name( Inp1 Inp2 .. )} --> def_cond(Name, Input)
read_statement([H1,H2|T], def_cond(Name, List)) :-
	token_data(H1, identifier, "Condition"),
	white_token(H2),
	read_header(T, header(Name, List, 0)).

% Export {Variable:V} : {Expression:e} --> def_export(V, E)
read_statement([H1,H2|T], def_export(Var, Expr)) :-
	token_data(H1, identifier, "Export"),
	white_token(H2),
	token_partition(T, ":", [V], E),
	read_identifier(V, Var),
	read_expression(E, Expr).

% Import {Variable:V1} : {Variable:V2} --> def_import(V1 V2)
read_statement([H1,H2|T], def_import(Var1, Var2)) :-
	token_data(H1, identifier, "Import"),
	white_token(H2),
	token_partition(T, ":", [V1], [V2]),
	read_identifier(V1, Var1),
	read_identifier(V2, Var2).

% Component header --> def_component(Name, Input, Ouput)
read_statement([H1, H2 | T], def_component(Name, Inp, Out)) :-
	token_data(H1, identifier, "Component"),
	white_token(H2),
	read_header(T, header(Name, Inp, Out)).

%Compose header -->  def_compose(Name, Input, Ouput)
read_statement([H1, H2|T], def_compose(Name, Inp, Out)) :-
	token_data(H1, identifier, "Compose"),
	white_token(H2),
	read_header(T, header(Name, Inp, Out)).

%-Header --> def_slot(Name, Input, Output)
read_statement([H|T], def_slot(Name, Inp, Out)) :-
	token_data(H, symbols, "-"),
	read_header(T, header(Name, Inp, Out)).

%+Header --> def_multislot(Name, Input, Output)
read_statement([H|T], def_multislot(Name, Inp, Out)) :-
	token_data(H, symbols, "+"),
	read_header(T, header(Name, Inp,Out)).

% Statement1 | Statement2 --> statement_or(Statement1, Statement2)
read_statement(L, statement_or(S1, S2)) :-
	token_partition(L, "|", L1, L2),
	\+ L1 = [],
	\+ L2 = [],
	read_statement(L1, S1),
	read_statement(L2, S2).

% Statement1 ; Statement2 --> statement_and(Statement1, Statement2)
read_statement(L, statement_and(S1, S2)) :-
	token_partition(L, ";", L1, L2),
	\+ L1 = [],
	\+ L2 = [],
	read_statement(L1, S1),
	read_statement(L2, S2).
	
% component {Identifier:X} : {Identifier:Y} --> component(X, Y)
read_statement([H1,H2|T], compose(Var, Name)) :-
	token_data(H1, identifier, "component"),
	white_token(H2),
	token_partition(T, ":", [L1], [L2]),
	read_identifier(L1, Var),
	read_identifier(L2, Name).

% component {Identifier:Z} : {Identifier:X}.{Identifier:Y} --> component(Z,(X, Y))
read_statement([H1,H2|T], compose(Var, (Module, Name))) :-
	token_data(H1, identifier, "component"),
	white_token(H2),
	token_partition(T, ":", [L], [T1, T2, T3]),
	read_identifier(L, Var),
	read_identifier(T1, Module),
	token_data(T2, symbols, "."),
	read_identifier(T3, Name).

% Identifier.SimpleHeader <+> SimpleHeader --> multibind(Parent, Slot, Child, I1, I2)
read_statement(L, multibind(Parent, Slot, Child, I1, I2)) :-
	token_partition(L, "<+>", L1, L2),
	read_call(L1, fcall((Parent, Slot), I1, 0)),
	read_call(L2, fcall(Child, I2, 0)).

% Identifier.SimpleHeader <-> SimpleHeader --> bind(Parent, Slot, Child, I1, I2)
read_statement(L, bind(Parent, Slot, Child, I1, I2)) :-
	token_partition(L, "<->", L1, L2),
	read_call(L1, pcall((Parent, Slot), I1)),
	read_call(L2, pcall(Child, I2)).

% Member
read_statement(Tokens, Value) :-
	read_membership(Tokens, Value).

% else --> else
read_statement([Token], else) :-
	token_data(Token, identifier, "else").
	
% if Condition then  --> if_(C)
read_statement([H1,H2|T], if_(Condition)) :-
	token_data(H1, identifier, "if"),
	white_token(H2),
	append(Rest, [L1, L2], T),
	token_data(L2, identifier, "then"),
	white_token(L1),
	read_condition(Rest, Condition).
	
% if Condition then Statement --> if_then(C, S)
read_statement([H1,H2|T], if_then(C, S)) :-
	token_data(H1, identifier, "if"),
	white_token(H2),
	spaced_token_partition(T, "then", Condition, Then),
	read_condition(Condition, C),
	read_statement(Then, S).

% if Condition then Statement  else Statement --> if_then_else(C, T, E)
read_statement([H1,H2|T], if_then_else(C, TS, ES)) :-
	token_data(H1, identifier, "if"),
	white_token(H2),
	spaced_token_partition(T, "then", Condition, Remaining),
	read_condition(Condition, C), !,
	spaced_token_partition(Remaining, "else", Then, Else), !,
	read_statement(Then, TS),
	read_statement(Else, ES).
	
% Var := Expression --> set(V, E)
read_statement(Tokens, set(V, E)) :-
	token_partition(Tokens, ":=", [H], T),
	read_identifier(H, V),
	read_expression(T, E).

% while Condition --> while1(C)
read_statement([H1,H2|T], while1(C)) :-
	token_data(H1, identifier, "while"),
	white_token(H2),
	read_condition(T, C).

% while (Condition) Statement --> while2(C, S)
read_statement([H1,H2,H3|T], while2(Condition,Statement)) :-
	token_data(H1, identifier, "while"),
	white_token(H2),
	token_data(H3, open1, _),
	token_partition(T, close1, C, S),
	read_condition(C, Condition),
	read_statement(S, Statement).

% do --> do
read_statement([Token], do) :-
	token_data(Token, identifier, "do").

% for( List ) --> for1( List)
read_statement(Tokens, for1(List)) :-
	Tokens = [Head1,Head2|Tail],
	token_data(Head1, identifier, "for"),
	token_data(Head2, open1, _),
	last(Tail, Last),
	append(Rest, [Last], Tail),
	token_data(Last, close1, _),
	read_majorlist(Rest, List).

% for( List ) Statement  -> for2(List, Statement)
read_statement([H1,H2|T], for2(List, Statement)) :-
	token_data(H1, identifier, "for"),
	token_data(H2, open1, _),
	space_partition(T,ListTokens, StatementTokens),
	append(Rest, [Last], ListTokens),
	token_data(Last, close1, _),
	read_majorlist(Rest, List),
	read_statement(StatementTokens, Statement).


% Condition: fcall(N, I, 0)
read_statement(Tokens, Value) :-
	read_condition(Tokens, Value).

% Module Name --> def_module(V)
read_statement([T1, T2, T3], def_module(V)) :- 
	token_data(T1, identifier, "Module"),
	white_token(T2),
	read_identifier(T3, V).

% Build Name --> build(Name)
read_statement([H1, H2, H3], build(Name)) :- 
	token_data(H1, identifier, "Build"),
	white_token(H2),
	read_identifier(H3, Name).

% file FileName in Location : Call --> bfile(FileName, Header, Location)
read_statement([H1, H2|T], bfile(FileName, Header, Location)) :- 
	token_data(H1, identifier, "file"),
	white_token(H2),
	token_partition(T, "in", [F], R),
	read_variable(F, FileName),
	spaced_token_partition(R, ":", L, H),
	read_call(H, Header),
	read_list(L, Location).


% copy FileName in Location : copy(FileName) --> filecopy(FileName, CopyName, Location)
read_statement([H1,H2|T], filecopy(FileName, CopyName, Location)) :- 
	token_data(H1, identifier, "copy"),
	white_token(H2),
	token_partition(T, "in", [F], R),
	read_variable(F, FileName),
	spaced_token_partition(R, ":", L, [C]),
	read_variable(C, CopyName),
	read_list(L, Location).

% folder variable in Location -->  bfolder(Name, Location)
read_statement([H1, H2|T], bfolder(Name, Location)) :- 
	token_data(H1, identifier, "folder"),
	white_token(H2),
	spaced_token_partition(T, "in", [Token], L),
	read_variable(Token, Name),
	read_list(L, Location).

% include variable in Location --> binclude(Name, Location)
read_statement([H1, H2|T], binclude(Name, Location)) :- 
	token_data(H1, identifier, "include"),
	white_token(H2),
	spaced_token_partition(T, "in", [Token], L),
	read_variable(Token, Name),
	read_list(L, Location).

%
% ******************** PROJECT STATEMENTS ******************** 
%

% Prayog {Identifier:Name} --> prayog(Name)		The first line of the project, Identifies which build is to be build
read_pstatement([H1, H2, H3], prayog(V)) :- 
	token_data(H1, identifier, "Prayog"),
	white_token(H2),
	parse_identifier(H3, V).

% [ --> Begin(H)
read_pstatement([H], begin) :-
	token_data(H, open2, _).

% ] --> End(H)
read_pstatement([H], end) :-
	token_data(H, close2, _).

% | --> Alt(H)
read_pstatement([H], alt) :-
	token_data(H, symbols, "|").


% Target Identifier --> target(V)
read_pstatement([H1, H2, H3], target(V)) :- 
	token_data(H1, identifier, "Target"),
	white_token(H2),
	parse_identifier(H3, V).

% include "FileName" --> pinclude(Filename)
read_pstatement(L, pinclude(F)) :- 
	L= [H1,H2, H3],
	token_data(H1, identifier, "include"),
	white_token(H2),
	read_string([H3], F).

% {Identifier:V} <- {Data:D} --> set(V,D)
read_pstatement(L, set(V, D)) :- 
	token_partition(L, "<-", [H], T),
	parse_identifier(H, V),
	read_data(T,D).

% (Identifier:V} -> {Expression:E} --> report(V,E)
read_pstatement(L, report(V, E)) :- 
	token_partition(L, "->", [H], T),
	parse_identifier(H, V),
	read_expression(T, E).

