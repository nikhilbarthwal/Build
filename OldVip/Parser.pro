GLOBAL PREDICATES
	determ parse_module(filetokens,  statementslist) - (i,o)
	determ parse_project(filetokens,  statementslist) - (i,o)
	
PREDICATES
	nondeterm parse_identifier(token, identifier) - (i,o)
	nondeterm parse_identifierlist(linetokens, identifierlist) - (i,o)
	nondeterm parse_number(linetokens, number) - (i,o)
	nondeterm parse_variable(linetokens, variable) - (i,o)
	nondeterm parse_string(linetokens, str) - (i,o)
	nondeterm parse_list(linetokens, expr) - (i,o)
	nondeterm parse_data(linetokens, data) - (i,o)
	nondeterm parse_datalist(linetokens, datalist) - (i,o)
	nondeterm parse_header(linetokens, identifier, identifierlist) - (i,o,o)
	nondeterm parse_call(linetokens, identifier, exprlist) - (i,o,o)
	nondeterm parse_port(linetokens, port) - (i,o)
	nondeterm parse_portlist(linetokens, portlist) - (i,o)
	nondeterm parse_membership(linetokens, member) - (i,o)
	nondeterm parse_minorlist(linetokens, minorlist) - (i,o)
	nondeterm parse_majorlist(linetokens, majorlist) - (i,o)
	nondeterm parse_condition(linetokens, condition) - (i,o)
	nondeterm parse_expr(linetokens, expr) - (i,o)
	nondeterm parse_exprlist(linetokens, exprlist) - (i,o)
	nondeterm parse_statement(linetokens, mstatement) - (i,o)
	nondeterm parse_pstatement(linetokens, pstatement) - (i,o)
	
CLAUSES

	parse_module([], []).
	parse_module([FH|FT], [SH|ST]) :-
		parse_statement(FH, S), !,
		FH=[H|_],
		H = token(FileName, Line, _, _,_),
		SH = m(FileName, Line, S),
		parse_module(FT, ST).
	
	parse_module([FH|FT], L) :-
		FH=[H|_],
		H = token(FileName, Line, _, _,_),
		add_error2(FileName, Line, "Unable to parse the statement"),
		parse_module(FT, L).
	
	parse_project([], []).
	parse_project([FH|FT], [SH|ST]) :-
		parse_pstatement(FH, S), !,
		FH=[H|_],
		H = token(FileName, Line, _, _,_),
		SH = p(FileName, Line, S),
		parse_project(FT, ST).
	
	parse_project([FH|FT], L) :-
		FH=[H|_],
		H = token(FileName, Line, _, _,_),
		add_error2(FileName, Line, "Unable to parse the statement"),
		parse_project(FT, L).

% ----- Identifier ----- 
parse_identifier(H, identifier(Z, H)) :- 
	token_data(H, identifier, Z).

% Identifier List: I1 I2 I3 ..  --> [ I1, I2, I2 .. ]
parse_identifierlist(L, []) :-
	token_chop(L, []).

parse_identifierlist(L, [A|B]) :-
	token_chop(L, LL),
	LL=[H|T],
	parse_identifier(H, A),
	parse_identifierlist(T, B).

parse_identifierlist(L, [A]) :-
	token_chop(L,LL),
	LL=[H],
	parse_identifier(H, A).

%
% ----- Number -----
%

% +xxxxx --> number(Value, Token)
parse_number([H1,H2], number(V, H1)) :-
	token_data(H1, symbols, "+"),
	token_data(H2, int, S),
	str_int(S,V).

% -xxxxx --> number(Value, Token)
parse_number([H1,H2], number(V, H1)) :-
	token_data(H1, symbols, "-"),
	token_data(H2, int, S),
	str_int(S, VV),
	V = -1*VV.

parse_number([H], number(V, H)) :-
	token_data(H, int, S),
	str_int(S,V).

% ----- Variable -----
parse_variable([H], variable(Z, H)) :-
	token_data(H, identifier, Z).

% "xx xx xx" --> str(Value, Token)
parse_variable([H], str(Z, H)) :-
	token_data(H, str, Z).


% ----- String -----

% "xx xx xx" --> str(Value, Token)
parse_string([H], str(Z, H)) :-
	token_data(H, str, Z).
	
% ----- List -----

% [data1 data2 data3] --> list(Value, Token)
parse_list([H|T], list(V)) :-
	last_token(T, L, R),
	token_data(H, open2, _),
	token_data(L, close2, _),
	parse_exprlist(R, V).


% ----  Data -----
% Data: String / List / Number 
parse_data(L, number(V,H)) :- parse_number(L,Z), Z=number(V,H).
parse_data(L, str(V,H)) :- parse_string(L,Z), Z=str(V,H).
parse_data(L, list(V)) :-
	L=[H|T],
	last_token(T, Z, R),
	token_data(H, open2, _),
	token_data(Z, close2, _),
	parse_datalist(R, V).

parse_datalist(L, []) :- 
	token_chop(L, []).

parse_datalist(L, [DH|DT]) :-
	token_spacial_partition(L, H, T),
	parse_data(H, DH),
	parse_datalist(T, DT).

parse_datalist(L, [H]) :-
	parse_data(L, H).


% ----- Header -----
parse_header([H1,H2|T], N, A) :-
	parse_identifier(H1, N),
	token_data(H2, open1, _D),
	last_token(T,L,R),
	token_data(L, close1, _D),
	parse_identifierlist(R, A).

% ----- Call -----
parse_call([H1,H2|T], N, E) :-
	parse_identifier(H1, N),
	token_data(H2, open1, _D),
	last_token(T,L,R),
	token_data(L, close1, _D),
	parse_exprlist(R, E).

% ----- Membership -----
parse_membership(L, member(V, E)) :-
	token_spaced_partition(L, identifier, "in", L1, L2),
	L1 =[H],
	parse_identifier(H, V),
	parse_expr(L2, E).

% ----- Minor List -----

% Membership1 ; Membership2 ; Membership3  --> [ M1 M2 M3]
parse_minorlist([], []).
parse_minorlist(L, [H|T]) :-
	token_partition(L, symbols, ",", L1, L2), 
	parse_membership(L1, H),
	parse_minorlist(L2, T).
parse_minorlist(L, [H]) :-
	token_trim(L, LL),
	parse_membership(LL, H).

% ----- Major List -----

% MinorList1 ; MinorList2 ; MinorList3  --> [ M1 M2 M3]
parse_majorlist([], []).
parse_majorlist(L, [H|T]) :-
	token_partition(L, symbols, ";", L1, L2),
	parse_minorlist(L1, H),
	parse_majorlist(L2, T).
parse_majorlist(L, [H]) :-
	token_trim(L, LL),
	parse_minorlist(LL, H).

% ----- Condition -----

% E1 in E2
parse_condition(L, member(E1, E2)) :- 
	token_spaced_partition(L, identifier, "in", L1, L2),
	parse_expr(L1, E1),
	parse_expr(L2, E2).

% if C1 then C2 --> cond_if1(C1, C2)
parse_condition([H1, H2|T], cond_if1(C1, C2)) :-
	token_spaced_partition(T, identifier, "then", L1, L2),
	token_data(H1, identifier, "if"),
	white_token(H2),
	parse_condition(L1, C1),
	parse_condition(L2, C2).

% if C1 then C2 else C3 --> cond_if2(C1, C2, C2)
parse_condition([H1, H2|T], cond_if2(C1, C2, C3)) :-
	token_spaced_partition(T, identifier, "then", L1, L),
	token_spaced_partition(L, identifier, "else", L2, L3),
	token_data(H1, identifier, "if"),
	white_token(H2),
	parse_condition(L1, C1),
	parse_condition(L2, C2),
	parse_condition(L3, C3).

% A and B --> cond_and(A, B)
parse_condition(L, cond_and(V1, V2)) :-
	token_spaced_partition(L, identifier, "and", L1, L2),
	parse_condition(L1, V1),
	parse_condition(L2, V2).

% A or B --> cond_or(A, B)
parse_condition(L, cond_or(V1, V2)) :-
	token_spaced_partition(L, identifier, "or", L1, L2),
	parse_condition(L1, V1),
	parse_condition(L2, V2).

% A < B --> lt(A, B)
parse_condition(L, lt(V1, V2)) :-
	token_partition(L, symbols, "<", L1, L2),
	parse_expr(L1, V1),
	parse_expr(L2, V2).
	
% A > B --> gt(A, B)
parse_condition(L, gt(V1, V2)) :-
	token_partition(L, symbols, ">", L1, L2),
	parse_expr(L1, V1),
	parse_expr(L2, V2).

% A => B --> ge(A, B)
parse_condition(L, ge(V1, V2)) :-
	token_partition(L, symbols, "=>", L1, L2),
	parse_expr(L1, V1),
	parse_expr(L2, V2).

% A >= B --> ge(A, B)
parse_condition(L, ge(V1, V2)) :-
	token_partition(L, symbols, ">=", L1, L2),
	parse_expr(L1, V1),
	parse_expr(L2, V2).

% A <= B --> le(A, B)
parse_condition(L, le(Value1, Value2)) :-
	token_partition(L, symbols, "<=", List1, List2),
	parse_expr(List1, Value1),
	parse_expr(List2, Value2).

% A =< B --> le(A, B)
parse_condition(L, le(V1, V2)) :-
	token_partition(L, symbols, "=<", L1, L2),
	parse_expr(L1, V1),
	parse_expr(L2, V2).

% A = B --> eq(A, B)
parse_condition(L, eq(V1, V2)) :-
	token_partition(L, symbols, "=", L1, L2),
	parse_expr(L1, V1),
	parse_expr(L2, V2).

% A != B --> ne(A, B)
parse_condition(L, ne(V1, V2)) :-
	token_partition(L, symbols, "!=", L1, L2),
	parse_expr(L1, V1),
	parse_expr(L2, V2).

% Condition Call: N(A1 A2 A3 A4 ... )
parse_condition(L, call(N, A)) :- 
	parse_call(L, N, A), !.

% not(C) --> cond_not(C)
parse_condition([H1,H2|T], cond_not(V)) :-
	token_data(H1, identifier, "not"),
	token_data(H2, open1, _),
	last_token(T, L, R),
	token_data(L, close1, _),
	parse_condition(R, V).

% (C) --> C
parse_condition([H|T], V) :-
	token_data(H, open1, _),
	last_token(T, L, R),
	token_data(L, close1, _),
	parse_condition(R, V).

% ----- Expression -----

% if C then E1 else E2 --> expr_if(C, E1, E2)
parse_expr([H1, H2 |T], expr_if(C, E1, E2)) :-
	token_data(H1, identifier, "if"),
	white_token(H2),
	token_spaced_partition(T, str, "then", CC, R),
	token_spaced_partition(R, str, "else", L1, L2),
	parse_condition(CC, C),
	parse_expr(L1, E1),
	parse_expr(L2, E2).

% Exp1 + Exp2 --> add(Exp1, Exp2)
parse_expr(L, add(V1, V2)) :-
	token_partition(L, symbols, "+", L1, L2),
	parse_expr(L1, V1),
	parse_expr(L2, V2).
	
% Exp1 - Exp2 --> sum(Exp1, Exp2)
parse_expr(L, sub(V1, V2)) :-
	token_partition(L, symbols, "-", L1, L2),
	parse_expr(L1, V1),
	parse_expr(L2, V2).

% Exp1 * Exp2 --> multiply(Exp1, Exp2)
parse_expr(L, multiply(V1, V2)) :-
	token_partition(L, symbols, "*", L1, L2),
	parse_expr(L1, V1),
	parse_expr(L2, V2).

% Exp1 / Exp2 --> divide(Exp1, Exp2)
parse_expr(L, divide(V1, V2)) :-
	token_partition(L, symbols, "/", L1, L2),
	parse_expr(L1, V1),
	parse_expr(L2, V2).

% Exp1 % Exp2 --> modulus(Exp1, Exp2)
parse_expr(L, modulus(V1, V2)) :-
	token_partition(L, symbols, "%", L1, L2),
	parse_expr(L1, V1),
	parse_expr(L2, V2).

% Exp1 ^ Exp2 --> concat(Exp1, Exp2)
parse_expr(L, concat(V1, V2)) :-
	token_partition(L, symbols, "^", L1, L2),
	parse_expr(L1, V1),
	parse_expr(L2, V2).

% Exp1 & Exp2 --> join(Exp1, Exp2)
parse_expr(L, join(V1, V2)) :-
	token_partition(L, symbols, "&", L1, L2),
	parse_expr(L1, V1),
	parse_expr(L2, V2).

% Exp1 ++ Exp2 --> add_list(Exp1, Exp2)
parse_expr(L, add_list(V1, V2)) :-
	token_partition(L, symbols, "++", L1, L2),
	parse_expr(L1, V1),
	parse_expr(L2, V2).

% Exp1 .. Exp2 --> gen(Exp1, Exp2)
parse_expr(L, generate(V1, V2)) :-
	token_partition(L, symbols, "..", L1, L2),
	parse_expr(L1, V1),
	parse_expr(L2, V2).

% map[ MajorList : Expr ?-  Cond ] --> map2(MajorList, Expr, Cond)
parse_expr([H1,H2|T], map2(M, E, C)) :-
	last_token(T, L, R),
	token_data(L, close2, _),
	token_partition(R, symbols, ":", MT, L2),
	token_spaced_partition(L2, symbols, "?-", ET, CT),
	token_data(H1, identifier, "map"),
	token_data(H2, open2, _),
	parse_majorlist(MT, M), 
	parse_expr(ET, E),
	parse_condition(CT, C).

% map[ MajorList : Expression ] --> map1(MajorList, Expr)
parse_expr([H1,H2|T], map1(M, E)) :-
	last_token(T, L, R),
	token_spaced_partition(R, symbols, ":", MT, ET),
	token_data(L, close2, _),
	token_data(H1, identifier, "map"),
	token_data(H2, open2, _),
	parse_majorlist(MT, M),
	parse_expr(ET, E).

% { Expression }
parse_expr([H|T], enum(V)) :-
	token_data(H, symbols, "{"),
	last_token(T, L, R),
	token_data(L, symbols, "}"),
	parse_expr(R, V).

% call(identifier, exprlist): N(E1 E2 E3 ... )
parse_expr(L, call(N,E)) :-
	parse_call(L,N,E).
	
% mcall(identifier, identifier, exprlist): M.N(E1 E2 E3 ... )
parse_expr([H1,H2|T], mcall(M,N,E)) :-
 	parse_identifier(H1, M),
	token_data(H2, symbols, "."),
	parse_call(T,N,E).
	
% Data --> variable(Name, Token) ; Num(V, H) ; list(L, H)
parse_expr(L, variable(V,H)) :- parse_variable(L,Z), Z=variable(V,H).
parse_expr(L, number(V,H)) :- parse_number(L,Z), Z=number(V,H).
parse_expr(L, list(V)) :- parse_list(L,Z), Z=list(V).
parse_expr(L, str(V,H)) :- parse_string(L,Z), Z=str(V,H).


% (Expression) --> Expression
parse_expr([H|T], E) :-
	token_data(H, open1, _),
	last_token(T, L, R),
	token_data(L, close1, _),
	token_trim(R, V),
	parse_expr(V,E).

% ----- Expression List -----
parse_exprlist([], []).
parse_exprlist(L, [H|T]) :-
	token_spacial_partition(L, L1, L2),
	parse_expr(L1, H),
	parse_exprlist(L2, T).
parse_exprlist(L, [H]) :-
	parse_expr(L, H).

% ----- Port -----
parse_port(L, expr(E)) :- !, parse_expr(L,E).
parse_port([H1,H2], port(identifier(E,H1))) :- !, token_data(H1, symbols, "@"), token_data(H2, identifier, E).

% ----- Port List -----
parse_portlist([], []).
parse_portlist(L, [H|T]) :-
	token_spacial_partition(L, L1, L2),
	parse_port(L1, H),
	parse_portlist(L2, T).
parse_portlist(L, [H]) :-
	parse_port(L, H).

% ----- Statement -----


% [ --> begin(Token)
parse_statement([Z], begin) :-
	token_data(Z, open2, _).

% ] --> end(Token)
parse_statement([Z], end) :-
	token_data(Z, close2, _).

% | --> alt(Token)
parse_statement([Z], alt) :-
	token_data(Z, symbols, "|").

% Function Header --> function(N, I)
parse_statement(L, function(N, I)) :-
	L=[H1,H2|T],
	token_data(H1,identifier,"Function"),
	white_token(H2),
	parse_header(T, N, I).


% Condition {Header:Name( Inp1 Inp2 .. )} --> condition(Name, Input)
parse_statement([H1,H2|T], condition(N, A)) :-
	token_data(H1, identifier, "Condition"),
	white_token(H2),
	parse_header(T,N,A), !.

% Export {Variable:V} : {Expression:e} --> export(V, E)
parse_statement([H1,H2|T], export(Var, Expr)) :-
	token_data(H1, identifier, "Export"),
	white_token(H2),
	token_partition(T, symbols, ":", L, E),
	L=[H3],
	parse_identifier(H3, Var),
	parse_expr(E, Expr).

% Import {Variable:V1} : {Variable:V2} --> import(V1 V2)
parse_statement([H1,H2|T], import(Var1, Var2)) :-
	token_data(H1, identifier, "Import"),
	white_token(H2),
	token_partition(T, symbols, ":", L1, L2),
	L1=[V1], L2=[V2],
	parse_identifier(V1, Var1),
	parse_identifier(V2, Var2).

% Component header --> component(Name, Input, Ouput)
parse_statement([H1,H2,H3,H4|T], component(identifier(N, H3), I, O)) :-
	token_data(H1, identifier, "Component"),
	white_token(H2),
	token_data(H3, identifier, N),
	token_data(H4, open1, _),
	token_partition(T, symbols, ":", L1, L2),
	last_token(L1, D, R),
	token_data(D, close1, _),
	parse_identifierlist(R, I),
	parse_identifierlist(L2, O).

% Component header --> component(Name, Input, Ouput)
parse_statement([H1,H2,H3,H4|T], component(identifier(N, H3), I, [])) :-
	token_data(H1, identifier, "Component"),
	white_token(H2),
	token_data(H3, identifier, N),
	token_data(H4, open1, _),
	last_token(T, D, R),
	token_data(D, close1, _),
	parse_identifierlist(R, I).

%Compose header -->  compose(Name, Input, Ouput)
parse_statement([H1,H2,H3,H4|T], compose(identifier(N, H3),I,O)) :-
	token_data(H1, identifier, "Compose"),
	white_token(H2),
	token_data(H3, identifier, N),
	token_data(H4, open1, _),
	token_partition(T, symbols, ":", L1, L2),
	last_token(L1, D, R),
	token_data(D, close1, _),
	parse_identifierlist(R, I),
	parse_identifierlist(L2, O).

%Compose header -->  compose(Name, Input, Ouput)
parse_statement([H1,H2,H3,H4|T], compose(identifier(N, H3),I,[])) :-
	token_data(H1, identifier, "Compose"),
	white_token(H2),
	token_data(H3, identifier, N),
	token_data(H4, open1, _),
	last_token(T, D, R),
	token_data(D, close1, _),
	parse_identifierlist(R, I).

%-Header --> slot(Name, Input, Output)
parse_statement([H| T], slot(N,I,O)) :-
	token_data(H, symbols, "-"),
	token_partition(T, symbols, ":", L1, L2),
	parse_header(L1,N,I),
	parse_identifierlist(L2,O).

%-Header --> slot(Name, Input, Output)
parse_statement([H|T], slot(N,I,[])) :-
	token_data(H, symbols, "-"),
	parse_header(T,N,I).

%+Header --> def_multislot(Name, Input, Output)
parse_statement([H|T], multislot(N, I, O)) :-
	token_data(H, symbols, "+"),
	token_partition(T, symbols, ":", L1, L2),
	parse_header(L1,N,I),
	parse_identifierlist(L2,O).

%+Header --> def_multislot(Name, Input, Output)
parse_statement([H|T], multislot(N, I, [])) :-
	token_data(H, symbols, "+"),
	parse_header(T,N,I).

% Statement1 | Statement2 --> statement_or(Statement1, Statement2)
parse_statement(L, statement_or(S1, S2)) :-
	token_partition(L, symbols, "|", L1, L2),
	not(L1 = []),
	not(L2 = []),
	parse_statement(L1, S1),
	parse_statement(L2, S2).

% Statement1 ; Statement2 --> statement_and(Statement1, Statement2)
parse_statement(L, statement_and(S1, S2)) :-
	token_partition(L, symbols, ";", L1, L2),
	not(L1 = []),
	not(L2 = []),
	parse_statement(L1, S1),
	parse_statement(L2, S2).
	
% component {Identifier:X} : {Identifier:Y} --> component(X, Y)
parse_statement([H1,H2|T], component1(V, N)) :-
	token_data(H1, identifier, "component"),
	white_token(H2),
	token_partition(T, symbols, ":", L1, L2),
	L1=[V1], L2=[V2],
	parse_identifier(V1, V),
	parse_identifier(V2, N).

% component {Identifier:Z} : {Identifier:X}.{Identifier:Y} --> component(Z,(X, Y))
parse_statement([H1,H2|T], component2(V,M,N)) :-
	token_data(H1, identifier, "component"),
	white_token(H2),
	token_partition(T, symbols, ":", D1, D2),
	D1 = [L], D2=[T1, T2, T3],
	parse_identifier(L, V),
	parse_identifier(T1, M),
	token_data(T2, symbols, "."),
	parse_identifier(T3, N).

% Identifier.SimpleHeader <+> SimpleHeader --> multibind(Parent, Slot, Child, I1, I2)
parse_statement([H1,H2,H3,H4|T], multibind(P, S, C, I1, I2)) :-
	parse_identifier(H1, P),
	token_data(H2, symbols, "."),
	parse_identifier(H3, S),
	token_data(H4, open1, _),
	token_partition(T, symbols, "<+>", L1, L2),
	last_token(L1,LL1,IL1),
	token_data(LL1, close1, _),
	parse_portlist(IL1, I1),
	L2=[F1,F2|T2],
	parse_identifier(F1, C),
	token_data(F2, open1, _),
	last_token(T2,LL2,IL2),
	token_data(LL2, close1, _),
	parse_portlist(IL2, I2).

% Identifier.SimpleHeader <-> SimpleHeader --> bind(Parent, Slot, Child, I1, I2)
parse_statement([H1,H2,H3,H4|T], bind(P, S, C, I1, I2)) :-
	parse_identifier(H1, P),
	token_data(H2, symbols, "."),
	parse_identifier(H3, S),
	token_data(H4, open1, _),
	token_partition(T, symbols, "<->", L1, L2),
	last_token(L1,LL1,IL1),
	token_data(LL1, close1, _),
	parse_portlist(IL1, I1),
	L2=[F1,F2|T2],
	parse_identifier(F1, C),
	token_data(F2, open1, _),
	last_token(T2,LL2,IL2),
	token_data(LL2, close1, _),
	parse_portlist(IL2, I2).

% Member
parse_statement(L, member(V,E)) :-
	parse_membership(L, M), M=member(V,E), !.

% else --> else
parse_statement([Token], statement_else) :-
	token_data(Token, identifier, "else").
	
% if Condition then  --> statement_if(C)
parse_statement([H1,H2|T], statement_if(Condition)) :-
	token_data(H1, identifier, "if"),
	white_token(H2),
	last_token(T, L1, RR),
	last_token(RR, L2, R),
	token_data(L1, identifier, "then"),
	white_token(L2),
	parse_condition(R, Condition).
	
% if Condition then Statement --> statement_if_then(C, S)
parse_statement([H1,H2|T], statement_if_then(C, S)) :-
	token_data(H1, identifier, "if"),
	white_token(H2),
	token_spaced_partition(T, identifier, "then", Condition, Then),
	parse_condition(Condition, C),
	parse_statement(Then, S).

% if Condition then Statement  else Statement --> if_then_else(C, T, E)
parse_statement([H1,H2|T], statement_if_then_else(C, TS, ES)) :-
	token_data(H1, identifier, "if"),
	white_token(H2),
	token_spaced_partition(T, identifier, "then", Condition, Remaining),
	parse_condition(Condition, C), !,
	token_spaced_partition(Remaining, identifier, "else", Then, Else),
	parse_statement(Then, TS),
	parse_statement(Else, ES).
	
% Var := Expression --> set(V, E)
parse_statement(Tokens, set(V, E)) :-
	token_partition(Tokens, symbols, ":=", L, T),
	L=[H],
	parse_identifier(H, V),
	parse_expr(T, E).

% [ V1, V2 ] := Expression --> mset1(V, N, E)
parse_statement(Tokens, mset1(V, N, E)) :-
	token_partition(Tokens, symbols, ":=", L1, L2),
	L1=[H1|T1],
	last_token(T1, Z1, R1),
	token_data(H1, open1, _),
	token_data(Z1, close2, _),
	parse_identifierlist(R1, V),
	parse_call(L2, N, E).

% [ V1, V2 ] := Expression --> mset2(V, M, N, E)
parse_statement(Tokens, mset2(V, M, N, E)) :-
	token_partition(Tokens, symbols, ":=", L1, L2),
	L1=[H1|T1],
	last_token(T1, Z1, R1),
	token_data(H1, open1, _),
	token_data(Z1, close2, _),
	parse_identifierlist(R1, V),
	L2=[H21,H22|T2],
	parse_identifier(H21, M),
	token_data(H22, symbols, "."),
	parse_call(T2, N, E).

	
% while Condition --> while1(C)
parse_statement([H1,H2|T], statement_while1(C)) :-
	token_data(H1, identifier, "while"),
	white_token(H2),
	parse_condition(T, C), !.

% while (Condition) Statement --> while2(C, S)
parse_statement([H1,H2,H3|T], statement_while2(Condition,Statement)) :-
	token_data(H1, identifier, "while"),
	white_token(H2),
	token_data(H3, open1, _),
	token_partition(T, close1, "",C, S),
	parse_condition(C, Condition),
	parse_statement(S, Statement).

% do --> do
parse_statement([Token], statement_do) :-
	token_data(Token, identifier, "do").

% for( List ) --> for1( List)
parse_statement(Tokens, statement_for1(List)) :-
	Tokens = [Head1,Head2|Tail],
	token_data(Head1, identifier, "for"),
	token_data(Head2, open1, _),
	last_token(Tail, Last, Rest),
	token_data(Last, close1, _),
	parse_majorlist(Rest, List).

% for( List ) Statement  -> for2(List, Statement)
parse_statement([H1,H2|T], statement_for2(List, Statement)) :-
	token_data(H1, identifier, "for"),
	token_data(H2, open1, _),
	token_spacial_partition(T, ListTokens, StatementTokens),
	last_token(ListTokens, Last, Rest),
	token_data(Last, close1, _),
	parse_majorlist(Rest, List),
	parse_statement(StatementTokens, Statement).

% Condition:
parse_statement(T, cond(V)) :-
	parse_condition(T, V).

% Module Name --> def_module(V)
parse_statement([T1, T2, T3], module(V)) :- 
	token_data(T1, identifier, "Module"),
	white_token(T2),
	parse_identifier(T3, V).

% Build Name --> build(Name)
parse_statement([H1, H2, H3], build(Name)) :- 
	token_data(H1, identifier, "Build"),
	white_token(H2),
	parse_identifier(H3, Name), !.

% file FileName in Location : Call --> bfile(FileName, Name, ExprList, Location)
parse_statement([H1, H2|T], bfile(FN, H, E, D)) :- 
	token_data(H1, identifier, "file"),
	white_token(H2),
	token_partition(T, identifier, "in", F, R),
	parse_variable(F, FN),
	token_partition(R, symbols, ":", L, C), 
	parse_call(C, H, E),
	parse_exprlist(L, D).

% copy FileName in Location : copy(FileName) --> filecopy(FileName, CopyName, Location)
parse_statement([H1,H2,H3,H4,H5|T], filecopy(F, CN, D)) :- 
	token_data(H1, identifier, "copy"),
	white_token(H2),
	parse_variable([H3], F),
	white_token(H4),
	token_data(H5, identifier, "in"),
	token_partition(T, symbols, ":", L, C),
	parse_variable(C, CN),
	parse_exprlist(L, D).

% folder variable in Location -->  bfolder(Name, Location)
parse_statement([H1, H2|T], bfolder(N, L)) :- 
	token_data(H1, identifier, "folder"),
	white_token(H2),
	token_spaced_partition(T, identifier, "in", Z, LL),
	parse_variable(Z, N),
	parse_exprlist(LL,L).

% include variable in Location --> binclude(Name, Location)
parse_statement([H1, H2|T], binclude(N, L)) :- 
	token_data(H1, identifier, "include"),
	white_token(H2),
	token_spaced_partition(T, identifier, "in", ZZ, LL),
	ZZ=[Z],
	parse_identifier(Z, N),
	parse_exprlist(LL, L).

%
% ******************** PROJECT STATEMENTS ******************** 
%

% Prayog {Identifier:Name} --> prayog(Name)		The first line of the project, Identifies which build is to be build
parse_pstatement([H1, H2, H3], project(V)) :- 
	token_data(H1, identifier, "Project"),
	white_token(H2),
	parse_identifier(H3, V).

% [ --> Begin(H)
parse_pstatement([H], begin) :-
	token_data(H, open2, _).

% ] --> End(H)
parse_pstatement([H], end) :-
	token_data(H, close2, _).

% | --> Alt(H)
parse_pstatement([H], alt) :-
	token_data(H, symbols, "|").


% Target Identifier --> target(V)
parse_pstatement([H1, H2, H3], target(V)) :- 
	token_data(H1, identifier, "Target"),
	white_token(H2),
	parse_identifier(H3, V).

% include "FileName" --> pinclude(Filename)
parse_pstatement(L, pinclude(F)) :- 
	L= [H1,H2, H3],
	token_data(H1, identifier, "include"),
	white_token(H2),
	parse_string([H3], F).

% {Identifier:V} <- {Data:D} --> set(V,D)
parse_pstatement(L, set(V, D)) :- 
	token_partition(L, symbols, "<-", HH, T),
	HH=[H],
	parse_identifier(H, V),
	parse_data(T,D).

% (Identifier:V} -> {Expression:E} --> report(V,E)
parse_pstatement(L, report(V, V1, V2)) :- 
	token_partition(L, symbols, "->", T1, T2),
	T1=[H],
	T2=[H1,H2,H3],
	parse_identifier(H, V),
	parse_identifier(H1, V1),
	token_data(H2, symbols, "."),
	parse_identifier(H3, V2).

