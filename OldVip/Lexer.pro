GLOBAL PREDICATES
	% Lexer.Pro
	determ tokenize_file(string, text, filetokens) - (i,i,o)

CONSTANTS
	comment_symbol = "#"
	block_open = '['
	block_close = ']'
	string_char = '\"'

PREDICATES
	determ tokenize_file0(string filename, integer line, text filetext, filetokens) - (i,i,i,o)
	determ tokenize_line(string, integer, integer, string, linetokens) - (i,i,i,i,o)
	nondeterm read_token(string, integer, integer, string, token, integer, string) - (i,i,i,i,o,o,o)
	
	determ remove_comment(linetokens, linetokens) - (i,o)
	determ remove_empty(filetokens, filetokens) - (i,o)
	determ join_lines(filetokens, filetokens) - (i,o)
	
	nondeterm read_integer(string, string, string)
	nondeterm read_white(string, string, string)
	nondeterm read_identifier(string, string, string)
	nondeterm read_string(string, string, string)
	nondeterm read_symbols(string, string, string)
	
CLAUSES

tokenize_file(FileName, FileText, FileTokens) :-
	tokenize_file0(FileName, 1, FileText, FileTokens1),
	join_lines(FileTokens1, FileTokens2),
	remove_empty(FileTokens2, FileTokens).
	

remove_empty([], []) :- !.
remove_empty([H|T], L) :- H = [ ], !, remove_empty(T, L).
remove_empty([H1|T1], [H2|T2]) :- token_trim(H1, H2), remove_empty(T1, T2).


join_lines([], []) :- !.
join_lines([H], []) :- last_token(H,Z, _), Z=token(_, _, _, symbols, "\\"), !, write("last line error").
join_lines([H], [H]) :- !.
join_lines([H1, H2|T], L) :- last_token(H1,Z, R), Z=token(_, _, _, symbols, "\\"), !, append_tokens(R, H2, H), join_lines([H|T], L).
join_lines([H|T1], [H|T2]) :- join_lines(T1, T2), !.

tokenize_file0(_, _, [], []) :- !.

tokenize_file0(FileName, N, [H1|T1], [H2|T2]) :-
	tokenize_line(FileName, N, 1, H1, L1), !,
	remove_comment(L1, L2), !,
	token_trim(L2, H2), % Trim White space
	NN = N+1,
	tokenize_file0(FileName, NN, T1, T2).

tokenize_file0(FileName, N, [_|T], L) :-
	NN = N+2,
	tokenize_file0(FileName, NN, T, L).


tokenize_line(_, _, _, "", []) :- !.

tokenize_line(F, N, P, S, [H|T]) :-
	read_token(F, N, P, S, H, PP, SS), !,
	tokenize_line(F, N, PP, SS, T).

tokenize_line(F, N, P, S, []) :-
	!, concat("Unable to parse txt \"", S, S0),
	concat(S0, "\"", S1),
	add_error3(F, N, P, S1),
	fail.

% Comment?
read_token(F, N, P, S, T, P, "") :-
	concat(comment_symbol, SS, S), !,
	T=token(F, N, P, comment, SS).

% Open1?
read_token(F, N, P, S, T, PP, R) :-
	concat("(", R, S), !,
	PP = P+1,
	T=token(F, N, P, open1, "").
	
% Close1?
read_token(F, N, P, S, T, PP, R) :-
	concat(")", R, S), !,
	PP = P+1,
	T=token(F, N, P, close1, "").

% Open2?
read_token(F, N, P, S, T, PP, R) :-
	frontchar(S, block_open, R), !,
	PP = P+1,
	T=token(F, N, P, open2, "").
	
% Close2?
read_token(F, N, P, S, T, PP, R) :-
	frontchar(S, block_close, R), !,
	PP = P+1,
	T=token(F, N, P, close2, "").

% Integer?
read_token(F, N, P, S, T, PP, R) :-
	frontchar(S,C,_),
	digit(C), !,
	read_integer(S, R, Z),
	str_len(Z, P0),
	PP = P + P0,
	T = token(F, N, P, int, Z).

% White Space?
read_token(F, N, P, S, T, PP, R) :-
	frontchar(S,C,_),
	white(C),
	read_white(S, R, Z), 
	T = token(F, N, P, white, Z),
	str_len(Z, P0),
	PP = P + P0.

% Identifier?
read_token(F, N, P, S, T, PP, R) :-
	frontchar(S,C,_),
	letter(C), !,
	read_identifier(S, R, Z),
	str_len(Z, P0),
	PP = P + P0,
	T = token(F, N, P, identifier, Z).

% String?
read_token(F, N, P, S, T, PP, R) :-
	frontchar(S, string_char, S0), !,
	read_string(S0, R, Z),
	str_len(Z, P0),
	PP = P + P0,
	T = token(F, N, P, str, Z).

% Symbols?
read_token(F, N, P, S, T, PP, R) :-
	read_symbols(S, R, Z),
	str_len(Z, P0),
	PP = P + P0,
	T = token(F, N, P, symbols, Z).


read_integer("", "", "") :- !.

read_integer(S, S, "") :-
	frontchar(S, C, _),
	not(digit(C)).

read_integer(S, R, Z) :-
	frontchar(S, C, S1),
	digit(C),
	str_char(S0, C),
	read_integer(S1, R, ZZ),
	concat(S0, ZZ, Z).


read_white("", "", "") :- !.

read_white(S, R, Z) :-
	frontchar(S, C, S1),
	white(C), !,
	str_char(S0, C),
	read_white(S1, R, ZZ),
	concat(S0, ZZ, Z).

read_white(S, S, "").

read_identifier("", "", "") :- !.

read_identifier(S, R, Z) :-
	frontchar(S, C, S1),
	alphanum(C), !,
	str_char(S0, C),
	read_identifier(S1, R, ZZ),
	concat(S0, ZZ, Z).

read_identifier(S, S, "") :-
	frontchar(S, C, _),
	not(alphanum(C)), !.


read_string("", "", "") :- fail.

read_string(S, R, "") :-
	frontchar(S, string_char, R), !.

read_string(S, R, Z) :-
	concat("\\\\", RR, S),
	read_string(RR, R, ZZ),
	concat("\\", ZZ, Z), !.

read_string(S, R, Z) :-
	concat("\\n", RR, S),
	read_string(RR, R, ZZ),
	concat("\n", ZZ, Z).

read_string(S, R, Z) :-
	concat("\\t", RR, S),
	read_string(RR, R, ZZ),
	concat("\t", ZZ, Z), !.

read_string(S, R, Z) :-
	concat("\\r", RR, S),
	read_string(RR, R, ZZ),
	concat("\r", ZZ, Z), !.

read_string(S, R, Z) :-
	frontchar(S, C1, SS),
	frontchar(SS, C2, RR),
	C1 = '\\', C2 = string_char, !,
	read_string(RR, R, ZZ),
	str_char(S0, string_char),
	concat(S0, ZZ, Z), !.

read_string(S, _, _) :-
	frontchar(S, C, _),
	C = '\\', !, fail.

read_string(S, R, Z) :-
	frontchar(S, C, S1),
	str_char(S0, C),
	read_string(S1, R, ZZ),
	concat(S0, ZZ, Z).
	
read_symbols("", "", "") :- !.

read_symbols(S, S, "") :- frontchar(S, C, _), alphanum(C), !.

read_symbols(S, S, "") :- frontchar(S, C, _), white(C), !.

read_symbols(S, S, "") :- concat( comment_symbol, _, S), !.

read_symbols(S, S, "") :- frontchar(S, block_open, _), !.

read_symbols(S, S, "") :- frontchar(S, block_close, _), !.

read_symbols(S, S, "") :- frontchar(S, '(', _), !.

read_symbols(S, S, "") :- frontchar(S, ')', _), !.

read_symbols(S, S, "") :- frontchar(S, string_char, _), !.

read_symbols(S, R, Z) :- !,
	frontchar(S, C, S1),
	str_char(S0, C),
	read_symbols(S1, R, ZZ),
	concat(S0, ZZ, Z).

remove_comment(L, []) :-
	L=[H],
	token_data(H, comment, _), !.

remove_comment(L1, L2) :-
	last_token(L1, Z1, R1),
	last_token(R1, Z2, L2),
	token_data(Z1, comment, _), 
	white_token(Z2), !.

remove_comment([H|T], []) :-
	last_token([H|T], Z, _R),
	token_data(Z, comment, _), !,
	H=token(F,  N, _, _, _),
	assert(err2(F,N,"Comments must preced with space")).
	
remove_comment(L, L) :- !.
