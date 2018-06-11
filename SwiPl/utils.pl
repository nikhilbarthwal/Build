:- module(utils, [read_file/2, token_data/3, trim_space/2, trim_first_space/2, trim_last_space/2,
   trim_all_space/2, print_token/1, count_error, reset_error, space_partition/3, token_partition/4,
   spaced_token_partition/4, tight_token_partition/4, white_token/1, tokens_atom/2, count/3,
   couple/2, group/3]).


% read_file(+FileName, -FileCodes): Read the FileName line by line, and return FileCodes as list of list of codes.
% 	Each line is FileName is converted into a line of codes. Error handling mechanism is build in.

read_file(FileName, FileCodes) :-
	catch(read_file_(FileName, FileCodes), _Error, fail).

read_file(FileName, _FileCodes) :-
	write('ERROR '),
	write(FileName),
	writeln(' : Unable to read file'),
	fail.


% read_file_(+FileName, -FileCodes): Read the FileName line by line, and return FileCodes as list of list of codes.
% 	Each line is FileName converted into a line of codes. No error handling is provided.
		
read_file_(FileName, FileCodes) :-
	open(FileName, read, Stream),
	read_codes(FileName, 1, Stream, FileCodes),
	close(Stream).	


% read_codes(+FileName, +Stream, -FileCodes): The stream from where the data is read andconverted into List of List of Codes.

read_codes(_, _, Stream, []) :-
	at_end_of_stream(Stream).

read_codes(FileName, LineNo, Stream , [FirstLine|RestLines]) :-
	\+ at_end_of_stream(Stream),
	read_line_to_codes(Stream,FirstLine),
	trim_codes(FirstLine, TextCodes),
	atom_codes(Text, TextCodes), 
	assertz(source(FileName, LineNo, Text)),
	NextLine is LineNo +1,
	read_codes(FileName, NextLine, Stream, RestLines).

trim_codes(Input, Output) :-
	trim_front(Input, Codes),
	trim_back(Codes, Output).
	
trim_front([H|T], Z) :-
	char_code(C, H),
	is_white(C),
	trim_front(T,Z).
 
trim_front(Z, Z).

trim_back(X,Y):-
	reverse(X, R),
	trim_front(R, Z),
	reverse(Z, Y).

% token_data(+Token, -Type, -TokenData)

token_data(token(_, _, _, comment(Data)), comment, Data).
token_data(token(_, _, _, open1), open1, []).
token_data(token(_, _, _, open2), open2, []).
token_data(token(_, _, _, close1), close1, []).
token_data(token(_, _, _, close2), close2, []).
token_data(token(_, _, _, int(Data)), int, Data).
token_data(token(_, _, _, str(Data)), str, Data).
token_data(token(_, _, _, symbols(Data)), symbols, Data).
token_data(token(_, _, _, identifier(Data)), identifier, Data).
token_data(token(_, _, _, white(Data)), white, Data).

white_token(token(_, _, _, white(_))).

% print_token(+Token) : Print the location of the Token
print_token(Token) :-
	Token = token(FileName, LineNo, Position, _),
	source(FileName, LineNo, Text),
	write(FileName), write('('), write(LineNo),
	write(','), write(Position), write(') '),
	write(Text), write(' : ').


% trim_first_space(+InputTokens, -OutputTokens) : Remove leading white spaces tokens

trim_first_space([],[]).
trim_first_space(InputTokens,OutputTokens) :-
	InputTokens = [Head|Tail],
	( white_token(Head) -> OutputTokens = Tail ; OutputTokens = InputTokens ).


% trim_last_space(+InputTokens, -OutputTokens) : Remove trailing white spaces tokens

trim_last_space([],[]).
trim_last_space(InputTokens,OutputTokens) :-
	last(InputTokens, Last),
	append(RestTokens, [Last], InputTokens),
	( white_token(Last) -> OutputTokens = RestTokens ; OutputTokens = InputTokens ).


% trim_space(+InputTokens, -OutputTokens) : Remove white spaces on both end

trim_space(InputTokens,OutputTokens) :-
	trim_first_space(InputTokens, TokensList),
	trim_last_space(TokensList, OutputTokens).

trim_all_space([],[]).

trim_all_space(InputTokens, OutputTokens) :-
	InputTokens = [Head|Tail],
	token_data(Head, white, _),
	trim_all_space(Tail, OutputTokens).

trim_all_space([Head|InputTail], [Head|OutputTail]) :-
	\+ token_data(Head, white, _),
	trim_all_space(InputTail, OutputTail).


reset_error :-
	nb_setval(error, 0).
	
count_error :-
	nb_getval(error, X),
	XX is X +1,
	nb_setval(error, XX).

% token_partition (+TokenList, +Data, -Token, -TokenList1, -TokenList2)
token_partition(Tokens, Data, Tokens1, Tokens2) :-
	trim_space(Tokens, L),
	token_partition_(L, Data, L1, L2),
	trim_last_space(L1, Tokens1),
	trim_first_space(L2, Tokens2).

token_partition_([Head|Tail], Data, [], Tail) :- token_data(Head, _, Data).
token_partition_([Head|Tail], open1, [], Tail) :- token_data(Head, open1, _).
token_partition_([Head|Tail], open2, [], Tail) :- token_data(Head, open2, _).
token_partition_([Head|Tail], close1, [], Tail) :- token_data(Head, close1, _).
token_partition_([Head|Tail], close2, [], Tail) :- token_data(Head, close2, _).
token_partition_([Head|Tail], Data, [Head|List1], List2) :- token_partition_(Tail, Data, List1, List2).
%token_partition_([], _, [], []).

% tight token_partition (+TokenList, +Data, -Token, -TokenList1, -TokenList2)
tight_token_partition(Tokens, Data, Tokens1, Tokens2) :-
	trim_space(Tokens, L),
	token_partition_(L, Data, Tokens1, Tokens2).

% spaced_token_partition (+TokenList, +Data, -Token, -TokenList1, -TokenList2)
spaced_token_partition(Tokens, Data, Tokens1, Tokens2) :-
	trim_space(Tokens, L),
	token_partition_(L, Data, L1, L2), 
	last(L1, T1),
	white_token(T1), !,
	trim_last_space(L1, Tokens1),
	L2 = [H|_],
	white_token(H),
	trim_first_space(L2, Tokens2).


% spaced_partition (+TokenList, -TokenList1, -TokenList2)
space_partition(Tokens, Tokens1, Tokens2) :-
	trim_space(Tokens, L),
	space_partition_(L, L1, L2),
	trim_last_space(L1, Tokens1),
	trim_first_space(L2, Tokens2).
	
space_partition_([], [], []).
space_partition_([Head|Tail], [], Tail) :- white_token(Head).
space_partition_([Head|Tail], [Head|List1], List2) :- space_partition_(Tail, List1, List2).


% tokens_atom(+Tokens, -Atom)
tokens_atom(Tokens, Atom) :- tokens_atom_(Tokens, Codes), atom_codes(Atom, Codes).

tokens_atom_([],[]).
tokens_atom_([H|T],Z) :-
	token_data(H, _, D),
	tokens_atom_(T, DD),
	append(D, DD, Z).

	
% print_error(HeadToken, StringType)
print_error([H|T], S) :-
	print_token(H),
	write('Unable to understand '),
	write(S),
	write(' '),
	tokens_atom([H|T], X),
	writeln(X),
	count_error.

	
% ******************** List Utilities ******************** 

couple(List, Result) :-
	triple_split(List, Part1, Main, Part2),
	Main = [Head | Tail],
	append(Rest, [Last], Tail),
	Head = begin,
	Last = end,
	couple(Rest, Middle),
	couple(Part1, Top),
	couple(Part2, Bottom),!,
	Temp = [block(Middle)|Bottom],
	append(Top, Temp, Result).

couple(List, List) :-
	\+ member(List, begin),
	\+ member(List, end).

couple(FileName, _, _) :-
	write(' Error '), write(FileName),
	writeln(' : Mismatch of block opening and closing statements'),
	count_error,
	fail.

triple_split(List, List1, List2, List3) :-
	append(List1, ListTemp, List),
	append(List2, List3, ListTemp).

count(E, L, N) :- count_(E, L, 1, N).
count_(_, [], Z, Z).
count_(E, [E|T], N, Z) :- NN is N+1, count_(E, T, NN, Z).
count_(E, [_|T], N, Z) :- count_(E, T, N, Z).


list_partition([H|T], H, [], T).
list_partition([H|T], X, [H|L1], L2) :- list_partition(T, X, L1, L2).


head_list(L, 0, [], L) :- !.
head_list([H|T], N, [H|R], L) :- NN is N-1, head_list(T, NN, R, L).

group(_, Statements, Group) :-
	group0(Statements, Blocks),
	reverse(Blocks, Reverse),
	group1(Reverse, ReverseGroups),
	reverse(ReverseGroups, Group), !.

group(FileName, _, _) :-
	write(' Error '), write(FileName),
	writeln(' : Mismatch of block opening and closing statements'),
	count_error,
	fail.

group0(List, Result) :-
	triple_split(List, Part1, Main, Part2),
	Main = [Head | Tail],
	last(Tail, Last),
	append(Middle, [Last], Tail),
	Head = statement( begin(_), _),
	Last = statement( end(_), _),
	group0(Part1, Top),
	group0(Part2, Bottom),!,
	Temp = [block(Middle)|Bottom],
	append(Top, Temp, Result).

group0(List, List) :-
	\+ member(List, statement( begin(_), _)),
	\+ member(List, statement( end(_), _)).

group1([], []).
group1([XH|XT], [YH|YT]) :-
		XH = block(L),
		XT = [N| T],
		\+ N = block(_),
		YH = group(N, L),
		group1(T,YT).

group1([H|XT], [H|YT]) :-
		group1(XT,YT).

