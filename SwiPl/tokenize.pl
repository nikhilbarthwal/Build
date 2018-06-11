:- module(tokenize, [tokenize_file/3, merge_check/1, merge_join/2, tokenize_line/5]).


% tokenize_file_(+FileName, +FileCodes, -FileTokens) : In FileName, Read list	of codes line by line,
%	convert each line into list of tokends. Return list of list of Tokens. Errors are put in Database.

tokenize_file(FileName, FileCodes, FileTokens) :-
	tokenize_file_(FileName, 1, FileCodes, Tokens1),
	trim_space(Tokens1, Tokens2),
	file_token_filter(Tokens2, FileTokens).

% tokenize_file_(+FileName, +LineNo , +Codes, -Tokens) : In FileName, Stating from LineNo, Read
%	list of codes line by line, convert each line into list of tokends. Return list of list of
%	Tokens. Errors are put in database.

tokenize_file_(_FileName, _LineNo, [], []) :- !.

tokenize_file_(FileName, LineNo, [CodesHead|CodesTail], [TokensHead|TokensTail]) :-
	tokenize_line(FileName, LineNo, 1, CodesHead, TokensHead), !,
	NextLine is LineNo + 1,
	tokenize_file_(FileName, NextLine, CodesTail, TokensTail).

tokenize_file_(FileName, LineNo, [_|CodesTail], TokensList) :-
	NextLine is LineNo + 2,
	tokenize_file_(FileName, NextLine, CodesTail, TokensList).


% tokenize_line(FileName, LineNo, Position, CodesList, TokensList) : Parse CodesList into TokensList. FileName, Line,
% 	Position are used for embedding info in tokens.

tokenize_line(_FileName, _LineNo, _Position, [], []).

tokenize_line(FileName, LineNo, Position, CodesList, [TokensHead|TokensTail]) :-
	read_token(FileName, LineNo, Position, CodesList, TokensHead, NewPosition, RestCodes), !,
	tokenize_line(FileName, LineNo, NewPosition, RestCodes, TokensTail).

tokenize_line(FileName, LineNo, Position, CodesList, []) :-
	atom_codes(Text, CodesList),
	count_error,
	write(FileName), write('('), write(LineNo), write(','), write(Position), 
	write(') : Unable to parse text '), writeln(Text), fail.

% Token is a comment?
read_token(FileName, LineNo, Position, [35|RestCodes], Token, Position, []) :-
	!, Token = token(FileName, LineNo, Position, comment(RestCodes)).

% Token is a (?
read_token(FileName, LineNo, Position, [40|RestCodes], Token, Position, RestCodes) :-
	!, Token = token(FileName, LineNo, Position, open1).

% Token is a )?
read_token(FileName, LineNo, Position, [41|RestCodes], Token, Position, RestCodes) :-
	!, Token = token(FileName, LineNo, Position, close1).

% Token is a [?
read_token(FileName, LineNo, Position, [91|RestCodes], Token, Position, RestCodes) :-
	!, Token = token(FileName, LineNo, Position, open2).

% Token is a ]?
read_token(FileName, LineNo, Position, [93|RestCodes], Token, Position, RestCodes) :-
	!, Token = token(FileName, LineNo, Position, close2).

% Token is a Integer?
read_token(FileName, LineNo, Position, [FirstCode|RestCodes], Token, NewPosition, LeftOvers) :-
	char_type(FirstCode, digit),!,
	token_integer([FirstCode|RestCodes], LeftOvers, Codes),
	length(Codes, Length),
	NewPosition is Position + Length,
	Token = token(FileName, LineNo, Position, int(Codes)).


% Token is a White Space?
read_token(FileName, LineNo, Position, [FirstCode|RestCodes], Token, NewPosition, LeftOvers) :-
	char_type(FirstCode, white),!,
	token_white([FirstCode|RestCodes], LeftOvers, Codes),
	length(Codes, Length),
	NewPosition is Position + Length,
	Token = token(FileName, LineNo, Position, white(Codes)).


% Token is a Identifier?
read_token(FileName, LineNo, Position, [FirstCode|RestCodes], Token, NewPosition, LeftOvers) :-
	char_type(FirstCode, alpha),!,
	token_identifier([FirstCode|RestCodes], LeftOvers, Codes),
	length(Codes, Length),
	NewPosition is Position + Length,
	Token = token(FileName, LineNo, Position, identifier(Codes)).
	

% Token is a String?
read_token(FileName, LineNo, Position, [FirstCode|RestCodes], Token, NewPosition, LeftOvers) :-
	FirstCode = 34, !,
	token_string(RestCodes, LeftOvers, Codes),
	length(Codes, Length),
	NewPosition is Position + Length,
	Token = token(FileName, LineNo, Position, str(Codes)).


% Token is a Symbols?
read_token(FileName, LineNo, Position, CodesList, Token, NewPosition, RestCodes) :-
	token_symbols(CodesList, RestCodes, SymbolsList),
	length(SymbolsList, Length),
	NewPosition is Position + Length,
	Token = token(FileName, LineNo, Position, symbols(SymbolsList)).


% token_integer(InputCodes, RestCodes, TokenCodes)

token_integer([], [], []).

token_integer(CodesList, CodesList, []) :-
	CodesList = [FirstCode|_],
	\+ char_type(FirstCode, digit).

token_integer([FirstCode|RestCodes], LeftOvers, [FirstCode|RestDigits]) :-
	char_type(FirstCode, digit),
	token_integer(RestCodes, LeftOvers, RestDigits). 


% token_white(InputCodes, RestCodes, TokenCodes)

token_white([], [], []).

token_white(CodesList, CodesList, []) :-
	CodesList = [FirstCode|_],
	\+ char_type(FirstCode, white).

token_white([FirstCode|RestCodes], LeftOvers, [FirstCode|RestChars]) :-
	char_type(FirstCode, white),
	token_white(RestCodes, LeftOvers, RestChars). 


% token_identifier(InputCodes, RestCodes, TokenCodes)

token_identifier([], [], []).

token_identifier(CodesList, CodesList, []) :-
	CodesList = [FirstCode|_],
	\+ char_type(FirstCode, alnum).

token_identifier([FirstCode|RestCodes], LeftOvers, [FirstCode|RestChars]) :-
	char_type(FirstCode, alnum),
	token_identifier(RestCodes, LeftOvers, RestChars). 


% token_string(InputCodes, RestCodes, TokenCodes)

token_string([], [], []) :- fail.

token_string([FirstCode|RestCodes], RestCodes, []) :-		% End of String
	FirstCode = 34.


token_string([FirstCode, SecondCode|RestCodes], LeftOvers, [FirstChar|RestChars]) :-	% Null Character
	FirstCode = 92,
	[SecondCode] = "0",
	FirstChar is 0,
	token_string(RestCodes, LeftOvers, RestChars).

token_string([FirstCode, SecondCode |RestCodes], LeftOvers, [FirstChar|RestChars]) :-	% Bell Character
	FirstCode = 92,
	[SecondCode] = "a",
	FirstChar is 7,
	token_string(RestCodes, LeftOvers, RestChars).

token_string([FirstCode,SecondCode|RestCodes], LeftOvers, [FirstChar|RestChars]) :-	% Backspace
	FirstCode = 92,
	[SecondCode] = "b",
	FirstChar is 8,
	token_string(RestCodes, LeftOvers, RestChars).

token_string([FirstCode,SecondCode|RestCodes], LeftOvers, [FirstChar|RestChars]) :-	% Tab Character
	FirstCode = 92,
	[SecondCode] = "t",
	FirstChar is 9,
	token_string(RestCodes, LeftOvers, RestChars).
	
token_string([FirstCode,SecondCode|RestCodes], LeftOvers, [FirstChar|RestChars]) :-	% New Line Character
	FirstCode =  92,
	[SecondCode] = "n",
	FirstChar is 10,
	token_string(RestCodes, LeftOvers, RestChars).

token_string([FirstCode,SecondCode|RestCodes], LeftOvers, [FirstChar|RestChars]) :-	% Formfeed Character
	FirstCode = 92,
	[SecondCode] = "f",
	FirstChar is 12,
	token_string(RestCodes, LeftOvers, RestChars).

token_string([FirstCode,SecondCode|RestCodes], LeftOvers, [FirstChar|RestChars]) :-	% Carriage Return
	[FirstCode] = "\\",
	[SecondCode] = "n",
	FirstChar is 13,
	token_string(RestCodes, LeftOvers, RestChars).

token_string([FirstCode,SecondCode|RestCodes], LeftOvers, [FirstChar|RestChars]) :-	% Backward Slash
	FirstCode = 92,
	SecondCode = 92,
	FirstChar is 92,
	token_string(RestCodes, LeftOvers, RestChars).

token_string([FirstCode,SecondCode|RestCodes], LeftOvers, [FirstChar|RestChars]) :-	% Double Quote
	FirstCode = 92,
	SecondCode = 34,
	FirstChar is 34,
	token_string(RestCodes, LeftOvers, RestChars).

token_string([FirstCode|_RestCodes], _LeftOvers, _CharsList) :-	% Check if String is ended properly
	FirstCode = 92,!,
	fail.

token_string([FirstCode|RestCodes], LeftOvers, [FirstCode|RestChars]) :-
	token_string(RestCodes, LeftOvers, RestChars).


% token_symbols(InputCodes, RestCodes, TokenCodes)

token_symbols([], [], []).

token_symbols(CodesList, CodesList, []) :-
	CodesList = [FirstCode|_],
	char_type(FirstCode, digit).

token_symbols(CodesList, CodesList, []) :-
	CodesList = [FirstCode|_],
	char_type(FirstCode, alnum).

token_symbols(CodesList, CodesList, []) :-
	CodesList = [FirstCode|_],
	char_type(FirstCode, white).

token_symbols(CodesList, CodesList, []) :-
	CodesList = [FirstCode|_],
	( FirstCode = 35 ; FirstCode = 91 ; FirstCode = 93 ;
	  FirstCode = 40 ; FirstCode = 41 ). % The # character

token_symbols([FirstCode|RestCodes], LeftOvers, [FirstCode|RestSymbols]) :-
	token_symbols(RestCodes, LeftOvers, RestSymbols). 


% token_filter(+RawTokenLists, -TokenLists) : Remove coments, and extra spaces.
%	Checks line spacing also.

 
file_token_filter([], []) :- !.

file_token_filter([RawTokensList|RestRawTokensList], [TokensList|RestTokensList]) :-
	token_list_filter(RawTokensList, TokensList), 
	\+ TokensList = [],
	file_token_filter(RestRawTokensList, RestTokensList).

file_token_filter([_|RawTokensList], TokensList) :-
	file_token_filter(RawTokensList, TokensList).


%token_list_filter(InputTokens, Outputokens) : removes comment, spacing, and checks for comment & joins

token_list_filter([Input], []) :- token_data(Input, white, _), !.
token_list_filter([Input], []) :- token_data(Input, comment, _), !.
token_list_filter([Input], []) :- token_data(Input, symbol,[92]), !.
token_list_filter([Token], [Token]).

token_list_filter(InputTokens, OutputTokens) :-
	append(RestTokens, [Last2, Last1], InputTokens),
	( token_data(Last1, comment, _) ->
		( token_data(Last2, white, _) ->
			OutputTokens = RestTokens	
		;
			print_token(Last1), writeln('Comments must be preceeded by white space.'),
			count_error, fail
		)
	;
		OutputTokens = InputTokens
	), !,
	\+ joins_errors(OutputTokens).


%joins_errors(TokensList) : Checks for comments & spacing

joins_errors(TokensList) :-
	append(_, [Last2, Last1], TokensList),
	Last1 = token(_, _, _, symbols([92])),
	\+ Last2 = token(_, _, _, white(_)),
	print_token(Last1),
	writeln('The \\ symbol must be preceeded by white space.'),
	count_error.

joins_errors(TokensList) :-
	nth1(N, TokensList, Token),
	Token = token(_, _, _, symbols([92])),
	\+ length(TokensList, N),
	print_token(Token),
	writeln('The \\ symbol cannot occur in the middle of line.'),
	count_error.


% merge_join(+InputTokens, -OutputToken) : Merged+LineNo , +Codes, -Tokens) : In FileName, Stating from LineNo,
% 	Read Tokens. Errors are put in database.

merge_join(FileTokens, TokensList) :-
	merge_check(FileTokens),
	merge_join_(FileTokens, TokensList).

% Check if the File can be merged
merge_check(FileTokens) :-
	last(FileTokens, TokenList),
	last(TokenList, Z),
	token_data(Z, symbols, [92]),
	print_token(Z),
	writeln(' Last line cannot end with \\ '),
	count_error,
	!, fail.
merge_check(_).

merge_join_([], []) :- !.

merge_join_([Head1|RestInput], [Head2|RestOutput]) :-
	trim_space(Head1, Head2),
	last(Head2, Last),
	\+ token_data(Last, symbols, [92]),!,
	merge_join_(RestInput, RestOutput).

merge_join_([Head1, Head2|RestInput], Output) :-
	trim_last_space(Head1, L),
	last(L, Last),
	append(Head, [Last], L),
	append(Head, Head2, List),
	trim_space(List, Input),
	merge_join_([Input|RestInput], Output).

