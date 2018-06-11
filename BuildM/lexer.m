% *****************************************************
%    LEXER: Convert a text file into token elements
% *****************************************************

:- module lexer.

:- interface.

:- import_module utils.

:- import_module io, int, list, string, char.

:- type tokenGroup ---> tokenGroup1 ; tokenGroup2 ; tokenGroup3.

:- type element --->
    elementList(tokenGroup, block(element));
    elementKeyword(string);
    elementIdentifier(string);
    elementString(string);
    elementChar(char);
    elementAnonymous;
    elementUnify(string);
    elementNumber(int);
    elementSymbols(string).

:- pred lexMain(string::in, block(element)::out) is semidet.

:- implementation.

:- type token --->
    tokenKeyword(string);
    tokenIdentifier(string);
    tokenString(string);
    tokenChar(char);
    tokenFile(string);
    tokenAnonymous;
    tokenUnify(string);
    tokenNumber(number);
    tokenSymbols(string);
    tokenOpen(tokenGroup);
    tokenClose(tokenGroup).

lexMain(_FileName, Elements) :- Elements = [].

