% --------------- Implemenet Lexer ---------------

implement lexer
    open core, errors, file, list

constants
    comment_char ='@'.
    string_char = '\"'.
    continue_char = '\\'.

class facts
    filename:string := "".
    linenum:unsigned := 0.
    position:unsigned := 0.
    charlist:char* := [].

class predicates
    isSymbol: (char) determ (i).
    isAnything: (char) determ (i).
    isDigit: (char) determ (i).
    isWhite: (char) determ (i).
    isAlphanum: (char) determ (i).
    isAlpha: (char) determ (i).
    isLower: (char) determ (i).
    isUpper: (char) determ (i).

    tokenize: (tokenlist) -> tokenlist.                                     % Convert Charlist to Tokens
    concat: (tokenlist Acc, tokenlist Input, integer Status ) -> tokenlist. % Remove { } to form continous sequence
    split: (tokenlist Input, tokenlist* Init) -> tokenlist*.                % Split Input into Lines, Init is [[]]
    trimWhiteTokens: (tokenlist) -> tokenlist.                              % Remove white tokens on both ends
    trimEmptyLines: (tokenlist*) -> tokenlist*.                             % Remove empty lines
    trimComments: (tokenlist) -> tokenlist.                                 % Remove all comments
    joinLines: (tokenlist*, tokenlist*) determ (i,o).                       % Join lines with continuation character

    readWhite: () -> unsigned.
    readUnderscore: () -> unsigned.
    readDigits: () -> char*.
    readAlphanum: () -> char*.
    readSymbols: () -> char*.
    readString: () -> char*.
    readComment: () procedure.

    str2num: (char*) -> unsigned.
    debugLexer: (tokenlist*) procedure.  %%% DEBUG %%%

clauses

    main(Filename, Output) :-
        filename := Filename,
        linenum := 1,
        position := 1,
        charlist := [],
        try
            F = file::readString(filename),
            charlist := string::toCharList(F)
        catch _TraceId do
            error1("Unable to read file", filename)
        end try,

        if (charlist=[]) then
            Output = []
        else
            T0 = tokenize([]),
            T1 = concat([], T0, 0),
            T2 = split(T1, []),
            T3 = list::filteredMap(T2, trimWhiteTokens),
            T4 = list::filteredMap(T3, trimComments),
            T5 = trimEmptyLines(T4),
            joinLines(T5, Output),
            debugLexer(Output) %%% DEBUG %%%
        end if.


    trimWhiteTokens(X) = Y :-
        if (X = [white(_,_,_)|T0]) then
            T1 = list::reverse(T0)
        else
            T1 = list::reverse(X)
        end if,

        if (T1 = [white(_,_,_)|T2]) then
            Y = list::reverse(T2)
        else
            Y = list::reverse(T1)
        end if.


    trimEmptyLines(X) = Y :-
        if (X = [H|T]) then
            if (H = []) then
                Y = trimEmptyLines(T)
            else
                Y = [H|trimEmptyLines(T)]
            end if
        else
            Y = []
        end if.


    trimComments(X) = Y :-
        if (list::reverse(X) = [comment(F, L, P), H|T]) then
            if (H = white(_, _, _)) then
                Y = list::reverse(T)
            else
                Y = [],
                error3("Comments must be preceded by white space", F, L, P)
            end if
        else
            Y = X
        end if.


    split(L, X) = Z :-
        if (X = [XH|XT]) then
            if (L = [H|T]) then
                if (H=endl(_,_,_) or H=comment(_,_,_)) then
                    Z = split(T, [[]|X])
                else
                    Z = split(T, [[H|XH]|XT])
                end if
            else
                Y = list::reverse(X),
                Z = list::filteredMap(Y, list::reverse)
            end if
        else
            Z = split(L, [[]])
        end if.


    tokenize(X) = Z :-
        if (charlist = [H|L]) then

            if (H = continue_char) then
                charlist := L,
                Y = [continue(filename, linenum, position)|X],
                position := position + 1

            elseif (H = '(') then
                charlist := L,
                Y = [open1(filename, linenum, position)|X],
                position := position + 1

            elseif (H = ')') then
                charlist := L,
                Y =  [close1(filename, linenum, position)|X],
                position := position + 1

            elseif (H = '{') then
                charlist := L,
                Y =  [open2(filename, linenum, position)|X],
                position := position + 1

            elseif (H = '}') then
                charlist := L,
                Y =  [close2(filename, linenum, position)|X],
                position := position + 1

            elseif (H = '[') then
                charlist := L,
                Y =  [open3(filename, linenum, position)|X],
                position := position + 1

            elseif (H = ']') then
                charlist := L,
                Y =  [close3(filename, linenum, position)|X],
                position := position + 1

            elseif (H = comment_char) then
                readComment(),
                Y =  [comment(filename, linenum, position)|X],
                linenum := linenum +1,
                position := 1

            elseif (H = '\n') then
                charlist := L,
                Y = [endl(filename, linenum, position)|X],
                linenum := linenum +1,
                position := 1

            elseif (H = '_') then
                N = readUnderscore(),
                Y =  [underscore(filename, linenum, position)|X],
                position := position + N

            elseif (isWhite(H)) then
                N = readWhite(),
                Y =  [white(filename, linenum, position)|X],
                position := position + N

            elseif (isDigit(H)) then
                Digits = readDigits(),
                Num = str2num(list::reverse(Digits)),
                Y =  [num(Num, filename, linenum, position)|X],
                position := position + list::length(Digits)

            elseif (isLower(H)) then
                S = readAlphanum(),
                Y =  [identifier0(string::createFromCharList(S), filename, linenum, position)|X],
                position := position + list::length(S)

            elseif (isUpper(H)) then
                S = readAlphanum(),
                Y =  [identifier1(string::createFromCharList(S), filename, linenum, position)|X],
                position := position + list::length(S)

            elseif (H = string_char) then
                P = position,
                position := position + 1,
                charlist := L,
                S = readString(),
                Y =  [str(string::createFromCharList(S), filename, linenum, P)|X]

            else
                S = readSymbols(),
                Y =  [symbols(string::createFromCharList(S), filename, linenum, position)|X],
                position := position + list::length(S)
            end if,

            Z = tokenize(Y)
        else
            Z = list::reverse(X)
        end if.


    concat(X, Z, N) = Y :-
        if (Z = [H|T]) then
            if (H = open2(_,L,P)) then
                if (N = 0) then
                    Y = concat(X, T, 1)
                else
                    error3("Nested { are not allowed", filename, L, P),
                    Y = concat(X, T, 1)
                end if

            elseif (H = close2(_,L,P)) then
                if (N = 1) then
                    Y = concat(X, T,0)
                else
                    error3("Stay } found", filename, L, P),
                    Y = concat(X, T,0)
                end if

            elseif (H = endl(_,L,P)) then
                if (N = 1) then
                    XX = [white(filename, L,P)|X],
                    Y = concat(XX, T,1)
                else
                    XX = [endl(filename, L,P)|X],
                    Y = concat(XX, T,0)
                end if
            else
                XX = [H|X],
                Y = concat(XX, T, N)
            end if

        else
            Y = list::reverse(X)
        end if.


    readUnderscore() = N :-
        if (charlist = ['_'|T]) then
            charlist := T,
            N = 1 + readUnderscore()
        else
            N = 0
        end if.


    readWhite() = N :-
        if ( (charlist = [H|T]) and (isWhite(H)) ) then
            charlist := T,
            N = 1 + readWhite()
        else
            N = 0
        end if.


    readDigits() = N :-
        if ((charlist = [H|T]) and (isDigit(H))) then
            charlist := T,
            N = [H|readDigits()]
        else
            N = []
        end if.


    readString() = N :-
        if (charlist = [H|T]) then
            charlist := T,
            position := position +1,
            if (H = string_char) then
                N = []
            elseif (H = '\n') then
                N = [],
                error3("Abnormal Termination of String", filename, linenum, position-1),
                linenum := linenum + 1,
                position := 1
            elseif (H = '\\') then
                position := position + 1,
                if (T = [HH|TT]) then
                    charlist := TT,
                    if (HH = '\\') then
                        N = ['\\'|readString()]
                    elseif (HH = 'n') then
                        N = ['\n'|readString()]
                    elseif (HH = '\"') then
                        N = ['\"'|readString()]
                    elseif (HH = '\'') then
                        N = ['\''|readString()]
                    elseif (HH = 't') then
                        N = ['\t'|readString()]
                    else
                        error3("Unable to understand the escape sequence", filename, linenum, position-1),
                        N = readString()
                    end if
                else
                    N = [],
                    error3("Abnormal End of File", filename, linenum, position-1)
                end if
            else
                N = [H|readString()]
            end if
        else
            N = [],
            error3("Abnormal End of File", filename, linenum, position)
        end if.


    readAlphanum() = N :-
        if (charlist = [H|T]) and (isAlphanum(H)) then
            charlist := T,
            N = [H|readAlphanum()]
        else
            N = []
        end if.


    readComment() :-
        if (charlist = [H|T]) then
            charlist := T,
            if not(H='\n') then readComment() end if
        end if.


    readSymbols() = N :-
         if ((charlist = [H|T]) and (isSymbol(H))) then
            charlist := T,
            N = [H|readSymbols()]
         else
            N = []
         end if.


    str2num(L) = N :-
        if (L=[H|T]) then
            NN = string::getCharValue(H) - 48,
            N= NN + 10*str2num(T)
        else
            N=0
        end if.


    joinLines([], []) :- !.

    joinLines([H|T], Z) :-
        [_|TT] = list::reverse(H),
        HH = list::getMember_nd(TT),
        HH = continue(F, L, P), !,
        error3("Line continuation character cannot occur in the middle of the line", F, L, P),
        joinLines(T, Z).

    joinLines([Z], [Z]) :- !.

    joinLines([H|T], Z) :-
        [H1|H2] = list::reverse(H),
        H1 = continue(_,_,_),
        T = [T1|T2],
        L =  list::append(list::reverse(H2), T1),
        Y = [L|T2],
        joinLines(Y, Z), !.

    joinLines([H|T1], [H|T2]) :-
        joinLines(T1, T2), !.


    getPosition(T, F, L, P) :- T = identifier0(_, F, L, P), !.
    getPosition(T, F, L, P) :- T = identifier1(_, F, L, P), !.
    getPosition(T, F, L, P) :- T = str(_, F, L, P), !.
    getPosition(T, F, L, P) :- T = num(_, F, L, P), !.
    getPosition(T, F, L, P) :- T = symbols(_, F, L, P), !.
    getPosition(T, F, L, P) :- T = endl(F, L, P), !.
    getPosition(T, F, L, P) :- T = continue(F, L, P), !.
    getPosition(T, F, L, P) :- T = white(F, L, P), !.
    getPosition(T, F, L, P) :- T = underscore(F, L, P), !.
    getPosition(T, F, L, P) :- T = open1(F, L, P), !.
    getPosition(T, F, L, P) :- T = open2(F, L, P), !.
    getPosition(T, F, L, P) :- T = open3(F, L, P), !.
    getPosition(T, F, L, P) :- T = close1(F, L, P), !.
    getPosition(T, F, L, P) :- T = close2(F, L, P), !.
    getPosition(T, F, L, P) :- T = close3(F, L, P), !.


    %%% DEBUG %%%
    debugLexer(Tokens) :-
        OutFile = string::concat(filename, ".lex"),
        try
            OutputFile =outputStream_file::create(OutFile),
            foreach X = list::getMember_nd(Tokens) do
                OutputFile:write(toString(X)),
                OutputFile:write('\n')
            end foreach,
            OutputFile:close()
        catch _TraceId do
            stdio::write("DEBUG : Unable to create "),
            stdio::write(OutFile),
            stdio::write("\n")
        end try.


    isSymbol(C) :- not(isAnything(C)), !.

    isAnything(C) :- isAlphanum(C), !.
    isAnything(C) :- isWhite(C), !.
    isAnything(C) :- list::isMember(C, ['_', '(', ')', '{', '}', '[', ']', '\n', comment_char, continue_char]), !.

    isAlphanum(C) :- isAlpha(C), !.
    isAlphanum(C) :- isDigit(C), !.

    isAlpha(C) :- isLower(C), !.
    isAlpha(C) :- isUpper(C), !.

    isWhite(C) :- list::isMember(C, [' ', '\r', '\t']).

    isDigit(C) :- list::isMember(C, ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']).

    isLower(C) :- list::isMember(C, ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z']).

    isUpper(C) :- list::isMember(C, ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z']).

end implement lexer

