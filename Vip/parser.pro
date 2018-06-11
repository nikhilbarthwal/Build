% --------------- Implement Parser ---------------

implement parser
    open core, errors, file, list

class facts
    statements : pstatement* := [].
    filename : string := "".

class predicates
    readPair: (lexer::token* Input, lexer::token* Output, pair) determ  (i,o,o).
    readMinorList: (lexer::token* Input, lexer::token* Output, minorlist) determ  (i,o,o).
    readMajorList: (lexer::token* Input, lexer::token* Output, majorlist) determ (i,o,o).
    readCond: (lexer::token* Input, lexer::token* Output, cond) determ (i,i,o) (i,o,o).
    readExpr: (lexer::token* Input, lexer::token* Output, expr) determ (i,i,o) (i,o,o).
    readExprList: (lexer::token* Input, lexer::token* Output, expr*) determ (i,o,o).
    readIdentifierList: (lexer::token* Input, lexer::token* Output, lexer::token*) determ (i,o,o).
    readStatement: (lexer::token* , statement) determ (i,o).

    matchSymbols: (lexer::token* Input, lexer::token* Output, string) determ (i,o,i) (i,i,i).
    matchIdentifier: (lexer::token* Input, lexer::token* Output, string) determ (i,o,i) (i,i,i).
    splitSymbol: (string, lexer::token*, lexer::token*, lexer::token*) nondeterm (i,i,o,o).
    splitIdentifier: (string, lexer::token*, lexer::token*, lexer::token*) nondeterm (i,i,o,o).
    whiteFirstToken: (lexer::token* Input, lexer::token* Output) determ (i,o).
    whiteLastToken: (lexer::token* Input, lexer::token* Output) determ (i,o).
    chopTokens: (lexer::token* Input, lexer::token* Output) procedure (i,o).
    trimTokens: (lexer::token* Input, lexer::token* Output) procedure (i,o).
    parse: (lexer::tokenlist*) determ (i).

    startBlock: (statement) determ (i).
    createBlocks: (pstatement*, pstatement*) determ (i,o).
    createBlocks1: (pstatement*, pstatement*, pstatement*) determ (i,o,o).
    addStatement:(pstatement) procedure (i).

    debugParser: (pstatement*) procedure.  %%% DEBUG %%%
    debugPrint: (outputStream, pstatement*, integer) procedure. %%% DEBUG %%%
    debugSpace: (outputStream, integer) procedure. %%% DEBUG %%%

clauses

    whiteFirstToken([H|T], T) :- H = lexer::white(_, _, _).

    whiteLastToken(L, Z) :-
        [H|T] = list::reverse(L),
        H = lexer::white(_, _, _),
        Z = list::reverse(T).


    chopTokens(L1, L2) :-
        if (L1 = [H|T]) then
            if (H = lexer::white(_, _, _)) then L2 = T else L2 = L1 end if
        else
            L2 = []
        end if.


    trimTokens(L1, L2) :-
        chopTokens(list::reverse(L1), L),
        L2 = list::reverse(L).


    readPair(X, Y, D) :-
        chopTokens(X, L1),
        L1 = [N|L2],
        N = lexer::identifier1(_, _, _, _),
        whiteFirstToken(L2, L3),
        matchIdentifier(L3, L4, "in"), !,
        whiteFirstToken(L4, L5),
        readExpr(L5, Y, E), !,
        D = pair(N, E).


    readMinorList(X, Y, [H|T]) :-
        readPair(X, L1, H),
        chopTokens(L1, L2),
        matchSymbols(L2, L3, ","), !,
        readMinorList(L3, Y, T).

    readMinorList(X, Y, [H]) :-
        readPair(X, Y, H), !.


    readMajorList(X, Y, [H|T]) :-
        readMinorList(X, L1, H),
        chopTokens(L1, L2),
        matchSymbols(L2, L3, ";"), !,
        readMajorList(L3, Y, T).

    readMajorList(X, Y, [H]) :-
        readMinorList(X, Y, H), !.


    readCond(X, Y, D) :-
        chopTokens(X, L1),
        matchIdentifier(L1, L2, "if"),
        whiteFirstToken(L2, L3),
        readCond(L3, L4, C),
        whiteFirstToken(L4, L5),
        matchIdentifier(L5, L6, "then"),
        whiteFirstToken(L6, L7),
        readCond(L7, L8, C1),
        whiteFirstToken(L8, L9),
        matchIdentifier(L9, L10, "else"), !,
        whiteFirstToken(L10, L11),
        readCond(L11, Y, C2), !,
        D = cond_if2(C, C1, C2).

    readCond(X, Y, D) :-
        chopTokens(X, L1),
        matchIdentifier(L1, L2, "if"),
        whiteFirstToken(L2, L3),
        readCond(L3, L4, C),
        whiteFirstToken(L4, L5),
        matchIdentifier(L5, L6, "then"), !,
        whiteFirstToken(L6, L7),
        readCond(L7, Y, C1), !,
        D = cond_if1(C, C1).

    readCond(X, Y, D) :-
        chopTokens(X, L1),
        readExpr(L1, L2, E1),
        whiteFirstToken(L2, L3),
        matchIdentifier(L3, L4, "in"), !,
        whiteFirstToken(L4, L5),
        readExpr(L5, Y, E2), !,
        D = memchk(E1, E2).

    readCond(X, Y, D) :-
        chopTokens(X, L),
        splitIdentifier("and", L, L1, L2),
        whiteLastToken(L1, LL1),
        whiteFirstToken(L2, LL2),
        readCond(LL1, [], C1), !,
        readCond(LL2, Y, C2),
        D = cand(C1, C2).

    readCond(X, Y, D) :-
        chopTokens(X, L),
        splitIdentifier("or", L, L1, L2),
        whiteLastToken(L1, LL1),
        whiteFirstToken(L2, LL2),
        readCond(LL1, [], C1), !,
        readCond(LL2, Y, C2),
        D = cor(C1, C2).

    readCond(X, Y, D) :-
        chopTokens(X, L1),
        readExpr(L1, L2, E1),
        chopTokens(L2, L3),
        matchSymbols(L3, L4, ">="), !,
        readExpr(L4, Y, E2),
        D = ge(E1, E2).

    readCond(X, Y, D) :-
        chopTokens(X, L1),
        readExpr(L1, L2, E1),
        chopTokens(L2, L3),
        matchSymbols(L3, L4, "=>"), !,
        readExpr(L4, Y, E2),
        D = ge(E1, E2).

    readCond(X, Y, D) :-
        chopTokens(X, L1),
        readExpr(L1, L2, E1),
        chopTokens(L2, L3),
        matchSymbols(L3, L4, "<="), !,
        readExpr(L4, Y, E2),
        D = le(E1, E2).

    readCond(X, Y, D) :-
        chopTokens(X, L1),
        readExpr(L1, L2, E1),
        chopTokens(L2, L3),
        matchSymbols(L3, L4, "=<"), !,
        readExpr(L4, Y, E2),
        D = le(E1, E2).

    readCond(X, Y, D) :-
        chopTokens(X, L1),
        readExpr(L1, L2, E1),
        chopTokens(L2, L3),
        matchSymbols(L3, L4, ">"), !,
        readExpr(L4, Y, E2),
        D = gt(E1, E2).

    readCond(X, Y, D) :-
        chopTokens(X, L1),
        readExpr(L1, L2, E1),
        chopTokens(L2, L3),
        matchSymbols(L3, L4, "<"), !,
        readExpr(L4, Y, E2),
        D = lt(E1, E2).

    readCond(X, Y, D) :-
        chopTokens(X, L1),
        readExpr(L1, L2, E1),
        chopTokens(L2, L3),
        matchSymbols(L3, L4, "!="), !,
        readExpr(L4, Y, E2),
        D = ne(E1, E2).

    readCond(X, Y, D) :-
        chopTokens(X, L1),
        readExpr(L1, L2, E1),
        chopTokens(L2, L3),
        matchSymbols(L3, L4, "="), !,
        readExpr(L4, Y, E2),
        D = eq(E1, E2).

    readCond(X, Y, D) :-
        chopTokens(X, L0),
        matchIdentifier(L0, L1, "assert"),
        matchSymbols(L1, L2, "("),
        readExpr(L2, L3, Z),
        chopTokens(L3, L4),
        matchSymbols(L4, Y, ")"), !,
        D =condcall2(Z).

    readCond(X, Y, D) :-
        chopTokens(X, L0),
        matchIdentifier(L0, L1, "assert"),
        matchSymbols(L1, L2, "("),
        chopTokens(L2, L3),
        L3 = [N|L4],
        N = lexer::identifier1(_, _, _, _),
        chopTokens(L4, L5),
        matchSymbols(L5, L6, ","),
        readExpr(L6, L7, Z),
        Z = list(A),
        chopTokens(L7, L8),
        matchSymbols(L8, Y, ")"), !,
        D = condcall1(N, A).

    readCond(X, Y, D) :-
        chopTokens(X, L0),
        L0 = [N|L1],
        N = lexer::identifier1(_, _, _, _),
        matchSymbols(L1, L2, "("), !,
        readExprList(L2, L3, A),
        chopTokens(L3, L4),
        matchSymbols(L4, Y, ")"),
        D = condcall(N, A).

    readCond(X, Y, D) :-
        chopTokens(X, L1),
        matchIdentifier(L1, L2, "not"),
        matchSymbols(L2, L3, "("), !,
        readCond(L3, L4, C),
        chopTokens(L4, L5),
        matchSymbols(L5, Y, ")"),
        D = cnot(C).

    readCond(X, Y, D) :-
        chopTokens(X, L1),
        matchSymbols(L1, L2, "("), !,
        readCond(L2, L3, D),
        chopTokens(L3, L4),
        matchSymbols(L4, Y, ")").


    readExpr([], _, _) :- !, fail.

    readExpr(X, Y, D) :-
        chopTokens(X, L1),
        matchIdentifier(L1, L2, "if"),
        whiteFirstToken(L2, L3),
        readCond(L3, L4, C),
        whiteFirstToken(L4, L5),
        matchIdentifier(L5, L6, "then"),
        whiteFirstToken(L6, L7),
        readExpr(L7, L8, E1),
        whiteFirstToken(L8, L9),
        matchIdentifier(L9, L10, "else"), !,
        whiteFirstToken(L10, L11),
        readExpr(L11, Y, E2), !,
        D = expr_if(C, E1, E2).

    readExpr(X, Y, D) :-
        chopTokens(X, L1),
        splitSymbol("+", L1, L2, L3),
        trimTokens(L2, LL2),
        readExpr(LL2, [], E1), !,
        readExpr(L3, Y, E2),
        D = add(E1, E2).

    readExpr(X, Y, D) :-
        chopTokens(X, L1),
        splitSymbol("-", L1, L2, L3),
        trimTokens(L2, LL2),
        readExpr(LL2, [], E1), !,
        readExpr(L3, Y, E2),
        D = sub(E1, E2).

    readExpr(X, Y, D) :-
        chopTokens(X, L1),
        splitSymbol("*", L1, L2, L3),
        trimTokens(L2, LL2),
        readExpr(LL2, [], E1), !,
        readExpr(L3, Y, E2),
        D = multiply(E1, E2).

    readExpr(X, Y, D) :-
        chopTokens(X, L1),
        splitSymbol("/", L1, L2, L3),
        trimTokens(L2, LL2),
        readExpr(LL2, [], E1), !,
        readExpr(L3, Y, E2),
        D = divide(E1, E2).

    readExpr(X, Y, D) :-
        chopTokens(X, L1),
        splitSymbol("%", L1, L2, L3),
        trimTokens(L2, LL2),
        readExpr(LL2, [], E1), !,
        readExpr(L3, Y, E2),
        D = modulus(E1, E2).

    readExpr(X, Y, D) :-
        chopTokens(X, L1),
        splitSymbol("&", L1, L2, L3),
        trimTokens(L2, LL2),
        readExpr(LL2, [], E1), !,
        readExpr(L3, Y, E2),
        D = concat(E1, E2).

    readExpr(X, Y, D) :-
        chopTokens(X, L1),
        splitSymbol("^", L1, L2, L3),
        trimTokens(L2, LL2),
        readExpr(LL2, [], E1), !,
        readExpr(L3, Y, E2),
        D = join(E1, E2).

    readExpr(X, Y, D) :-
        chopTokens(X, L1),
        splitSymbol("++", L1, L2, L3),
        trimTokens(L2, LL2),
        readExpr(LL2, [], E1), !,
        readExpr(L3, Y, E2), !,
        D = addlist(E1, E2).

    readExpr(X, Y, D) :-
        chopTokens(X, L1),
        splitSymbol("..", L1, L2, L3),
        trimTokens(L2, LL2),
        readExpr(LL2, [], E1), !,
        readExpr(L3, Y, E2),
        D = genlist(E1, E2).

    readExpr(X, Y, D) :-
        chopTokens(X, L0),
        L0 = [N|L1],
        N = lexer::identifier0(_, _, _, _),
        matchSymbols(L1, L2, "("), !,
        readExprList(L2, L3, A),
        chopTokens(L3, L4),
        matchSymbols(L4, Y, ")"),
        D = term(N, A).

    readExpr(X, Y, D) :-
        chopTokens(X, L0),
        L0 = [N|L1],
        N = lexer::identifier1(S, _, _, _),
        not('_' = string::frontChar(S)),
        matchSymbols(L1, L2, "("), !,
        readExprList(L2, L3, A),
        chopTokens(L3, L4),
        matchSymbols(L4, Y, ")"),
        D = funcall(N, A).

    readExpr(X, Y, D) :-
        chopTokens(X, L1),
        splitSymbol("=..", L1, L2, L3),
        trimTokens(L2, LL2),
        readExpr(LL2, [], E1), !,
        readExpr(L3, Y, E2),
        D = univ(E1, E2).

    readExpr(X, Y, D) :-
        chopTokens(X, L),
        L = [H|Y],
        H = lexer::num(_, _, _, _), !,
        D = number(H).

    readExpr(X, Y, D) :-
        chopTokens(X, L),
        L = [H|T],
        H = lexer::underscore(_,_,_), !,
        if (T=[lexer::identifier1(_,_,_,_)|TT]) then
            Y = TT
        else
            Y = T
        end if,
        D = unknown.

    readExpr(X, Y, D) :-
        chopTokens(X, L),
        L = [H|Y],
        H = lexer::identifier1(S, _, _, _), !,
        D = var(H).

    readExpr(X, Y, D) :-
        chopTokens(X, L),
        L = [H|Y],
        H = lexer::str(_, _, _, _), !,
        D = str(H).

    readExpr(X, Y, D) :-
        chopTokens(X, L1),
        matchSymbols(L1, L2, "["),
        readExprList(L2, L3, A),
        chopTokens(L3, L4),
        matchSymbols(L4, L5, "|"),
        readExpr(L5, L6, T), !,
        chopTokens(L6, L7),
        matchSymbols(L7, Y, "]"),
        D = taillist(A,T).

    readExpr(X, Y, D) :-
        chopTokens(X, L1),
        matchSymbols(L1, L2, "["), !,
        readExprList(L2, L3, A), !,
        chopTokens(L3, L4),
        matchSymbols(L4, Y, "]"),
        D = list(A).

    readExpr(X, Y, D) :-
        chopTokens(X, L0),
        matchIdentifier(L0, L1, "call"), !,
        matchSymbols(L1, L2, "("),
        chopTokens(L2, L3),
        L3 = [N|L4],
        N = lexer::identifier1(_, _, _, _),
        chopTokens(L4, L5),
        matchSymbols(L5, L6, ","),
        readExpr(L6, L7, Z),
        chopTokens(L7, L8),
        matchSymbols(L8, Y, ")"),
        D = exprcall1(N, Z).

    readExpr(X, Y, D) :-
        chopTokens(X, L0),
        matchIdentifier(L0, L1, "call"), !,
        matchSymbols(L1, L2, "("),
        readExpr(L2, L3, Z),
        chopTokens(L3, L4),
        matchSymbols(L4, Y, ")"),
        D = exprcall2(Z).

    readExpr(X, Y, D) :-
        chopTokens(X, L1),
        matchSymbols(L1, L2, "-"),
        readExpr(L2, Y, A), !,
        D = negate(A).

    readExpr(X, Y, D) :-
        chopTokens(X, L1),
        matchSymbols(L1, L2, "("),
        readExpr(L2, L3, D), !,
        chopTokens(L3, L4),
        matchSymbols(L4, Y, ")").


    readExprList(X, Y, [H|T]) :-
        readExpr(X, L1, H),
        chopTokens(L1, L2),
        matchSymbols(L2, L3, ","), !,
        readExprList(L3, Y, T).

    readExprList(X, Y, [H]) :-
        readExpr(X, Y, H), !.

    readExprList(L, L, []).


    readIdentifierList(X, Y, [H|T]) :-
        chopTokens(X, L1),
        L1 = [H|L2],
        H = lexer::identifier1( _, _, _, _),
        chopTokens(L2, L3),
        matchSymbols(L3, L4, ","), !,
        readIdentifierList(L4, Y, T).

    readIdentifierList(X, Y, [H]) :-
        chopTokens(X, L),
        L = [H|Y],
        H = lexer::identifier1( _, _, _, _), !.

    readIdentifierList(L, L, []).


    readStatement(X,Z) :-
        matchIdentifier(X, L1, "feature"),
        whiteFirstToken(L1, L2), !,
        L2=[H],
        H = lexer::identifier1(_, _, _, _),
        Z = feature(H).

    readStatement(X,Z) :-
        splitSymbol(";", X, L1, L2),
        whiteLastToken(L1, LL1),
        readStatement(LL1, S1),
        whiteFirstToken(L2, LL2), !,
        readStatement(LL2, S2),
        Z = and_s(S1, S2).

    readStatement(X,Z) :-
        matchIdentifier(X, L1, "extern"), !,
        whiteFirstToken(L1, L2),
        L2=[H],
        H = lexer::identifier1(_, _, _, _),
        Z = extern(H).

    readStatement(X,Z) :-
        matchIdentifier(X, L1, "var"), !,
        whiteFirstToken(L1, L2),
        L2 = [H|L3],
        H = lexer::identifier1(_, _, _, _),
        chopTokens(L3, L4),
        matchSymbols(L4, L5, "="),
        chopTokens(L5, L6),
        readExpr(L6, [], E),
        Z = var(H, E).

    readStatement(L, block_start) :- matchSymbols(L, [], "["), !.

    readStatement(L, block_end) :- matchSymbols(L, [], "]"), !.

    readStatement(L, block_or) :- matchSymbols(L, [], "|"), !.

    %readStatement(L, block_default_or) :- matchSymbols(L, [], "|=>"), !.

    readStatement(L, stop) :- matchIdentifier(L, [], "stop"), !.

    readStatement(X,Z) :-
        matchIdentifier(X, L1, "if"),
        whiteFirstToken(L1, L2),
        readCond(L2, L3, C),
        whiteFirstToken(L3, L4),
        matchIdentifier(L4, L5, "then"),
        whiteFirstToken(L5, L6),
        splitIdentifier("else", L6, M1, M2),
        whiteLastToken(M1, MM1),
        whiteFirstToken(M2, MM2),
        readStatement(MM1, S1), !,
        readStatement(MM2, S2),
        Z = if_then_else(C,S1,S2).

    readStatement(X,Z) :-
        matchIdentifier(X, L1, "if"),
        whiteFirstToken(L1, L2),
        readCond(L2, L3, C),
        whiteFirstToken(L3, L4),
        matchIdentifier(L4, L5, "then"),
        whiteFirstToken(L5, L6),
        readStatement(L6, S), !,
        Z = if_then_s(C,S).

    readStatement(X,Z) :-
        matchIdentifier(X, L1, "if"),
        whiteFirstToken(L1, L2),
        readCond(L2, L3, C),
        whiteFirstToken(L3, L4),
        matchIdentifier(L4, [], "then"), !,
        Z = if_then(C).

    readStatement(X,Z) :-
        matchIdentifier(X, L1, "else"),
        whiteFirstToken(L1, L2),
        matchIdentifier(L2, L3, "if"), !,
        whiteFirstToken(L3, L4),
        readCond(L4, [], C),
        Z = else_if(C).

    readStatement(X,Z) :-
        matchIdentifier(X, [], "else"), !,
        Z = else_s.

    readStatement(X,Z) :-
        matchIdentifier(X, L1, "procedure"),
        whiteFirstToken(L1, L2), !,
        L2 = [N|L3],
        N = lexer::identifier1(_, _, _, _),
        matchSymbols(L3, L4, "("),
        readIdentifierList(L4, L5, A),
        chopTokens(L5, L6),
        matchSymbols(L6, [], ")"),
        Z = defcond(N, A).

    readStatement(X,Z) :-
        matchIdentifier(X, L1, "function"),
        whiteFirstToken(L1, L2), !,
        L2 = [N|L3],
        N = lexer::identifier1(_, _, _, _),
        matchSymbols(L3, L4, "("),
        readIdentifierList(L4, L5, A),
        chopTokens(L5, L6),
        matchSymbols(L6, [], ")"),
        Z = defun(N, A).

    readStatement(X,Z) :-
        matchSymbols(X, L1, "["),
        trimTokens(L1, L2),
        readCond(L2, L3, C), !,
        whiteFirstToken(L3, L4),
        matchSymbols(L4, [], "=>"),
        Z = block_cond_start(C).

    readStatement(X,Z) :-
        matchSymbols(X, L1, "|"),
        trimTokens(L1, L2),
        readCond(L2, L3, C), !,
        whiteFirstToken(L3, L4),
        matchSymbols(L4, [], "=>"),
        Z = block_cond_or(C).

    %readStatement(X,Z) :-
    %    matchSymbols(X, L1, "|"),
    %    chopTokens(L1, L2),
    %    matchSymbols(L2, [], "=>"), !,
    %    Z = block_default_or.

    readStatement(X,Z) :-
        readExpr(X, L1, E),
        chopTokens(L1, L2),
        matchSymbols(L2, L3, "="),
        chopTokens(L3, L4),
        matchIdentifier(L4, L5, "map"),
        matchSymbols(L5, L6, "("), !,
        chopTokens(L6, L7),
        readMajorList(L7, L8, M),
        chopTokens(L8, L9),
        matchSymbols(L9, [], ")"),
        Z = map(E, M).

    readStatement(X,Z) :-
        matchIdentifier(X, L1, "for"),
        matchSymbols(L1, L2, "("), !,
        chopTokens(L2, L3),
        readMajorList(L3, L4, M),
        chopTokens(L4, L5),
        matchSymbols(L5, [], ")"),
        Z = forloop(M).

    readStatement(X,Z) :-
        readExpr(X, L1, E),
        chopTokens(L1, L2),
        matchSymbols(L2, L3, "="),
        chopTokens(L3, L4),
        matchIdentifier(L4, L5, "loop"),
        matchSymbols(L5, L6, "("), !,
        chopTokens(L6, L7),
        L7 = [V|L8],
        V = lexer::identifier1(_, _, _, _),
        chopTokens(L8, L9),
        matchSymbols(L9, L10, "="),
        chopTokens(L10, L11),
        readExpr(L11, L12, I),
        chopTokens(L12, L13),
        matchSymbols(L13, L14, ":"),
        chopTokens(L14, L15),
        readMajorList(L15, L16, M),
        chopTokens(L16, L17),
        matchSymbols(L17, [], ")"),
        Z = loop(E, V, I, M).

    readStatement(X,Z) :-
        readExpr(X, L1, E1),
        chopTokens(L1, L2),
        matchSymbols(L2, L3, "="), !,
        readExpr(L3, [], E2),
        Z = assign(E1, E2).

    readStatement(X,Z) :-
        readCond(X, [], C), !,
        Z = condition(C).

    readStatement(X,Z) :-
        matchIdentifier(X, L1, "return"),
        whiteFirstToken(L1, L2), !,
        readExpr(L2, [], E), !,
        Z = return(E).

    readStatement(X,Z) :-
        matchIdentifier(X, L1, "yeild"),
        whiteFirstToken(L1, L2), !,
        readExpr(L2, [], E), !,
        Z = yeild(E).

    /*readStatement([H|_],_) :-
        lexer::getPosition(H, F, L, _),
        errors::error2("Unable to understand the statement", F, L), !,
        fail.
    readStatement([],_) :- fail. % Should never happen*/


    matchSymbols([H|T], T, "(") :- H = lexer::open1(_, _, _).
    matchSymbols([H|T], T, "{") :- H = lexer::open2(_, _, _).
    matchSymbols([H|T], T, "[") :- H = lexer::open3(_, _, _).
    matchSymbols([H|T], T, ")") :- H = lexer::close1(_, _, _).
    matchSymbols([H|T], T, "}") :- H = lexer::close2(_, _, _).
    matchSymbols([H|T], T, "]") :- H = lexer::close3(_, _, _).
    matchSymbols([H|T], T, S)   :- H = lexer::symbols(S, _, _, _).

    matchIdentifier([H|T], T, S) :- H = lexer::identifier0(S, _, _, _).


    splitSymbol(Z, L, L1, L2) :-
        L=[H|L2],
        H = lexer::symbols(Z, _, _, _),
        L1 = [].

    splitSymbol(Z, L, L1, L2) :-
        L=[H|LL],
        splitSymbol(Z, LL, LL1, L2),
        L1 = [H|LL1].


    splitIdentifier(Z, L, L1, L2) :-
        L=[H|L2],
        H = lexer::identifier0(Z, _, _, _),
        L1 = [].

    splitIdentifier(Z, L, L1, L2) :-
        L=[H|LL],
        splitIdentifier(Z, LL, LL1, L2),
        L1 = [H|LL1].


    parse([]) :- !.

    parse([H|T]) :-
        H =[P|_],
        lexer::getPosition(P, F, L, _),
        readStatement(H,D), !,
        S = line(F, L, D),
        addStatement(S),
        parse(T), !.

    parse([H|T]) :-
        H = [HH|_],
        lexer::getPosition(HH, F, L, _),
        errors::error2("Unable to understand the statement", F, L), !,
        parse(T).


    addStatement(S) :-
        if (S = line(F,N,and_s(S1,S2))) then
            addStatement(line(F,N,S1)), addStatement(line(F,N,S2))
        else
            statements := [S|statements]
        end if.


    startBlock(S) :- S = block_start, !.
    startBlock(S) :- S = block_or, !.
    %startBlock(S) :- S = block_default_or, !.
    startBlock(S) :- S = block_cond_start(_), !.
    startBlock(S) :- S = block_cond_or(_), !.

    createBlocks([], []) :- !.

    createBlocks([H|T], L) :-
        H = line(F, N, block_end), !,
        error2("unexpected ] found", F, N),
        createBlocks(T, L), !.

    createBlocks([H1|T1], [H2|T2]) :-
        H1 = line(_, _, S),
        startBlock(S), !,
        createBlocks1(T1, L, L0),
        H2 = block([H1|L]),
        createBlocks(L0, T2), !.

    createBlocks([H|T1], [H|T2]) :-
        createBlocks(T1, T2).

    createBlocks1([], _, _) :-
        error1("Unexpected end of file", filename), !,
        fail.

    createBlocks1([H|T], [], T) :-  H = line(_, _, block_end), !.
    createBlocks1([H|T], [], [H|T]) :-  H = line(_, _, block_or), !.
    %createBlocks1([H|T], [], [H|T]) :-  H = line(_, _, block_default_or), !.
    createBlocks1([H|T], [], [H|T]) :-  H = line(_, _, block_cond_or(_)), !.

    createBlocks1([H1|T1], [H2|T2], L) :-
        H1 = line(_, _, S),
        startBlock(S), !,
        createBlocks1(T1, L1, L0),
        H2 = block([H1|L1]),
        createBlocks1(L0, T2, L), !.
    
    createBlocks1([H|T1], [H|T2], L) :-
        createBlocks1(T1, T2, L).


    main(F,L,S) :-
        filename := F,
        statements := [],
        parse(L),
        LL = list::reverse(statements),
        createBlocks(LL,S),
        debugParser(S).     %%% DEBUG %%%

    %%% DEBUG %%%
    debugParser(Z) :-
        OutFile = string::concat(filename, ".parse"),
        try
            OutputFile =outputStream_file::create(OutFile),
            debugPrint(OutputFile,Z,0),
            OutputFile:close()
        catch _TraceId do
            stdio::write("DEBUG : Unable to create "),
            stdio::write(OutFile),
            stdio::write("\n")
        end try.

    %%% DEBUG %%%
    debugPrint(Q,Z,N) :-
        foreach X = list::getMember_nd(Z) do
            if (X = block(Y)) then debugPrint(Q,Y,N+1) end if,
            if (X = line(F,LN,S)) then
                debugSpace(Q,N),
                Q:write(F), Q:write(" "), Q:write(LN), Q:write(" : "),
                Q:write(toString(S)), Q:nl
            end if
        end foreach.

    %%% DEBUG %%%
    debugSpace(Q,N) :- if (N>0) then Q:write("    "), debugSpace(Q,N-1) end if.

end implement parser

