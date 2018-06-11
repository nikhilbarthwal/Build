% --------------- Implement Printer  ---------------

implement printer
    open core, structure,parser

class predicates
    printVar: (outputStream, varBlock) determ(i,i).
    printFunc: (outputStream, funcBlock) determ(i,i).
    printCondition: (outputStream, condBlock) determ(i,i).
    printExpr: (outputStream, expr) procedure(i,i).
    printCond: (outputStream, cond) procedure(i,i).
    printPair: (outputStream, pair) determ(i,i).
    printMinorList: (outputStream, minorlist) determ(i,i).
    printMajorList: (outputStream, majorlist) determ(i,i).
    printExprList: (outputStream, expr*) determ(i,i).
    printIdentifierList: (outputStream, lexer::token*) determ(i,i).
    printBlock: (outputStream, structure::statement*, integer) determ(i,i,i).
    printStatement: (outputStream, structure::statement, integer) determ(i,i,i).
    indent: (outputStream, integer) procedure(i,i).

clauses

    indent(F, N) :-
        if (N>0) then
            F:write("\t"),
            indent(F,N-1)
        end if.


    printExpr(F, expr_if(C,E1,E2)) :-
        F:write(" (if "),
        printCond(F,C),
        F:write(" then "),
        printExpr(F,E1),
        F:write(" else "),
        printExpr(F,E2),
        F:write(") "), !.

    printExpr(F, number(T)) :-
        T = lexer::num(N, _, _, _),
        F:write(N), !.

    printExpr(F, str(T)) :-
        T = lexer::str(D, _, _, _),
        F:write("\""), F:write(D), F:write("\""), !.

    printExpr(F, list(L)) :-
        F:write(" ["), printExprList(F,L), F:write("] "), !.

    printExpr(F, taillist(L,T)) :-
        F:write(" ["),
        printExprList(F,L),
        F:write(" | "),
        printExpr(F,T),
        F:write("] "), !.

    printExpr(F, var(V)) :-
        V = lexer::identifier1(D,_,_,_),
        F:write(D), !.

    printExpr(F, unknown) :-
        F:write("_"), !.

    printExpr(F, term(N,L)) :-
        N = lexer::identifier0(D,_,_,_),
        F:write(D),
        F:write("("),
        printExprList(F,L),
        F:write(")"), !.

    printExpr(F, funcall(N,L)) :-
        N = lexer::identifier1(D,_,_,_),
        F:write(D),
        F:write("("),
        printExprList(F,L),
        F:write(")"), !.

    printExpr(F, add(E1,E2)) :-
        F:write("("),
        printExpr(F,E1),
        F:write(" + "),
        printExpr(F,E2),
        F:write(")"), !.

    printExpr(F, sub(E1,E2)) :-
        F:write("("),
        printExpr(F,E1),
        F:write(" - "),
        printExpr(F,E2),
        F:write(")"), !.

    printExpr(F, multiply(E1,E2)) :-
        F:write("("),
        printExpr(F,E1),
        F:write(" * "),
        printExpr(F,E2),
        F:write(")"), !.

    printExpr(F, divide(E1,E2)) :-
        F:write("("),
        printExpr(F,E1),
        F:write(" / "),
        printExpr(F,E2),
        F:write(")"), !.

    printExpr(F, modulus(E1,E2)) :-
        F:write("("),
        printExpr(F,E1),
        F:write(" % "),
        printExpr(F,E2),
        F:write(")"), !.

    printExpr(F, concat(E1,E2)) :-
        F:write("("),
        printExpr(F,E1),
        F:write(" & "),
        printExpr(F,E2),
        F:write(")"), !.

    printExpr(F, join(E1,E2)) :-
        F:write("("),
        printExpr(F,E1),
        F:write(" ^ "),
        printExpr(F,E2),
        F:write(")"), !.

    printExpr(F, addlist(E1,E2)) :-
        F:write("("),
        printExpr(F,E1),
        F:write(" ++ "),
        printExpr(F,E2),
        F:write(")"), !.

    printExpr(F, genlist(E1,E2)) :-
        F:write("("),
        printExpr(F,E1),
        F:write(" .. "),
        printExpr(F,E2),
        F:write(")"), !.

    printExpr(F, univ(E1,E2)) :-
        F:write("("),
        printExpr(F,E1),
        F:write(" =.. "),
        printExpr(F,E2),
        F:write(")"), !.

    printExpr(F, negate(E)) :-
        F:write(" -"),
        printExpr(F,E), !.

    printExpr(F, exprcall1(N,A)) :-
        F:write(" call("),
        N = lexer::identifier1(D,_,_,_),
        F:write(D), F:write(", "),
        printExpr(F,A),
        F:write(") "), !.

    printExpr(F, exprcall2(Z)) :-
        F:write(" call("),
        printExpr(F,Z),
        F:write(") "), !.

    printExpr(_, E) :-
        stdio::write("Fatal Error: "),
        stdio::write(E), stdio::nl,
        errorExit(31).


    printCond(F, cond_if1(C,C1)) :-
        F:write(" (if "),
        printCond(F,C),
        F:write(" then "),
        printCond(F,C1),
        F:write(") "), !.

    printCond(F, cond_if2(C,C1,C2)) :-
        F:write(" (if "),
        printCond(F,C),
        F:write(" then "),
        printCond(F,C1),
        F:write(" else "),
        printCond(F,C2),
        F:write(") "), !.

    printCond(F, memchk(E1,E2)) :-
        F:write(" ("),
        printExpr(F,E1),
        F:write(" in "),
        printExpr(F,E2),
        F:write(") "), !.

    printCond(F, condcall(N,A)) :-
        N = lexer::identifier0(D,_,_,_),
        F:write(D),
        F:write("("),
        printExprList(F,A),
        F:write(") "), !.

    printCond(F, cand(C1,C2)) :-
        F:write(" ("),
        printCond(F,C1),
        F:write(" and "),
        printCond(F,C2),
        F:write(") "), !.

    printCond(F, cor(C1,C2)) :-
        F:write(" ("),
        printCond(F,C1),
        F:write(" or "),
        printCond(F,C2),
        F:write(") "), !.

    printCond(F, cnot(C)) :-
        F:write("not("),
        printCond(F,C),
        F:write(")"), !.

    printCond(F, ge(E1,E2)) :-
        F:write("( "),
        printExpr(F,E1),
        F:write(" >= "),
        printExpr(F,E2),
        F:write(") "), !.

    printCond(F, le(E1,E2)) :-
        F:write("( "),
        printExpr(F,E1),
        F:write(" <= "),
        printExpr(F,E2),
        F:write(") "), !.

    printCond(F, gt(E1,E2)) :-
        F:write("( "),
        printExpr(F,E1),
        F:write(" > "),
        printExpr(F,E2),
        F:write(") "), !.

    printCond(F, lt(E1,E2)) :-
        F:write("( "),
        printExpr(F,E1),
        F:write(" < "),
        printExpr(F,E2),
        F:write(") "), !.

    printCond(F, ne(E1,E2)) :-
        F:write("( "),
        printExpr(F,E1),
        F:write(" != "),
        printExpr(F,E2),
        F:write(") "), !.

    printCond(F, eq(E1,E2)) :-
        F:write("( "),
        printExpr(F,E1),
        F:write(" = "),
        printExpr(F,E2),
        F:write(") "), !.

    printCond(F, condcall1(N,A)) :-
        F:write(" assert("),
        N = lexer::identifier1(S,_,_,_),
        F:write(S), F:write(", "),
        printExprList(F,A),
        F:write(") "), !.

    printCond(F, condcall2(E)) :-
        F:write(" assert("),
        printExpr(F,E),
        F:write(") "), !.

    printCond(_,C) :-
        stdio::write("Fatal Error: "),
        stdio::write(C), stdio::nl,
        errorExit(32).


    printExprList(_, []) :- !.
    printExprList(F, [H]) :- printExpr(F,H), !.
    printExprList(F, [H|T]) :- printExpr(F,H), F:write(", "), printExprList(F,T), !.


    printIdentifierList(_, []) :- !.

    printIdentifierList(F, [H]) :-
        H=lexer::identifier1(S,_,_,_),
        F:write(S), !.

    printIdentifierList(F, [H|T]) :-
        H=lexer::identifier1(S,_,_,_),
        F:write(S),
        F:write(", "),
        printIdentifierList(F,T), !.


    printVar(F, V) :-
        F:write("var "),
        V = varBlock(N, E),
        N = lexer::identifier1(S, _, _, _),
        F:write(S),
        F:write(" = "),
        printExpr(F,E), !.


    printFunc(F, cFunBlock(N,I,C,B)) :-
        F:write("function "),
        N = lexer::identifier1(S,_,_,_),
        F:write(S),
        F:write("("),
        printIdentifierList(F,I),
        F:write(")"),
        F:nl,
        F:write("[ "), printCond(F,C), F:write(" => "), F:nl,
        printBlock(F,B,1),
        F:write("]"), F:nl, !.

    printFunc(F, gFunBlock(N,I,B)) :-
        F:write("function "),
        N = lexer::identifier1(S,_,_,_),
        F:write(S),
        F:write("("),
        printIdentifierList(F,I),
        F:write(")"),
        F:nl,
        F:write("[ "), F:nl,
        printBlock(F,B,1),
        F:write("]"), F:nl, !.


    printCondition(F, cCondBlock(N,I,C,B)) :-
        F:write("condition "),
        N = lexer::identifier1(S,_,_,_),
        F:write(S),
        F:write("("),
        printIdentifierList(F,I),
        F:write(")"),
        F:nl,
        F:write("[ "), printCond(F,C), F:write(" => "), F:nl,
        printBlock(F,B,1),
        F:write("]"), F:nl, !.

    printCondition(F, gCondBlock(N,I,B)) :-
        F:write("condition "),
        N = lexer::identifier1(S,_,_,_),
        F:write(S),
        F:write("("),
        printIdentifierList(F,I),
        F:write(")"),
        F:nl,
        F:write("[ "), F:nl,
        printBlock(F,B,1),
        F:write("]"), F:nl, !.


    printPair(F,pair(P,E)) :-
        P=lexer::identifier1(S,_,_,_),
        F:write(S),
        F:write(" in "),
        printExpr(F,E).


    printMinorList(_, []) :- !.
    printMinorList(F,[H]) :- printPair(F,H), !.
    printMinorList(F,[H|T]) :-
        printPair(F,H),
        F:write(", "),
        printMinorList(F,T), !.


    printMajorList(_, []) :- !.
    printMajorList(F,[H]) :- printMinorList(F,H), !.
    printMajorList(F,[H|T]) :-
        printMinorList(F,H),
        F:write(", "),
        printMajorList(F,T), !.


    printStatement(F, unify(E1, E2), N) :-
        indent(F,N),
        F:write("unify "),
        printExpr(F,E1),
        F:write(" :: "),
        printExpr(F,E2), !.

    printStatement(F, structure::yeild(E), N) :-
        indent(F,N),
        F:write("yeild "),
        printExpr(F,E), !.

    printStatement(F, structure::return(E), N) :-
        indent(F,N),
        F:write("return "),
        printExpr(F,E), !.

    printStatement(F, structure::stop, N) :-
        indent(F,N),
        F:write("stop"), !.

    printStatement(F, structure::check(C), N) :-
        indent(F,N),
        F:write("check "),
        printCond(F,C), !.

    printStatement(F, map(E,M,B), N) :-
        indent(F,N),
        printExpr(F,E),
        F:write(" = map("),
        printMajorList(F,M),
        F:write(")"), F:nl,
        indent(F,N),
        F:write("["),
        printBlock(F,B,N+1),
        indent(F,N), F:write("]"), !.

    printStatement(F, structure::if_then_else(C,B1,B2), N) :-
        indent(F,N),
        F:write("if "),
        printCond(F,C),
        F:write(" then"), F:nl,
        indent(F,N), F:write("["), F:nl,
        printBlock(F,B1,N+1),
        indent(F,N), F:write("]"), F:nl,
        if (B2=[_H|_T]) then
            indent(F,N), F:write("else"), F:nl,
            indent(F,N), F:write("["), F:nl,
            printBlock(F,B2,N+1),
            indent(F,N), F:write("]"), F:nl
        end if, !.

    printStatement(F, loop(E,V,I,M,B), N) :-
        indent(F,N),
        printExpr(F,E),
        F:write(" = loop("),
        V = lexer::identifier1(S,_,_,_),
        F:write(S),
        F:write(" = "),
        printExpr(F,I),
        F:write(" : "),
        printMajorList(F,M),
        F:write(")"), F:nl,
        indent(F,N),
        F:write("["),
        printBlock(F,B,N+1),
        indent(F,N), F:write("]"), !.

    printStatement(F, forloop(M,B), N) :-
        indent(F,N),
        F:write("foreach("),
        printMajorList(F,M),
        F:write(")"), F:nl,
        indent(F,N),
        F:write("["),
        printBlock(F,B,N+1),
        indent(F,N), F:write("]"), !.

    printBlock(_,[],_) :- !.
    printBlock(F,[H],N) :- printStatement(F,H,N), F:nl, !.
    printBlock(F,[H|T],N) :- printStatement(F,H,N), F:nl, printBlock(F,T,N), !.


    main(FN, P) :-
        P = p(N, E, V, F, C),
        if not(N = "_") then
            OutFile = string::concat(FN, ".program"),
            try
                OutputFile =outputStream_file::create(OutFile),
                OutputFile:write("Feature:  "),
                OutputFile:write(N),
                OutputFile:write("\n\n"),
                foreach X = list::getMember_nd(E) do
                    if (X = lexer::identifier1(ED, _, _, _)) then
                        OutputFile:write("extern "),
                        OutputFile:write(ED),
                        OutputFile:nl
                    end if
                end foreach,
                OutputFile:write("\n"),
                foreach X = list::getMember_nd(V) do
                    if (printVar(OutputFile, X)) then OutputFile:write("\n") end if
                end foreach,
                OutputFile:write("\n"),
                foreach X = list::getMember_nd(F) do
                    if (printFunc(OutputFile, X)) then OutputFile:write("\n") end if
                end foreach,
                OutputFile:write("\n"),
                foreach X = list::getMember_nd(C) do
                    if (printCondition(OutputFile, X)) then OutputFile:write("\n") end if
                end foreach,
                OutputFile:write("\n"),
                OutputFile:close()
            catch _TraceId do
                stdio::write("DEBUG : Unable to create "),
                stdio::write(OutFile),
                stdio::write("\n")
            end try
        end if.

end implement printer

