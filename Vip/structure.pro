% --------------- Implement Structure  ---------------
implement structure
    open core, parser, lexer

domains
    blockType = func; cond; ifthenelse; maplist; loopbody; forloop.

class facts
    name:string :="_".
    defExtern:lexer::token* := [].
    defVar: varBlock* := [].
    defFunc:funcBlock* := [].
    defCond:condBlock* := [].

class predicates
    getHeader: (pstatement*, pstatement*) determ(i,o).
    processTopLevel: (pstatement*) determ(i).
    processBlock: (pstatement*, statement*, blockType) determ(i,o,i).
    processIfBlock: (pstatement* Input, pstatement* Output, statement*) determ(i,o,o).

    debugStructure: (string) procedure.  %%% DEBUG %%%

clauses

    processTopLevel([]) :- !.

    processTopLevel([H|T]) :-
        H = line(_,_,extern(K)), !,
        defExtern := [K|defExtern],
        processTopLevel(T), !.

    processTopLevel([H|T]) :-
        H = line(_,_,var(V,E)), !,
        defVar := [varBlock(V,E)|defVar],
        processTopLevel(T), !.

    processTopLevel([H1,H2|T]) :-
        H1 = line(_,_,defun(N,A)),
        H2 = parser::block([BH|BT]), !,
        processBlock(BT, B, func),
        BH = line(F, LN, S),
        if (S = block_cond_start(C)) then
            Z = cFunBlock(N,A,C,B)
        elseif (S = block_cond_or(C)) then
            Z = cFunBlock(N,A,C,B)
        elseif (S = block_start) then
            Z = gFunBlock(N,A,B)
        elseif (S = block_or) then
            Z = gFunBlock(N,A,B)
        else
            stdio::write("Fatal error, expecting block start here", F, LN),
            stdio::nl, errorExit(21)
        end if,

        defFunc := [Z|defFunc],
        if (T = [parser::block(_)|_]) then
            processTopLevel([H1|T])
        else
            processTopLevel(T)
        end if, !.

    processTopLevel([H|T]) :-
        H = line(F,L,defun(N,_A)), !,
        N = identifier1(S,_,_,_),
        Msg = string::concat("No body found for function ", S),
        errors::error2(Msg,F,L),
        processTopLevel(T), !.

    processTopLevel([H1,H2|T]) :-
        H1 = line(_,_,defcond(N,A)),
        H2 = parser::block([BH|BT]), !,
        processBlock(BT, B, cond),

        BH = line(F, LN, S),
        if (S = block_cond_start(C)) then
            Z = cCondBlock(N,A,C,B)
        elseif (S = block_cond_or(C)) then
            Z = cCondBlock(N,A,C,B)
        elseif (S = block_start) then
            Z = gCondBlock(N,A,B)
        elseif (S = block_or) then
            Z = gCondBlock(N,A,B)
        else
            stdio::write("Fatal error, expecting block start here", F, LN),
            stdio::nl, errorExit(22)
        end if,

        defCond := [Z|defCond],
        if (T = [parser::block(_)|_]) then
            processTopLevel([H1|T])
        else
            processTopLevel(T)
        end if, !.

    processTopLevel([H|T]) :-
        H = line(F,L,defcond(N,_A)),
        N = identifier1(S,_,_,_),
        Msg = string::concat("No body found for procedure ", S),
        errors::error2(Msg,F,L),
        processTopLevel(T), !.

    processTopLevel([H|T]) :-
        H = line(F,L,_),
        errors::error2("Misplaced statement found", F, L),
        processTopLevel(T), !.


    processBlock([], [], _) :- !.

    processBlock([H|T], L, M) :-
        H = line(_, _, block_start), !,
        processBlock(T, L, M).

    processBlock([H1|T1], T2, M) :-
        % H1 = parse::block(parser::block(_)) is a fatal error
        H1 = parser::block([line(F, N, _)|B]), !, %TODO: Error to have stray blocks
        errors::error2("Stray block found", F, N),
        processBlock(B, _BB, M),
        processBlock(T1, T2, M).

    processBlock([H|T], L, M) :-
        H = line(F, N, block_or), !,
        errors::error2("Unexpected | found", F, N),
        processBlock(T, L, M).

    processBlock([H|T], L, M) :-
        H = line(F, N, block_cond_start(_)), !,
        errors::error2("Unexpected conditional [ found", F, N),
        processBlock(T, L, M).

    processBlock([H|T], L, M) :-
        H = line(F, N, block_cond_or(_)), !,
        errors::error2("Unexpected conditional | found", F, N),
        processBlock(T, L, M).

    processBlock([H1|T1], [H2|T2], M) :-
        H1 = line(F , LN, parser::if_then_else(C, S1, S2)), !,
        B1 = [line(F, LN, S1)],
        B2 = [line(F, LN, S2)],
        processBlock(B1, BB1, ifthenelse),
        processBlock(B2, BB2, ifthenelse),
        H2 = if_then_else(C, BB1, BB2),
        processBlock(T1, T2, M), !.

    processBlock([H1|T1], [H2|T2], M) :-
        H1 = line(F , LN, if_then_s(C, S)), !,
        B = [line(F, LN, S)],
        processBlock(B, BB, ifthenelse),
        H2 = if_then_else(C, BB, []),
        processBlock(T1, T2, M), !.

    processBlock([H1, H2, H3, H4|T1], [HH|T2], M) :-
        H1 = line(_, _, if_then(C)),
        H2 = parser::block(B1),
        H3 = line(_, _, else_s),
        H4 = parser::block(B2), !,
        processBlock(B1, BB1, ifthenelse),
        processBlock(B2, BB2, ifthenelse),
        HH = if_then_else(C, BB1, BB2),
        processBlock(T1, T2, M), !.

    processBlock([H1, H2, H3, H4|T], L, M) :-
        H1 = line(_, _, if_then(_)),
        H2 = parser::block(B),
        processBlock(B, _BB, ifthenelse),
        H3 = line(_, _, else_s),
        H4 = line(F, LN, _), !,
        errors::error2("Expecting block defination here", F, LN),
        processBlock([H4|T], L, M), !.

    processBlock([H1, H2|T], [H|L], M) :-
        H1 = line(_, _,if_then(C)),
        H2 = parser::block(B), !,
        processBlock(B, BB1, ifthenelse),
        processIfBlock(T, TT, BB2),
        H = if_then_else(C, BB1, BB2),
        processBlock(TT, L, M), !.

    processBlock([H1, H2|T], L, M) :-
        H1 = line(_, _,if_then(_)),
        H2 = line(F, LN, _), !,
        errors::error2("Expecting block defination here", F, LN),
        processBlock([H2|T], L, M), !.

    processBlock([H|T], L, M) :-
        H = line(F, LN, else_s), !,
        errors::error2("Unexpected else found", F, LN),
        processBlock(T, L, M), !.

    processBlock([H|T], L, M) :-
        H = line(F, LN, else_if(_)), !,
        errors::error2("Unexpected else found", F, LN),
        processBlock(T, L, M), !.

    processBlock([H1|T1], [H2|T2], M) :-
        H1 = line(_, _, parser::yeild(E)),
        list::isMember(M, [maplist, loopbody]), !,
        H2 = yeild(E),
        processBlock(T1, T2, M), !.

    processBlock([H|T], L, M) :-
        H = line(F, N, parser::yeild(_)), !,
        errors::error2("Unexpected yeild found", F, N),
        processBlock(T, L, M), !.

    processBlock([H1, H2|T], [HH|TT], M) :-
        H1 = line(_, _, map(E, L)),
        H2 = parser::block(B), !,
        processBlock(B, BB, maplist),
        HH = map(E,L,BB),
        processBlock(T, TT, M), !.

    processBlock([H1, H2|T], L, M) :-
        H1 = line(_, _, map(_, _)),
        H2 = line(F, N, _), !,
        errors::error2("Expecting block here", F, N),
        processBlock([H2|T], L, M), !.

    processBlock([H1, H2|T], [HH|TT], M) :-
        H1 = line(_, _, loop(E, V, I, L)),
        H2 = parser::block(B), !,
        processBlock(B, BB, loopbody),
        HH = loop(E, V, I, L, BB),
        processBlock(T, TT, M), !.

    processBlock([H1, H2|T], L, M) :-
        H1 = line(_, _, loop(_, _, _, _)),
        H2 = line(F, N, _), !,
        errors::error2("Expecting block here", F, N),
        processBlock([H2|T], L, M), !.

    processBlock([H1, H2|T], [HH|TT], M) :-
        H1 = line(_, _, forloop(L)),
        H2 = parser::block(B), !,
        processBlock(B, BB, forloop),
        HH = forloop(L, BB),
        processBlock(T, TT, M), !.

    processBlock([H1, H2|T], L, M) :-
        H1 = line(_, _, forloop(_)),
        H2 = line(F, N, _), !,
        errors::error2("Expecting block here", F, N),
        processBlock([H2|T], L, M), !.

    processBlock([H1|T1], [H2|T2], func) :-
        H1 = line(_, _, parser::return(E)), !,
        H2 = return(E),
        processBlock(T1, T2, func), !.

    processBlock([H|T], L, M) :-
        H = line(F, N, parser::return(_)), !,
        errors::error2("Unexpected return found", F, N),
        processBlock(T, L, M), !.

    processBlock([H1|T1], [H2|T2], M) :-
        H1 = line(_, _, parser::stop), !,
        H2 = stop,
        processBlock(T1, T2, M), !.

    processBlock([H1|T1], [H2|T2], M) :-
        H1 = line(_, _, parser::assign(E1, E2)), !,
        H2 = unify(E1, E2),
        processBlock(T1, T2, M), !.

    processBlock([H|T], L, M) :-
        H = line(F, N, _), !,
        errors::error2("Unexpected Statement found", F, N),
        processBlock(T, L, M), !.


    processIfBlock([H1,H2|T], L, BB) :-
        H1 = line(_, _, parser::else_s), !,
        if (H2 = line(F, N, _)) then
            errors::error2("Expecting block here", F, N),
            L =[H2|T], BB = []
        elseif (H2 = parser::block(B)) then
            processBlock(B, BB, ifthenelse), L=T
        else
            stdio::write(" Should be line or Block ", H2),
            errorExit(23)
        end if.

    processIfBlock([H1,H2|T], L, S) :-
        H1 = line(_, _, parser::else_if(C)), !,
        if (H2 = line(F, N, _)) then
            errors::error2("Expecting block here", F, N),
            L =[H2|T], S = []
        elseif (H2 = parser::block(B)) then
            processBlock(B, BB, ifthenelse),
            processIfBlock(T, L, SS),
            S = [if_then_else(C, BB, SS)]
            else
                L=[], S = [if_then_else(C, [], [])] % Should never happen
        end if.

    processIfBlock(L, L, []) :- !.


    main(F, L, P) :-
        name := "_",
        defExtern := [],
        defVar := [],
        defFunc := [],
        defCond := [],
        getHeader(L, S),
        processTopLevel(S),
        errors::check(), %%% DEBUG %%%
        debugStructure(F), %%% DEBUG %%%
        P = p(name, list::reverse(defExtern), list::reverse(defVar), list::reverse(defFunc), list::reverse(defCond)).


    getHeader(L, LL) :-
        L=[H|T],
        H = line(F,N,S),
        if (S = feature(identifier1(D,_,_,_))) then
            name := D,
            LL = T
        else
            errors::error2("Expecting Feature defination here", F, N),
            LL = L
        end if.


    %%% DEBUG %%%
    debugStructure(F) :-
        if not(name="_") then
            OutFile = string::concat(F, ".struct"),
            try
                OutputFile =outputStream_file::create(OutFile),
                OutputFile:write("Feature:  "),
                OutputFile:write(name),
                OutputFile:write("\n\nExtern:\n"),
                foreach X = list::getMember_nd(defExtern) do
                    OutputFile:write("\t"),
                    OutputFile:write(toString(X)),
                    OutputFile:write("\n")
                end foreach,
                OutputFile:write("\n"),
                OutputFile:write("Vars:\n"),
                foreach X = list::getMember_nd(defVar) do
                    OutputFile:write("\t"),
                    OutputFile:write(toString(X)),
                    OutputFile:write("\n")
                end foreach,
                OutputFile:write("\n"),
                OutputFile:write("Func:\n"),
                foreach X = list::getMember_nd(defFunc) do
                    OutputFile:write("\t"),
                    OutputFile:write(toString(X)),
                    OutputFile:write("\n")
                end foreach,
                OutputFile:write("\n"),
                OutputFile:write("Proc:\n"),
                foreach X = list::getMember_nd(defCond) do
                    OutputFile:write("\t"),
                    OutputFile:write(toString(X)),
                    OutputFile:write("\n")
                end foreach,
                OutputFile:write("\n"),
                OutputFile:close()
            catch _TraceId do
                stdio::write("DEBUG : Unable to create "),
                stdio::write(OutFile),
                stdio::write("\n")
            end try
        end if.

end implement structure

