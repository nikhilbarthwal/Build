% --------------- Implement Execute  ---------------

implement excute
    open core, structure, parser, lexer



domains
    var = (string, expr).
    vars = var*.

class facts
    temp : expr := dummy.
    templist : expr* := [].
    filename : string = "".
    feature : string  = "".
    externs : token*  = [].
    vardef : varBlock* = [].
    fundef : funcBlock* = [].
    conddef : condBlock* = [].


class predicates

    addVar(vars In, vars Out, string Var, expr Val) procedure(i,o,i,i).
    checkVar(vars, string, expr) determ(i,i,o).
    execute(vars, statement*, bool Return, expr Val) determ(i,i,i,o)
    unify(vars T1, vars T2, expr E1, expr E2) determ(i,o,i,i)
    unifyList(vars T1, vars T2, expr* E1, expr* E2, expr* E) determ(i,o,i,i,i) (i,o,i,i,o).


clauses

    addVar(T0, T1, V, E) :-
        if (checkVar(T0, V, E0)) then
            E = E0
    else
        T1 = [(V, E)| T0 ]
    end if.


    checkVar(_, V, E) :- list::isMember(vardef, X), X=(V, E).
    checkVar(T, V, E) :- list::isMember(T, X), X=(V, E).
    



    execute(_,  [], true, _) :-
        % Pass the Block position ***************************
        stdio::write("Block ended without return"),
        errorExit(0).

    execute(_,  [], false, dummy) :- !.

    execute(_,  [S|_], false, dummy) :-
        S = return(_),
        % Pass the Block position ***************************
        stdio::write("Unexpected return statement found"),
        errorExit(0).

    execute(Vars, [S|_],  true, Val) :-
        S = return(E),
        Val = eval(Var, E). % code: eval ****

    execute(VarsIn, [S|L], R, Val) :-
        S = unify(U1, U2),
        unify(VarsIn, VarsOut, U1, U2), % code: unify ****
        execute(VarsOut, L, R, Val).

    execute(Vars, [S|L], R, Val) :-
        S = check(C),
        check(Vars,C),  % code: check ****
        execute(Vars, L, R, Val).

    execute(VarsIn, [S|L],  R, Val) :-  
        S = map(E,M,B),
        checkMajorList(Vars,M), % code: checkMajorList ****
        MM = enumMajorList(M), % code: enumMajorList ****
        Eval = executeMap([], MM, VarsIn, B), % code: executeMap ****
        unify(VarsIn, VarsOut, E, Eval),
        execute(VarsOut, L, R, Val).

    execute(VarsIn, [S|L],  R, Val) :- 
        S = loop(E1, Z, X, M, Block),
        if (checkVar(VarsIn, X, _)) then
            stdio::write("Variable already exists"),
        end if,
        Y = eval(VarsIn, X),
        evarMajorList(VarsIn, M,MM),
    temp := Y,
        executeLoop(Z, Y, VarsIn, Block, MM),  % code: executeLoop ****
        unify(VarsIn, VarsOut, E1, temp),
        execute(VarsOut, L, R, Val).




    % unify(T0, T1, E0, E1)

    unify(T, T, unknown, _E) :- !.

    unify(T1, T2, list(L), E2) :-
        list(LL) = eval(T1, E),
        unifyList(T1, T2, L, LL, []), !.

    unify(T1, T2, var(X), E) :-
        EE = eval(T1, E),
        X = identifier1(V, _, _, _),
        T2 = [(V, EE)| T1 ], !.

    unify(V1, V3, taillist(H,T), E) :-
        list(HH) = eval(V1, E),
        unifyList(V1, V2, H, HH, TT),
        unify(V2, V3, T, TT), !.

    unify(T1, T2, term(N, A1), E) :-
        term(N, A2) = eval(T1, E),
        unifyList(T1, T2, A1, A2, []), !.



    unifyList(T1, T2, [], L, L) :- !.

    unifyList(T1, T2, [H1|L1], [H2|L2], L) :-
        unify(T1, T, H1, H2),
        unifyList(T, T2, L1, L2, L), !.



    eval(T, E) = EE :-
        if (E = expr_if(C, E1, E2)) then
            if (check(T,C)) then EE = eval(T, E1) else EE = eval(T, E2) end if

        elseif (E = number(_)) then
            EE = E

        elseif (E = str(_)) then
            EE = E

        elseif (E = list(L)) then
            EE = evalList(T, L)

        elseif (E = taillist(H, L)) then
            EH = evalList(T, H),
            ET = eval(T,L),
            EE - list::append(E1, E2)

        elseif ((E = var(identifier1(X,F, L, P)) and checkVar(T, X, E0)) then
            EE = E0

        elseif ((E = term(N, A)) then
            AA = evalList(T, A),
            EE = term(N, AA)

        elseif (E = funcall(F, A) then
            if (func(R, F, A)) then
                EE = E
            else
                stdio::write("Unable to evaluate function F"), stdio::nl, errorExit(1)
            end if
                

        elseif (E = add(E1, E2)) then
            EE1 = eval(T, E1),
            EE2 = eval(T, E2),
            if (EE1 = number(X1)) then
                if (EE2 = number((X2)) then
                    X = X1 + X2, EE = number(X)
                else
                    stdio::write("Expecting number", EE2), stdio::nl
                end if
            else
                stdio::write("Expecting number", EE2), stdio::nl, errorExit(1)
            end if

        elseif (E = sub(E1, E2)) then
            EE1 = eval(T, E1),
            EE2 = eval(T, E2),
            if (EE1 = number(X1)) then
                if (EE2 = number((X2)) then
                    X = X1 - X2, EE = number(X)
                else
                    stdio::write("Expecting number", EE2), stdio::nl
                end if
            else
                stdio::write("Expecting number", EE2), stdio::nl
            end if

        elseif (E = multiply(E1, E2)) then
            EE1 = eval(T, E1),
            EE2 = eval(T, E2),
            if (EE1 = number(X1)) then
                if (EE2 = number((X2)) then
                    X = X1 * X2, EE = number(X)
                else
                    stdio::write("Expecting number", EE2), stdio::nl
                end if
            else
                stdio::write("Expecting number", EE2), stdio::nl
            end if


        elseif (E = divide(E1, E2)) then
            EE1 = eval(T, E1),
            EE2 = eval(T, E2),
            if (EE1 = number(X1)) then
                if (EE2 = number((X2)) then
                    X = X1 // X2, EE = number(X)
                else
                    stdio::write("Expecting number", EE2), stdio::nl
                end if
            else
                stdio::write("Expecting number", EE2), stdio::nl
            end if


        elseif (E = modulus(E1, E2)) then
            EE1 = eval(T, E1),
            EE2 = eval(T, E2),
            if (EE1 = number(X1)) then
                if (EE2 = number((X2)) then
                    X = X1 % X2, EE = number(X)
                else
                    stdio::write("Expecting number", EE2), stdio::nl
                end if
            else
                stdio::write("Expecting number", EE2), stdio::nl
            end if

        elseif (E = concat(E1, E2)) then
            EE1 = eval(T, E1),
            EE2 = eval(T, E2),
            if (EE1 = str(X1)) then
                if (EE2 = str((X2)) then
                    X = string::concat(X1, X2), EE = str(X)
                else
                    stdio::write("Expecting string ", EE2), stdio::nl
                end if
            else
                stdio::write("Expecting string ", EE2), stdio::nl
            end if

        elseif (E = join(EH, ET)) then
            HH = eval(T, EH),
            TT = eval(T, ET),
            if (TT = list(L)) then
                EE = list([HH|L])
            else
                stdio::write("Expecting list ", TT), stdio::nl
            end if

        elseif (E = addlist(E1, E2)) then
            EE1 = eval(T, E1),
            EE2 = eval(T, E2),
            if (EE1 = list(X1)) then
                if (EE2 = list((X2)) then
                    X = list::concat(X1, X2), EE = list(X)
                else
                    stdio::write("Expecting list ", E2), stdio::nl
                end if
            else
                stdio::write("Expecting list ", E2), stdio::nl
            end if          

        elseif (E = genlist(E1, E2)) then
            EE1 = eval(T, E1),
            EE2 = eval(T, E2),
            if (EE1 = number(X1)) then
                if (EE2 = number((X2)) then
                    if (X1>X2) then L = generate(X2, X1) else L = generate(X1, X2) end if,
                    EE = list(L)
                else
                    stdio::write("Expecting number", EE2), stdio::nl
                end if
            else
                stdio::write("Expecting number", EE2), stdio::nl
            end if

        elseif (E = negate(E0)) then
            E1 = eval(T, E0),
            if (E1 = number(N)) then
                EE = number(-1*N)
            else
                stdio::write("Expecting number ", E0), stdio::nl
            end if


        elseif (E = call(identifier1(X,F, L, P), A)) then
            E1 = eval(T, E0),
            AA = evalList(T, A),
            EE = unknown.

        else
            stdio::write("Unable to eveluate expression"),
            errorExit(1)
        end if.



        % evalList(vars, expr*) -> expr*

        evalList(T, X) = Y :-
            if (X=[H|L]) then
                HH = eval(T,H),
                LL = eval(T,L),
                Y = [HH|LL]
            else
                Y = []
            end if.



    evalMajorList(V, [], [])  :- !.

    evalMajorList(V, [H|T], [HH|TH]) :- HH = evalMinorList(V, H), TT = evalMajorList(V, T).



    evalMinorList(V, [], []) :- !.

    evalMinorList(V, [H|T], [HH|TT]) :- 
        H = pair(Z, E),
        Z = token::identifier1(S, F, L, P),
        if (checkVar(V, S, _)) then
        errors::error3("Variable is already present", F, L, P),
        errorExit(1)
        else
        eval(V, E, EE),
        if (EE = list(_)) then
            HH = pair(Z, EE)
        else
            stdio::write("EE does not evaluate to a list"), stdio::nl,
            errorExit(1)
        end if
        end if,
        evalMinorList(V, T, TT).



        executeLoop(Z, V, B, M) :- 
        initMajoList(M, V, VV),     
        if (addVar(VV, VVV, Z, temp)) then 
            if (processBlock(VVV, B, true, Return)) then
                temp := Return, fail
            else
                !, fail
            end if
        else % Add variable fails
            stdio::write("This Z Var is already exists"),
            errorExit(1)
        end if.

        executeLoop(_, _, _, _) :- !.



        executeMap(M, V, B) :- 
        initMajoList(M, V, VV),     
        if (processBlock(VV, B, true, Return)) then
            tempList := [Return|tempList]
        end if,
        fail.

        executeMap(_, _, _) :- !.


    initMajoList([], V, V) :- !.

    initMajoList([H|T], V0, V1) :-
        initMinorList(H, V0, V),
        initMajorList(T, V, V1).
        


    initMinorList([], V, V) :- !.

    initMinorList([H|T], V0, V1) :-
        H = pair(S, E),
        add(V0, V, S, E),
        initMinorList(T, V, V1), !.


/*


func & check to be done


-- Improce MajorList structure
-- Remove Yeild
-- Fix Expressions
-- Pass Block positions
-- Centralize Fatal Error
-- Centralize Runtime Error


F = Eval (V, E) is procedure
Unify(V, VV, E1, E2) is determ
check(V,C) is determ

Map(E, M, B, V) :- is procedure
    L = enumerate(V,M),  Check: M and V should not have duplicate variables
    Final = executeMap(Init=[], L, V, B)
    Return reverse(Final).
    
Loop(E , X, I, M, B, V)  :-
     Check if X is par of V
    Y = Eval I
    L = enumerate(V,M),  Check: M and V should not have duplicate variables
    executeLoop(Final, Loop Var Name , Init, V is Var Table,  B, L),
    E=Final.
    

program = program(string Name, lexer::token* ExternList, varBlock*, funcBlock*, condBlock*).

    varBlock = varBlock(lexer::token Name, parser::expr Value).

    funcBlock =
        cFunBlock(lexer::token Name, lexer::token* IdentifierList, parser::cond Condition, statement* Body) ;
        gFunBlock(lexer::token Name, lexer::token* IdentifierList, statement* Body).

    condBlock =
        cCondBlock(lexer::token Name, lexer::token* IdentifierList, parser::cond Condition, statement* Body) ;
        gCondBlock(lexer::token Name, lexer::token* IdentifierList, statement* Body).

    statement =
        unify(parser::expr, parser::expr) ;
        yeild(parser::expr) ;
        %block(statement*) ; Nested blocks are not allowed!
        check(parser::cond) ;  condition
        map(parser::expr, parser::majorlist, statement* Block) ;
        if_then_else(parser::cond If, statement* Then, statement* Else) ;
        loop(parser::expr , lexer::token V, parser::expr I, parser::majorlist, statement* Block) ;
        forloop(parser::majorlist, statement* Block) ;
        return(parser::expr) ;  only for function
        stop.

    */
