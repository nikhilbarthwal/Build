% --------------- Class Parser ---------------

class parser
    open core, lexer

domains

    pair = pair(token, expr).

    minorlist = pair*.

    majorlist = minorlist*.

    statement =
        and_s(statement S1, statement S2) ;                 % S1 ; S2
        if_then_else(cond C, statement S1, statement S2);   % if C then S1 else S2
        if_then_s(cond C, statement S) ;                    % if C then S
        if_then(cond C) ;                                   % if C then
        else_s ;                                            % else
        else_if(cond C) ;                                   % elseif C
        feature(token N) ;                                  % feature N
        extern(token V) ;                                   % extern V
        var(token V, expr E) ;                              % var V = E
        block_start ;                                       % [
        block_or ;                                          % |
        block_end ;                                         % ]
        defcond(token, token*) ;                            % condition C(A1, A2, ...)
        defun(token, token*) ;                              % function F(A1, A2, ...)
        block_cond_start(cond) ;                            % [ condition =>
        block_cond_or(cond) ;                               % | condition =>
        assign(expr, expr) ;                                % expr = expr
        condition(cond) ;                                   % condition
        forloop(majorlist) ;                                % for(majorlist)
        map(expr Z, majorlist) ;                            % Z = map(majorlist)
        loop(expr E, token V, expr I, majorlist) ;          % E = loop(V=I: majorlist)
        yeild(expr Z) ;                                     % yeild Z        
        return(expr Z) ;                                    % return Z
        stop ;                                              % stop
        dummy.                                              % Error control

    cond =
        cond_if1(cond C, cond C1) ;                 % if C then C1
        cond_if2(cond C, cond C1, cond C2) ;        % if C then C1 else C2
        memchk(expr E1, expr E2) ;                  % E1 in E2
        condcall(token N, expr* A) ;                % n(A1, A2, A3, ...)
        cand(cond C1, cond C2) ;                    % C1 and C2
        cor(cond C1, cond C2) ;                     % C1 or C2
        cnot(cond C) ;                              % not(C)
        ge(expr E1, expr E2) ;                      % E1 >= E2 -or- E1 => E2
        gt(expr E1, expr E2) ;                      % E1 > E2
        le(expr E1, expr E2) ;                      % E1 <= E2 -or- E1 =< E2
        lt(expr E1, expr E2) ;                      % E1 < E2
        ne(expr E1, expr E2) ;                      % E1 != E2
        eq(expr E1, expr E2) ;                      % E1 = E2
        condcall1(token N, expr* A) ;               % assert(N, A)
        condcall2(expr E).                          % assert(E)

    expr =
        expr_if(cond C, expr E1, expr E2) ;         % if C then E1 else E2
        number(token) ;                             % 1234
        str(token) ;                                % "abcd..."
        list(expr* A) ;                             % [A1, A2, A3, ...]
        taillist(expr* A, expr T) ;                 % [A1, A2, A3, ... | T]
        var(token) ;                                % X
        unknown ;                                   % _X
        term(token N, expr* A) ;                    % n(A1, A2, A3 ...)
        funcall(token N, expr* A) ;                 % N(A1, A2, A3 ...)
        add(expr E1, expr E2) ;                     % E1 + E2
        sub(expr E1, expr E2) ;                     % E1 - E2
        multiply(expr E1, expr E2) ;                % E1 * E2
        divide(expr E1, expr E2) ;                  % E1 / E2
        modulus(expr E1, expr E2) ;                 % E1 % E2
        concat(expr E1, expr E2) ;                  % E1 & E2, string concatenation
        join(expr H, expr T) ;                      % H ^ T, H is Head and T is Tail of a list
        addlist(expr E1, expr E2) ;                 % E1 ++ E2, list concatenation
        genlist(expr E1, expr E2) ;                 % E1 .. E2, generate a list from E1 to E2
        univ(expr E1, expr E2) ;                    % E1 =.. E2 -- Shuld be moved to statement
        negate(expr E) ;                            % -E
        exprcall1(token N, expr A) ;                % call(N, A)
        exprcall2(expr Z).                          % call(Z) -- should be removed


     pstatement =
        line(string File, unsigned LineNum, statement) ;
        block(pstatement*) .


predicates
    main: (string Filename, lexer::tokenlist*, pstatement*) determ (i,i,o).

end class parser

