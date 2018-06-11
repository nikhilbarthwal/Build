% --------------- Implementation process: Convert token list to Structure ---------------

implement parse_project
    open core, errors, process, parse_utils

class facts
    filename:string := "".
    feature_files:string* =[].
    poject_files:string* =[].

class predicates
    parse_project: (atom, project) determ (i,o)
    verify_namespace: (atom*) determ (i)
    read_projectvars: (atom*, project_pairs*) determ (i,o)

clauses

    main(F, X) = Z :-
        filename = F,
        if (parse_project(X, Y)) then Z=X else Z=dummy end if.

    parse_project(X, Y) :-
        !, X = list(L),
        N = list::length(L),
        N > 4, N < 7,
        L = [H1, H2, H3, H4, H5 | T],
        
        if not(match_keyword(H1, "project")) then
            get_position(H1, F1, L1, P1),
            error3("Expected \"project\" here", F1, L1, P1)
        else if,


        if (not(match_identifier(H2, _))) then
            get_position(H2, F2, L2, P2),
            error3("Expected folder name here", F2, L2, P2)
        else if,

        if (H3 = list(X3, F3, L3, P3)) then
            read_projectvars(X3, Vars)
        else
            Vars = [],
            error3("Expected Project variable list here", F3, L3, P3)
        end if,

        if (H4 = list(X4, F4, L4, P4)) then
            if (verify_namespace(X4)) then
                NameSpace = X4
            else
                NameSpace = []
            else if
        else
            NameSpace = [],
            error3("Expected NameSpace quialifier here", F4, L4, P4)
        end if,

        if (H5 = list(X5, F5, L5, P5)) then
            read_statements(X5, Statements)
        else
            Statements = [],
            error3("Expected Statements list here", F5, L5, P5)
        end if,

        Y=proj(F2, Vars, NameSpace, Statements, []).



    read_projectvars([], []) :- !.

    read_projectvars([H|T], L) :- 
        H = list(H1, H2),

        if not(match_identifier(H1, _)) then
            get_position(H1, F1, L1, P1), 
            error3("Expecting Variable Name here", F, L, P)
        end if,

        if not(match_str(H2, _)) then
            get_position(H2, F2, L2, P2), 
            error3("Expecting String value here", F2, L2, P2)
        end if,

        HH = pair(H1, H2),
        read_projectvars(T, TT),
        L = [HH|TT].

    read_projectvars([H|T], L) :- 
        get_position(H, F, L, P),
        error3("Expecting Variable Name/Value Pair", F, L, P),
        read_projectvars(T, L).


    verify_namespace([]) :- !.
    verify_namespace([H|T]) :- match_identifier(H, _), verify_namespace(T).
    verify_namespace([H|_]) :- get_position(H, F, L, P), error3("Expecting NameSpace qualifier here", F, L, P), !, fail.


    read_statements([], []) :- !.
    read_statements([H1|T1], [H2|T2]) :- read_statement(H1, H2), read_statements(T1, T2).
    read_statements([H|T], L) :- get_position(H, F, L, P), error3("Unable to parse the statement", F, L, P), read_statements(T, L).


    read_statements([], []) :- !.
    read_statements([H1|T1], [H2|T2]) :- read_statement(H1, H2), read_statements(T1, T2).
    read_statements([H|T], L) :- get_position(H, F, L, P), error3("Unable to parse the statement", F, L, P), read_statements(T, L).



end implement parse_project


