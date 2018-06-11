% ***** implementation parse_project : Convert Token Stream into Project Item *****

implement parse_project
    open core, lexer, parse_utils, errors

class facts
    eof:unsigned :=0.

class predicates
    parse_project: (token* Input, token* Output, project Result) determ (i,o,o).
    parse_project_pairs: (token* Input, token* Output, p_pair* Result) determ (i,o,o).
    parse_p_pair: (token* Input, token* Output, p_pair Result) determ (i,o,o).
    parse_pvar_name: (token* Input, token* Output, token Result) determ (i,o,o).
    parse_pvar_value: (token* Input, token* Output, token Result) determ (i,o,o).
    parse_statementlist: (token* Input, token* Output, p_statement* Result) determ (i,o,o).
    parse_build_vars: (token* Input, token* Output, token* Result) determ (i,o,o).
    parse_b_value: (token* Input, token* Output, token Result) determ (i,o,o).
    parse_namespace: (token* Input, token* Output, token* Result) determ (i,o,o).
    parse_grouplist: (token* Input, token* Output, p_group* Result) determ (i,o,o).
    parse_group: (token* Input, token* Output, p_group Result) determ (i,o,o).
    parse_argslist: (token* Input, token* Output, token* Result) determ (i,o,o).
    parse_statement: (token* Input, token* Output, p_statement Result) determ (i,o,o).

clauses
    parse(F,L) = Z :-
        if (parse_project(L, _, R)) then
            Z=R
        elseif (eof=1) then
            error1(F,"Unexpected end of file"),
            Z=dummy
        else
            Z=dummy
        end if.

    parse_project(L0, L, R):-
stdio::write(" Error1 :project"), stdio::nl,        if (L0=[]) then
            eof := 1, fail
        else
        parse_error3(L0, "Unable to understand the project", L), 
            R = dummy
        end if, !.


    parse_project_pairs(L0, L, R):-
stdio::write(" Start :project_pairs"), stdio::nl,
        if (L0=[]) then
            eof := 1, fail
        else
            parse_p_pair(L0, L1, H),
            trim_white(L1, L2),
            match_symbols(L2, L3, ","),
            parse_project_pairs(L3, L4, T),
            L = L4,
stdio::write(" End :project_pairs : [H|T]"), stdio::nl,
            R = [H|T]
        end if, !.

    parse_project_pairs(L0, L, R):-
stdio::write(" Start :project_pairs"), stdio::nl,
        if (L0=[]) then
            eof := 1, fail
        else
            parse_p_pair(L0, L1, H),
            L = L1,
stdio::write(" End :project_pairs : [H]"), stdio::nl,
            R = [H]
        end if, !.


    parse_p_pair(L0, L, R):-
stdio::write(" Start :p_pair"), stdio::nl,
        if (L0=[]) then
            eof := 1, fail
        else
            trim_white(L0, L1),
            parse_pvar_name(L1, L2, N),
            match_symbols(L2, L3, "="),
            parse_pvar_value(L3, L4, V),
            L = L4,
stdio::write(" End :p_pair : var(N, V)"), stdio::nl,
            R = var(N, V)
        end if, !.


    parse_pvar_name(L0, L, R):-
stdio::write(" Start :pvar_name"), stdio::nl,
        if (L0=[]) then
            eof := 1, fail
        else
            trim_white(L0, L1),
            parse_identifier1(L1, L2, X),
            L = L2,
stdio::write(" End :pvar_name : X"), stdio::nl,
            R = X
        end if, !.


    parse_pvar_value(L0, L, R):-
stdio::write(" Start :pvar_value"), stdio::nl,
        if (L0=[]) then
            eof := 1, fail
        else
            trim_white(L0, L1),
            parse_identifier0(L1, L2, X),
            L = L2,
stdio::write(" End :pvar_value : X"), stdio::nl,
            R = X
        end if, !.


    parse_statementlist(L0, L, R):-
stdio::write(" Start :statementlist"), stdio::nl,
        if (L0=[]) then
            eof := 1, fail
        else
            parse_statement(L0, L1, H),
            L = L1,
stdio::write(" End :statementlist : [H]"), stdio::nl,
            R = [H]
        end if, !.

    parse_statementlist(L0, L, R):-
stdio::write(" Start :statementlist"), stdio::nl,
        if (L0=[]) then
            eof := 1, fail
        else
            parse_statement(L0, L1, H),
            match_endline(L1, L2),
            parse_statementlist(L2, L3, T),
            L = L3,
stdio::write(" End :statementlist : [H|T]"), stdio::nl,
            R = [H|T]
        end if, !.


    parse_build_vars(L0, L, R):-
stdio::write(" Start :build_vars"), stdio::nl,
        if (L0=[]) then
            eof := 1, fail
        else
            parse_b_value(L0, L1, H),
            trim_white(L1, L2),
            match_symbols(L2, L3, ","),
            parse_build_vars(L3, L4, T),
            L = L4,
stdio::write(" End :build_vars : [H|T]"), stdio::nl,
            R = [H|T]
        end if, !.

    parse_build_vars(L0, L, R):-
stdio::write(" Start :build_vars"), stdio::nl,
        if (L0=[]) then
            eof := 1, fail
        else
            parse_b_value(L0, L1, H),
            L = L1,
stdio::write(" End :build_vars : [H]"), stdio::nl,
            R = [H]
        end if, !.


    parse_b_value(L0, L, R):-
stdio::write(" Start :b_value"), stdio::nl,
        if (L0=[]) then
            eof := 1, fail
        else
            trim_white(L0, L1),
            parse_identifier0(L1, L2, X),
            L = L2,
stdio::write(" End :b_value : X"), stdio::nl,
            R = X
        end if, !.

    parse_b_value(L0, L, R):-
stdio::write(" Start :b_value"), stdio::nl,
        if (L0=[]) then
            eof := 1, fail
        else
            trim_white(L0, L1),
            parse_identifier1(L1, L2, X),
            L = L2,
stdio::write(" End :b_value : X"), stdio::nl,
            R = X
        end if, !.


    parse_namespace(L0, L, R):-
stdio::write(" Start :namespace"), stdio::nl,
        if (L0=[]) then
            eof := 1, fail
        else
            parse_identifier1(L0, L1, H),
            match_symbols(L1, L2, "."),
            parse_namespace(L2, L3, T),
            L = L3,
stdio::write(" End :namespace : [H|T]"), stdio::nl,
            R = [H|T]
        end if, !.

    parse_namespace(L0, L, R):-
stdio::write(" Start :namespace"), stdio::nl,
        if (L0=[]) then
            eof := 1, fail
        else
            parse_identifier1(L0, L1, H),
            L = L1,
stdio::write(" End :namespace : [H]"), stdio::nl,
            R = [H]
        end if, !.


    parse_grouplist(L0, L, R):-
stdio::write(" Start :grouplist"), stdio::nl,
        if (L0=[]) then
            eof := 1, fail
        else
            parse_group(L0, L1, H),
            parse_grouplist(L1, L2, T),
            L = L2,
stdio::write(" End :grouplist : [H|T]"), stdio::nl,
            R = [H|T]
        end if, !.

    parse_grouplist(L0, L, R):-
stdio::write(" Start :grouplist"), stdio::nl,       L0 = [],
        L = [],
stdio::write(" End :grouplist : []"), stdio::nl,        R = [], !.


    parse_group(L0, L, R):-
stdio::write(" Start :group"), stdio::nl,
        if (L0=[]) then
            eof := 1, fail
        else
            parse_identifier1(L0, L1, N),
            parse_argslist(L1, L2, A),
            trim_white(L2, L3),
            match_close1(L3, L4),
            match_endline(L4, L5),
            parse_statementlist(L5, L6, S),
            match_identifier1(L6, L7, "End"),
            match_endline(L7, L8),
            L = L8,
stdio::write(" End :group : p_grp(N,A,S)"), stdio::nl,
            R = p_grp(N,A,S)
        end if, !.


    parse_argslist(L0, L, R):-
stdio::write(" Start :argslist"), stdio::nl,
        if (L0=[]) then
            eof := 1, fail
        else
            trim_white(L0, L1),
            parse_identifier1(L1, L2, H),
            trim_white(L2, L3),
            match_symbols(L3, L4, ","),
            parse_argslist(L4, L5, T),
            L = L5,
stdio::write(" End :argslist : [H|T]"), stdio::nl,
            R = [H|T]
        end if, !.

    parse_argslist(L0, L, R):-
stdio::write(" Start :argslist"), stdio::nl,
        if (L0=[]) then
            eof := 1, fail
        else
            trim_white(L0, L1),
            parse_identifier1(L1, L2, H),
            L = L2,
stdio::write(" End :argslist : [H]"), stdio::nl,
            R = [H]
        end if, !.


    parse_statement(L0, L, R):-
stdio::write(" Start :statement"), stdio::nl,
        if (L0=[]) then
            eof := 1, fail
        else
            match_identifier0(L0, L1, "feature"),
            match_white(L1, L2),
            parse_namespace(L2, L3, S),
            match_open1(L3, L4),
            parse_build_vars(L4, L5, A),
            trim_white(L5, L6),
            match_white(L6, L7),
            match_identifier0(L7, L8, "in"),
            match_white(L8, L9),
            parse_str(L9, L10, F),
            L = L10,
stdio::write(" End :statement : feature(F, S, A)"), stdio::nl,
            R = feature(F, S, A)
        end if, !.

    parse_statement(L0, L, R):-
stdio::write(" Start :statement"), stdio::nl,
        if (L0=[]) then
            eof := 1, fail
        else
            match_identifier0(L0, L1, "group"),
            match_white(L1, L2),
            parse_identifier1(L2, L3, N),
            parse_build_vars(L3, L4, A),
            trim_white(L4, L5),
            L = L5,
stdio::write(" End :statement : group(N, A)"), stdio::nl,
            R = group(N, A)
        end if, !.

    parse_statement(L0, L, R):-
stdio::write(" Start :statement"), stdio::nl,
        if (L0=[]) then
            eof := 1, fail
        else
            match_identifier0(L0, L1, "project"),
            match_white(L1, L2),
            parse_identifier1(L2, L3, N),
            parse_build_vars(L3, L4, A),
            trim_white(L4, L5),
            match_white(L5, L6),
            match_identifier0(L6, L7, "in"),
            match_white(L7, L8),
            parse_str(L8, L9, F),
            L = L9,
stdio::write(" End :statement : project(F,N, A)"), stdio::nl,
            R = project(F,N, A)
        end if, !.

    parse_statement(L0, L, R):-
stdio::write(" Error2 :statement"), stdio::nl,
        if (L0=[]) then
            eof := 1, fail
        else
        !, parse_error2(L0, "Unable to understand the statement", L), 
            R = dummy
        end if, !.



end implement parse_project
