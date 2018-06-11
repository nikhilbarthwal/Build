% --------------- Implementation parse_utils: Parse Utilities ---------------

implement parse_utils
    open core, lexer, errors

clauses

    parse_identifier0(L0, L1, R) :-
        L0 = [R|L1],
        R = identifier0(_, _,_, _).

    parse_identifier1(L0, L1, R) :-
        L0 = [R|L1],
        R = identifier1(_, _,_, _).

    parse_str(L0, L1, R) :-
        L0 = [R|L1],
        R = str(_, _,_, _).

    match_identifier0(L0, L1, S) :-
        L0 = [R|L1],
        R = identifier0(S, _, _, _).

    match_identifier1(L0, L1, S) :-
        L0 = [R|L1],
        R = identifier1(S, _, _, _).

    match_symbols(L0, L1, S) :-
        L0 = [R|L1],
        R = symbols(S, _, _, _).

    match_white(L0, L1) :-
        L0 = [R|L1],
        R = white(_, _, _).

    match_open1(L0, L1) :-
        L0 = [R|L1],
        R = open1(_, _, _).

    match_open2(L0, L1) :-
        L0 = [R|L1],
        R = open2(_, _, _).

    match_open3(L0, L1) :-
        L0 = [R|L1],
        R = open3(_, _, _).

    match_close1(L0, L1) :-
        L0 = [R|L1],
        R = close1(_, _, _).

    match_close2(L0, L1) :-
        L0 = [R|L1],
        R = close2(_, _, _).

    match_close3(L0, L1) :-
        L0 = [R|L1],
        R = close3(_, _, _).

    match_endline(L0, L1) :-
        L0 = [R|L1],
        R = endline(_, _, _).

    trim_white(L0, L1) :- L0 = [R|T],  if (R = white(_, _, _)) then L1=T else L1=L0 end if.

    position([H|_], F, N, P) :- H=identifier0(_,F,N,P), !.
    position([H|_], F, N, P) :- H=identifier1(_,F,N,P), !.
    position([H|_], F, N, P) :- H=str(_,F,N,P), !.
    position([H|_], F, N, P) :- H=num(_,F,N,P), !.
    position([H|_], F, N, P) :- H=white(F,N,P), !.
    position([H|_], F, N, P) :- H=symbols(_,F,N,P), !.
    position([H|_], F, N, P) :- H=open1(F,N,P), !.
    position([H|_], F, N, P) :- H=open2(F,N,P), !.
    position([H|_], F, N, P) :- H=open3(F,N,P), !.
    position([H|_], F, N, P) :- H=close1(F,N,P), !.
    position([H|_], F, N, P) :- H=close2(F,N,P), !.
    position([H|_], F, N, P) :- H=close3(F,N,P), !.
    position([H|_], F, N, P) :- H=endline(F,N,P), !.

    skip(Input) = Output :-
        if (Input=[H|T]) then
            if (H=endline(_,_,_)) then Output=T else Output=skip(T) end if
        else
            Output=[]
        end if.

    parse_error1(L0, M, L1) :-
        L1=skip(L0),
        position(L0, F, _N, _P),
        error1(M, F).

    parse_error2(L0, M, L1) :-
        L1=skip(L0),
        position(L0, F, N, _P),
        error2(M, F, N).

    parse_error3(L0, M, L1) :-
        L1=skip(L0),
        position(L0, F, N, P),
        error3(M, F, N, P).

end implement parse_utils


