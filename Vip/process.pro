% --------------- Implementation process: Convert token list to Structure ---------------

implement process
    open core, lexer, errors

class facts
    filename:string := "".
    %tokens:token* := [].

class predicates
    read_atom: (token* Input, token* LeftOvers, atom Result) determ (i,o,o).
    read_list: (token* Input, token* LeftOvers, atom* Result) determ (i,o,o).

clauses

    main(F, L) = P :-
        filename := F,
        if (read_atom(L, LL, Z)) then
            P = Z,
            if not(LL=[]) then error1("Excess tokens in file", F) end if
        else
            P = list([])
        end if.


    read_atom([], [], _Z) :- error1("Excess tokens in file", filename), fail.

    read_atom([H|T], T, Z) :- H = identifier0(S, L, P), !, Z = keyword(S, filename, L, P).

    read_atom([H|T], T, Z) :- H = identifier1(S, L, P), !, Z = identifier(S, filename, L, P).

    read_atom([H|T], T, Z) :- H = str(S, L, P), !, Z = str(S, filename, L, P).

    read_atom([H|T], T, Z) :- H = num(S, L, P), !, Z = num(S, filename, L, P).

    read_atom([H|T], T, Z) :- H = symbols(S, L, P), !, Z = symbols(S, filename, L, P).

    read_atom([H|T], R, Z) :- H = openp(L, P), !, read_list(T, R, X), Z = list(X, filename, L, P).

    read_atom([H|T], R, Z) :- H = opend(L, P), !, read_list(T, R, X), Z = datalist(X,  filename, L, P).

    read_atom(_, _, _) :- error1("Unexpected end of file", filename), fail.


    read_list([H|L], L, [])  :- H = close(_, _), !.

    read_list(L1, L2, [H|T]) :- read_atom(L1, L, H), read_list(L, L2, T).


end implement process


