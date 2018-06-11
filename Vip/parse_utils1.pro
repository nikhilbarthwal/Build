% --------------- Implementation process: Convert token list to Structure ---------------

implement parse_utils
    open core, process

class predicates

    
clauses

    match_keyword(Z, S) :-  Z = keyword(S, _, _, _).

    match_identifier(Z, S) :-  Z = identifier(S, _, _, _).

    get_position(Z, F, L, P) :- Z = keyword(_, F, L, P), !.
    get_position(Z, F, L, P) :- Z = identifier(_, F, L, P), !.
    get_position(Z, F, L, P) :- Z = str(_, F, L, P), !.
    get_position(Z, F, L, P) :- Z = num(_, F, L, P), !.
    get_position(Z, F, L, P) :- Z = symbols(_, F, L, P), !.
    get_position(Z, F, L, P) :- Z = list(_, F, L, P), !.
    get_position(Z, F, L, P) :- Z = datalist(_, F, L, P), !.

end implement parse_utils


