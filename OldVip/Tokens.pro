% TOKEN UTILITIES

GLOBAL DOMAINS
	type = open1 ; open2 ; close1 ; close2 ; int ; str ; symbols ; identifier ; white ; comment
	token = token(string, integer, integer, type, string)
	linetokens = token*
	filetokens = linetokens*

GLOBAL PREDICATES
	
	determ last_token(linetokens, token, linetokens) - (i,o,o)
	
PREDICATES
	determ token_reverse(linetokens, linetokens) -(i,o)
	determ token_reverse0(linetokens, linetokens, linetokens) - (i,i,o)
	
	determ token_trim(linetokens, linetokens) - (i, o)

	determ white_token(token) - (i)
	determ token_data(token, type, string) - (i,i,o), (i,i,i)
	
	determ append_tokens(linetokens, linetokens, linetokens) - (i,i,o)
	
	determ token_chop(linetokens, linetokens) - (i, o), (i,i)
	
	nondeterm token_tight_partition(linetokens, type, string, linetokens, linetokens) - (i,i,i,o,o)
	nondeterm token_spaced_partition(linetokens, type, string, linetokens, linetokens) - (i,i,i,o,o)
	nondeterm token_partition(linetokens, type, string, linetokens, linetokens) - (i,i,i,o,o)
	nondeterm token_partition0(linetokens, type, string, linetokens, linetokens) - (i,i,i,o,o)
	
	nondeterm token_spacial_partition(linetokens, linetokens, linetokens) - (i,o,o)
	nondeterm token_spacial_partition0(linetokens, linetokens, linetokens) - (i,o,o)
	
CLAUSES
	
	token_reverse(L1, L2) :- token_reverse0(L1, [], L2).
	
	token_reverse0([H|T], L, Z) :- 
		token_reverse0(T, [H|L], Z).
	
	token_reverse0([], L, L).

	append_tokens([],L,L).
	append_tokens([H|T],L2,[H|L3]) :- append_tokens(T,L2,L3).
	
	last_token(L, Z, R) :-
		token_reverse(L, LL),
		LL = [Z|RR],
		token_reverse(RR, R).

	white_token(token(_, _, _, white, _)).

	token_data(token(_, _, _, T, Z), T, Z).
		
	token_chop([H|T], T) :-  white_token(H), !.
	token_chop(L, L).
	
	token_trim(L1, L2) :- !,
		token_chop(L1, L),
		token_reverse(L, R),
		token_chop(R, RR),
		token_reverse(RR, L2).
	
	token_partition0([H|T], C, Z, [], T) :- token_data(H, C, Z).
	token_partition0([H|T], C, Z, [H|L1], L2) :- token_partition0(T, C, Z, L1, L2).
	
	token_partition(L, C, Z, L1, L2) :-
		token_trim(L, LL), !,
		token_partition0(LL, C, Z, LL1, LL2),
		token_trim(LL1, L1),
		token_trim(LL2, L2).
		
	token_tight_partition(L, C, Z, L1, L2) :-
		token_trim(L, LL), !,
		token_partition0(LL, C, Z, L1, L2).
		

	token_spaced_partition(L, C, Z, L1, L2) :-
		token_trim(L, LL), !,
		token_partition0(LL, C, Z, LL1, LL2),
		last_token(LL1, W1, _),
		white_token(W1),
		LL2=[W2|_],
		white_token(W2),
		token_trim(LL1, L1),
		token_trim(LL2, L2).
	
	token_spacial_partition(L, L1, L2) :- 
		token_trim(L, LL), !,
		token_spacial_partition0(LL, L1, L2).
	
	token_spacial_partition0([H|T], [], T) :- white_token(H).
	token_spacial_partition0([H|T], [H|L1], L2) :- token_spacial_partition0(T, L1, L2).
