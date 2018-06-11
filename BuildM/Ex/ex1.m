:- module ex1.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module ex4.

:- pred test(io::di, io::uo) is det.

:- type playing_card ---> card(rank, suit) ; joker.

:- type rank ---> ace ; two ; three ; four ; five ; six ; seven ; eight ; nine ; ten ; jack ; queen ; king.

:- type suit ---> clubs ; diamonds ; hearts ; spades.




test(X,X).


main(IOState_in, IOState_out) :-
    ex4__text(X),
    io.write_string(X, IOState_in, IOState_tmp),
    test(IOState_tmp, IOState_out). 

% mmc --target c --high-level-code --high-level-data ex1.m

