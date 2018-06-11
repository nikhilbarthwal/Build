:- module ex2.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.


:- pred nikhil(string::out) is multi.
%:- pred print(io::in, io::out) is failure.
:- pred print(io::in, io::uo) is failure. %det. %failure.
:- type playing_card ---> card(rank, suit) ; joker.

:- type rank ---> ace ; two ; three ; four ; five ; six ; seven ; eight ; nine ; ten ; jack ; queen ; king.

:- type suit ---> clubs ; diamonds ; hearts ; spades.


main(IOState_in, IOState_out) :- print(IOState_in, IO1),  io.write_string("Hello, World!\n", IO1, IOState_out). 
%io.write_string("    Yes\n", IO1, IOState_out) ; io.write_string("    No\n", IOState_in, IOState_out) ).


nikhil("A").
nikhil("B").
nikhil("C").
nikhil("D").
nikhil("E").
nikhil("F").

print(IOState_in, IOState_out) :- nikhil(X), io.write_string(X, IOState_in, _), fail.
%print(IOState_in, IOState_out) :- io.write_string("Hello, World!\n", IOState_in, IOState_out).
%IOState_out = IOState_in.


%io.write_string("Hello, World!\n", IOState_in, IOState_out).

% mmc --target c --high-level-code --high-level-data ex1.m

