% **********************************
%    UTILS: All common utilities
% *********************************

:- module utils.

:- interface.

:- import_module io, int, list, string. %, bool, char.

:- type counter == int. % Unsigned Number

:- type number == int.  % Signed Number

:- pred ioStateCopy(io::di, io::uo) is det.

:- type position == { string, counter, counter }. % (filename, linepos, counter)

:- type pos(T) ---> position(val::T, position).

:- type block(T) == list(pos(T)).

:- implementation.

ioStateCopy(X,X).