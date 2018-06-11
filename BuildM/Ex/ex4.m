:- module ex4.

:- interface.

:- import_module string.

:- pred text(string::out) is multi.

:- implementation.

:- import_module string.

text("A").
text("B").
text("C").
text("D").
text("E").
text("F").


