f(X) :- X > 0.
g(X) :- f(X), write(X), nl.


go :- clause(g(5),X), X, nl.
 