pair(_,_).
maybe(X) :- just(X).
maybe(X) :- X = niente.
just(_).

double(X,R) :- R is 2*X.

myfunctor(F,just(V),R) :- call(F,V,R).
myfunctor(_,niente,R) :- R = niente.

is_same(pair(X,Y)) :- X == Y.

