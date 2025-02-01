maybe(X) :- just(X).
maybe(X) :- X = niente.
just(_).

myfunctor(F,just(V),R) :- call(F,V,R).
myfunctor(_,niente,R) :- R = niente.

double(X,R) :- R is 2*X.

