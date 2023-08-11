change(0, H,Q,D,N,P) :- set_to_zero([H,Q,D,N,P]).
change(X, H,Q,D,N,P) :- X >= 50, Y=X-50, H is 1, change(Y, H,Q,D,N,P), !.
change(X, H,Q,D,N,P) :- X >= 25, Y=X-25, Q is 1, change(Y, H,Q,D,N,P), !.
change(X, H,Q,D,N,P) :- X >= 20, Y=X-20, D is 2, change(Y, H,Q,D,N,P), !.
change(X, H,Q,D,N,P) :- X >= 10, Y=X-10, D is 1, change(Y, H,Q,D,N,P), !.
change(X, H,Q,D,N,P) :- X >= 5 , Y=X-5 , N is 1, change(Y, H,Q,D,N,P), !.
change(X, H,Q,D,N,P) :- X <  5 , P is X, change(0, H,Q,D,N,P), !.

set_to_zero([]).
set_to_zero([X|T]) :- ground(X), set_to_zero(T), !.
set_to_zero([X|T]) :- X is 0, set_to_zero(T).

