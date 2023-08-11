parent(jaime, candide).
parent(jaime, eduard).
parent(esperanza, candide).
parent(esperanza, eduard).

parent(margarita, treasure).
parent(gilles, treasure).

parent(celmira, jaime).
parent(celmira, jorge).
parent(celmira, clara).
parent(margot, esperanza).
parent(francisco, esperanza).
parent(margot, margarita).
parent(francisco, margarita).

male(francisco).
male(jaime).
male(jorge).
male(gilles).
male(candide).
male(eduard).
male(treasure).

female(celmira).
female(margot).
female(margarita).
female(esperanza).
female(clara).

mother(X,Y) :- female(X), parent(X,Y).
father(X,Y) :-   male(X), parent(X,Y).
sibling(X,Y) :- parent(Z,X), parent(Z,Y), X @< Y.
%sibling_helper(X,Y) :- parent(Z,X), parent(Z,Y), X @< Y, !.
grandparent(X,Y) :- parent(X,Z), parent(Z,Y).
grandmother(X,Y) :- grandparent(X,Y), female(X).
grandfather(X,Y) :- grandparent(X,Y),   male(X).
cousin(X,Y) :- grandparent(Z,X), grandparent(Z,Y), X @< Y, not(sibling(X,Y)).
aunt(X,Y)  :- parent(Z,Y), sibling(X,Z), female(X).
uncle(X,Y) :- parent(Z,Y), sibling(X,Z),   male(X).
