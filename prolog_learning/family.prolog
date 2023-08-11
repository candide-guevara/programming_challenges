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

human(X) :- male(X) ; female(X).
mother(X,Y) :- female(X), parent(X,Y).
father(X,Y) :-   male(X), parent(X,Y).
all_sibling(X,Y) :- human(X), sibling(X,Y).
sibling(X,Y) :- parent(Z,X), parent(Z,Y), X \= Y, !.
grandparent(X,Y) :- parent(X,Z), parent(Z,Y).
grandmother(X,Y) :- female(X), grandparent(X,Y).
grandfather(X,Y) :-   male(X), grandparent(X,Y).
cousin(X,Y) :- grandparent(Z,X), grandparent(Z,Y), \+sibling(X,Y).
aunt(X,Y)  :- female(X), parent(Z,Y), sibling(X,Z).
uncle(X,Y) :-   male(X), parent(Z,Y), sibling(X,Z).
