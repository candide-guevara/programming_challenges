house(H) :- member(H, [1, 2, 3, 4, 5]).
human(O) :- member(O, [english, japanese, norvegian, spaniard, ukrainian]).
pet(P)   :- member(P, [dog, fox, horse, snails, zebra]).
drink(D) :- member(D, [coffee, milk, orange, tea, water]).
smoke(S) :- member(S, [chester, kools, lucky, oldgold, parliaments]).
color(C) :- member(C, [blue, green, ivory, red, yellow]).

colored(H,C)   :- house(H), color(C).
rightto(H1,H2) :- house(H1), house(H2), 1 is (H2 - H1).
nextto(H1,H2)  :- rightto(H1, H2).
nextto(H1,H2)  :- rightto(H2, H1).

fullset(H,O,P,D,S,C) :- house(H), human(O), pet(P), drink(D), smoke(S), color(C).

unique_house(F1,F2,F3,F4,F5) :-
                                F1 = [H1,_,_,_,_,_],
                                F2 = [H2,_,_,_,_,_],
                                F3 = [H3,_,_,_,_,_],
                                F4 = [H4,_,_,_,_,_],
                                F5 = [H5,_,_,_,_,_],
                                permutation([H1,H2,H3,H4,H5], [1,2,3,4,5]).
unique_owner(F1,F2,F3,F4,F5) :-
                                F1 = [_,O1,_,_,_,_],
                                F2 = [_,O2,_,_,_,_],
                                F3 = [_,O3,_,_,_,_],
                                F4 = [_,O4,_,_,_,_],
                                F5 = [_,O5,_,_,_,_],
                                permutation([O1,O2,O3,O4,O5], [english, japanese, norvegian, spaniard, ukrainian]).
unique_pet(F1,F2,F3,F4,F5) :-
                                F1 = [_,_,P1,_,_,_],
                                F2 = [_,_,P2,_,_,_],
                                F3 = [_,_,P3,_,_,_],
                                F4 = [_,_,P4,_,_,_],
                                F5 = [_,_,P5,_,_,_],
                                permutation([P1,P2,P3,P4,P5], [dog, fox, horse, snails, zebra]).
unique_drink(F1,F2,F3,F4,F5) :-
                                F1 = [_,_,_,D1,_,_],
                                F2 = [_,_,_,D2,_,_],
                                F3 = [_,_,_,D3,_,_],
                                F4 = [_,_,_,D4,_,_],
                                F5 = [_,_,_,D5,_,_],
                                permutation([D1,D2,D3,D4,D5], [coffee, milk, orange, tea, water]).
unique_smoke(F1,F2,F3,F4,F5) :-
                                F1 = [_,_,_,_,S1,_],
                                F2 = [_,_,_,_,S2,_],
                                F3 = [_,_,_,_,S3,_],
                                F4 = [_,_,_,_,S4,_],
                                F5 = [_,_,_,_,S5,_],
                                permutation([S1,S2,S3,S4,S5], [chester, kools, lucky, oldgold, parliaments]).
unique_color(F1,F2,F3,F4,F5) :-
                                F1 = [_,_,_,_,_,C1],
                                F2 = [_,_,_,_,_,C2],
                                F3 = [_,_,_,_,_,C3],
                                F4 = [_,_,_,_,_,C4],
                                F5 = [_,_,_,_,_,C5],
                                permutation([C1,C2,C3,C4,C5], [blue, green, ivory, red, yellow]).

generator(F1,F2,F3,F4,F5) :- unique_house(F1,F2,F3,F4,F5),
                             unique_owner(F1,F2,F3,F4,F5),
                             unique_pet(F1,F2,F3,F4,F5),
                             unique_drink(F1,F2,F3,F4,F5),
                             unique_smoke(F1,F2,F3,F4,F5),
                             unique_color(F1,F2,F3,F4,F5).

unique_pair(H1,O1,P1,D1,S1,C1,
            H2,O2,P2,D2,S2,C2) :- fullset(H1,O1,P1,D1,S1,C1),
                                  fullset(H2,O2,P2,D2,S2,C2),
                                  H1 \= H2,
                                  O1 \= O2,
                                  P1 \= P2,
                                  D1 \= D2,
                                  S1 \= S2,
                                  C1 \= C2.

unpack1(Func, F) :- F = [H,O,P,D,S,C], apply(Func, F).
unpack2(Func, F1, F2) :- F1 = [H1,O1,P1,D1,S1,C1],
                         F2 = [H2,O2,P2,D2,S2,C2],
                         append(F1,F2,Fs),
                         apply(Func, Fs).

% The green house is immediately to the right of the ivory house.
ivory_green(H1,O1,P1,D1,S1,C1,
            H2,O2,P2,D2,S2,C2) :- rightto(H1,H2),
                                  C1=ivory,
                                  C2=green,
                                  unique_pair(H1,O1,P1,D1,S1,C1,H2,O2,P2,D2,S2,C2).
% The Englishman lives in the red house.
english_red(H,O,P,D,S,C) :- O=english, C=red,
                            fullset(H,O,P,D,S,C).
% The Norwegian lives in the first house.
norv_first(H,O,P,D,S,C) :- H=1, O=norvegian,
                           fullset(H,O,P,D,S,C).
% The Norwegian lives next to the blue house.
norv_next_blue(H1,O1,P1,D1,S1,C1,
               H2,O2,P2,D2,S2,C2) :- nextto(H1,H2),
                                     O1=norvegian,
                                     C2=blue,
                                     unique_pair(H1,O1,P1,D1,S1,C1,H2,O2,P2,D2,S2,C2).
% The Spaniard owns the dog.
spaniard_dog(H,O,P,D,S,C) :- O=spaniard, P=dog,
                             fullset(H,O,P,D,S,C).
% Coffee is drunk in the green house.
coffee_green(H,O,P,D,S,C) :- D=coffee, C=green,
                             fullset(H,O,P,D,S,C).
% The Ukrainian drinks tea.
ukr_tea(H,O,P,D,S,C) :- O=ukrainian, D=tea,
                        fullset(H,O,P,D,S,C).
% Milk is drunk in the middle house.
milk_middle(H,O,P,D,S,C) :- H=3, D=milk,
                            fullset(H,O,P,D,S,C).
% The Old Gold smoker owns snails.
oldg_snail(H,O,P,D,S,C) :- P=snails, S=oldgold,
                           fullset(H,O,P,D,S,C).
% Kools are smoked in the yellow house.
kool_yellow(H,O,P,D,S,C) :- S=kools, C=yellow,
                            fullset(H,O,P,D,S,C).
% The man who smokes Chesterfields lives in the house next to the man with the fox.
chester_fox(H1,O1,P1,D1,S1,C1,
            H2,O2,P2,D2,S2,C2) :- nextto(H1,H2),
                                  S1=chester,
                                  P2=fox,
                                  unique_pair(H1,O1,P1,D1,S1,C1,H2,O2,P2,D2,S2,C2).
% Kools are smoked in the house next to the house where the horse is kept.
kool_horse(H1,O1,P1,D1,S1,C1,
           H2,O2,P2,D2,S2,C2) :- nextto(H1,H2),
                                 S1=kools,
                                 P2=horse,
                                 unique_pair(H1,O1,P1,D1,S1,C1,H2,O2,P2,D2,S2,C2).
% The Lucky Strike smoker drinks orange juice.
lucky_orange(H,O,P,D,S,C) :- D=orange, S=lucky,
                             fullset(H,O,P,D,S,C).
% The Japanese smokes Parliaments.
jap_parlia(H,O,P,D,S,C) :- O=japanese, S=parliaments,
                           fullset(H,O,P,D,S,C).


drink_sol(F1,F2,F3,F4,F5) :- unpack1(ukr_tea,      F1),
                             unpack1(milk_middle,  F2),
                             unpack1(lucky_orange, F3),
                             unpack1(coffee_green, F4),
                             unpack1(fullset,      F5).

human_sol(F1,F2,F3,F4,F5) :- unpack1(norv_first,   F1),
                             unpack1(english_red,  F2),
                             unpack1(spaniard_dog, F3),
                             unpack1(ukr_tea,      F4),
                             unpack1(jap_parlia,   F5).

smoke_sol(F1,F2,F3,F4,F5) :- unpack1(oldg_snail,   F1),
                             unpack1(kool_yellow,  F2),
                             unpack1(lucky_orange, F3),
                             unpack1(jap_parlia,   F4),
                             member(Fx, [F1,F2,F3,F4]),
                             unpack2(chester_fox,  Fx,F5).

color_sol(F1,F2,F3,F4,F5) :- unpack2(ivory_green, F1,F2),
                             unpack1(coffee_green,F2),
                             unpack1(english_red, F3),
                             unpack1(kool_yellow, F4),
                             member(Fx, [F1,F2,F3,F4]),
                             unpack2(norv_next_blue, Fx,F5).

pet_sol(F1,F2,F3,F4,F5) :- unpack1(spaniard_dog, F1),
                           member(Fx, [F1,F3,F4,F5]),
                           unpack2(oldg_snail, Fx,F2),
                           member(Fy, [F1,F2,F4,F5]),
                           unpack2(chester_fox, Fy,F3),
                           member(Fz, [F1,F2,F3,F5]),
                           unpack2(kool_horse, Fz,F4),
                           unpack1(fullset, F5).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% GPT SOLUTION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% PROMPT:
% Do you know about the zebra problem? Can you give the solution?
% Can you write a prolog program to solve the zebra problem?

solve(ZebraOwner, WaterDrinker) :-
    Houses = [house(1, _, _, _, _, _),
              house(2, _, _, _, _, _),
              house(3, _, _, _, _, _),
              house(4, _, _, _, _, _),
              house(5, _, _, _, _, _)],

    % Clues from the problem
    member(house(_, red, british, _, _, _), Houses),   % (1) Brit in the red house
    member(house(_, _, swedish, _, _, dogs), Houses),  % (2) Swede keeps dogs
    member(house(_, _, danish, tea, _, _), Houses),    % (3) Dane drinks tea
    right_of(house(_, green, _, _, _, _), house(_, white, _, _, _, _), Houses), % (4) Green is left of white
    member(house(_, green, _, coffee, _, _), Houses),  % (5) Green house's owner drinks coffee
    member(house(_, _, _, _, pall_mall, birds), Houses), % (6) Pall Mall smoker keeps birds
    member(house(_, yellow, _, _, dunhill, _), Houses), % (7) Yellow house smokes Dunhill
    Houses = [_, _, house(3, _, _, milk, _, _), _, _], % (8) Center house drinks milk
    Houses = [house(1, _, norwegian, _, _, _)|_],     % (9) Norwegian in first house
    next_to(house(_, _, _, _, blends, _), house(_, _, _, _, _, cats), Houses), % (10) Blends smoker next to cat owner
    next_to(house(_, _, _, _, dunhill, _), house(_, _, _, _, _, horses), Houses), % (11) Dunhill smoker next to horse owner
    member(house(_, _, _, beer, blue_master, _), Houses), % (12) Blue Master smoker drinks beer
    member(house(_, _, german, _, prince, _), Houses),    % (13) German smokes Prince
    next_to(house(_, _, norwegian, _, _, _), house(_, blue, _, _, _, _), Houses), % (14) Norwegian next to blue house
    next_to(house(_, _, _, _, blends, _), house(_, _, _, water, _, _), Houses),   % (15) Blends smoker has a neighbor who drinks water
    
    % Extract answers
    member(house(_, _, ZebraOwner, _, _, zebra), Houses),
    member(house(_, _, WaterDrinker, water, _, _), Houses).

% Helper predicates
to_right(A, B, [A, B | _]).
to_right(A, B, [_ | Rest]) :- to_right(A, B, Rest).

right_of(A, B, Houses) :- to_right(B, A, Houses).
next_to(A, B, Houses) :- to_right(A, B, Houses); to_right(B, A, Houses).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MAIN %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- initialization((
                   solve(ZebraOwner, WaterDrinker),
                   format("ZebraOwner=~w\n", ZebraOwner),
									 format("WaterDrinker=~w\n", WaterDrinker),
                   halt(0)
                 )).

