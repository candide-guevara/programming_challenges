%program(Si, Ti, To, Mv, So).
start_state(go_no_carry).
final_state(stop).
zero_val(0).

tape(_,_,_).
empty_tape(tape([], Z, [])) :- zero_val(Z).
tape_in_add_3_3(tape([], 1, [1,0,0, 1,1,0,0, stop])).
tape_in_add_1_f(tape([], 1, [0,0,0, 1,1,1,1, stop])).
tape_in_add_9_6(tape([], 1, [0,0,1, 0,1,1,0, stop])).
tape_in_add_6_9(tape([], 0, [1,1,0, 1,0,0,1, stop])).
tape_in_add_f_f(tape([], 1, [1,1,1, 1,1,1,1, stop])).
tape_in_add_0_0(tape([], 0, [0,0,0, 0,0,0,0, stop])).

read_tape(tape(_,X,_), X).
write_tape(Di, tape(L,_,R), tape(L,Di,R)).
head_right(tape(L,M,[R|T]), tape(Lo,R,T)) :- push_back(L, M, Lo).
head_right(tape(L,M,[]), tape(Lo,X,[])) :- push_back(L, M, Lo), zero_val(X).
head_left(tape(Li,M,R), tape(Lo,X,[M|R])) :- pop_back(Li, X, Lo).
head_left(tape([],M,R), tape([],X,[M|R])) :- zero_val(X).

push_back([], I, [I]).
push_back([L|T], I, [L|O]) :- push_back(T, I, O).
pop_back(Li, I, Lo) :- reverse(Li, [I|Lt]), reverse(Lt, Lo).

move_head(right, Ti, To) :- head_right(Ti, To).
move_head(left, Ti, To) :- head_left(Ti, To).
move_head(center, Ti, Ti).

run_turing(Ti, To) :- start_state(Si),
                      run_helper(Si, Ti, To).

run_helper(Si, Tf, Tf) :- final_state(Si).
run_helper(Si, Ti, Tf) :- read_tape(Ti, Di),
                          add_four_bits(Si, Di, Do, Mv, So),
                          write_tape(Do, Ti, Tt),
                          write(Si), write("  "), write(Ti), write(" -> "), write(So), write("  "), write(Tt), nl,
                          move_head(Mv, Tt, To),
                          run_helper(So, To, Tf).

% Input  : tape([], op1_first_bit, [op1_rest, op2, stop])
% Output : tape([op1, result], stop, [])
add_four_bits(_, stop, stop, center, stop).
add_four_bits(go_no_carry, 0, 0, right, adv_3_op1_zero).
add_four_bits(go_no_carry, 1, 1, right, adv_3_op1_one).
add_four_bits(go_carry, 0, 0, right, adv_3_op1_one).
add_four_bits(go_carry, 1, 1, right, adv_3_op1_two).

add_four_bits(adv_3_op1_zero, X, X, right, adv_2_op1_zero).
add_four_bits(adv_2_op1_zero, X, X, right, adv_1_op1_zero).
add_four_bits(adv_1_op1_zero, X, X, right, add_bit_op1_zero).
add_four_bits(adv_3_op1_one, X, X, right, adv_2_op1_one).
add_four_bits(adv_2_op1_one, X, X, right, adv_1_op1_one).
add_four_bits(adv_1_op1_one, X, X, right, add_bit_op1_one).
add_four_bits(adv_3_op1_two, X, X, right, adv_2_op1_two).
add_four_bits(adv_2_op1_two, X, X, right, adv_1_op1_two).
add_four_bits(adv_1_op1_two, X, X, right, add_bit_op1_two).

add_four_bits(add_bit_op1_zero, 0, 0, left, bck_2_no_carry).
add_four_bits(add_bit_op1_zero, 1, 1, left, bck_2_no_carry).
add_four_bits(add_bit_op1_one, 0, 1, left, bck_2_no_carry).
add_four_bits(add_bit_op1_one, 1, 0, left, bck_2_carry).
add_four_bits(add_bit_op1_two, 0, 0, left, bck_2_carry).
add_four_bits(add_bit_op1_two, 1, 1, left, bck_2_carry).

add_four_bits(bck_2_no_carry, X, X, left, bck_1_no_carry).
add_four_bits(bck_1_no_carry, X, X, left, go_no_carry).
add_four_bits(bck_2_carry, X, X, left, bck_1_carry).
add_four_bits(bck_1_carry, X, X, left, go_carry).

