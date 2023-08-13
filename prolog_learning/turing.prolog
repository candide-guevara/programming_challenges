start_state(start).
final_state(stop).
zero_val(0).

tape(_,_,_).
empty_tape(tape([], Z, [])) :- zero_val(Z).

read_tape(tape(_,X,_), X).
write_tape(Di, tape(L,_,R), tape(L,Di,R)).

head_right(tape(L,M,[X|T]), tape(Lo,X,T)) :- append(L, [M], Lo).
head_right(tape(L,M,[]), tape(Lo,X,[])) :- append(L, [M], Lo), zero_val(X).

head_left(tape(Li,M,R), tape(Lo,X,[M|R])) :- append(Lo, [X], Li).
head_left(tape([],M,R), tape([],X,[M|R])) :- zero_val(X).

move_head(right, Ti, To) :- head_right(Ti, To).
move_head(left, Ti, To) :- head_left(Ti, To).
move_head(center, T, T).

run_turing(Ti, Tr, To) :- start_state(Si),
                          run_helper(Si, Ti, Tr, To).

print_transition(Tr, Si, Di) :- call(Tr, Si, Di, Do, Mv, So),
                                format('(~w, ~w) -> (~w, ~w, ~w)\n', [Si, Di, Do, Mv, So]).

run_helper(Si, T, _, T) :- final_state(Si), !.
run_helper(Si, Ti, Tr, Tf) :- read_tape(Ti, Di),
                              call(Tr, Si, Di, Do, Mv, So),
                              print_transition(Tr, Si, Di),
                              write_tape(Do, Ti, Tt),
                              move_head(Mv, Tt, To),
                              run_helper(So, To, Tr, Tf).

prog_add_one_bit(start, X, X, left, read_first).
prog_add_one_bit(read_first, 1, 1, right, right_one).
prog_add_one_bit(read_first, 0, 0, right, right_zero).
prog_add_one_bit(right_zero, 0, 0, right, result_nul).
prog_add_one_bit(right_zero, 1, 1, right, result_one).
prog_add_one_bit(right_one,  0, 0, right, result_one).
prog_add_one_bit(right_one,  1, 1, right, result_two).
prog_add_one_bit(result_nul, _, 0, center, stop).
prog_add_one_bit(result_one, _, 1, center, stop).
prog_add_one_bit(result_two, _, 0, right,  write_carry).
prog_add_one_bit(write_carry, _, 1, center, stop).

