:- dynamic cell_value/3.

width_root(3).
width(X) :- width_root(R), X is R*R.
default_symbol(0).

% Comparison operators cannot generate solutions.
%range(X) :- width(W), X >= 0, X < W.
range(X) :- width(W), U is W-1, between(0,U,X).
cell_indexes(L) :- setof(X, range(X), L).

cell(X,Y) :- range(X), range(Y), !.

cell_value_with_def(R,C,V) :- cell_value(R,C,V), !.
cell_value_with_def(_,_,V) :- default_symbol(V).

block_value(R,C,V) :- width_root(W),
                      divmod(R,W,Qr,_),
                      divmod(C,W,Qc,_),
                      range(R1),
                      divmod(R1,W,Qr,_),
                      range(C1),
                      divmod(C1,W,Qc,_),
                      cell_value(R1,C1,V).

start_cell(cell(0,0)).
last_cell(X) :- width(W), X = cell(W,W).

solve_sudoku :- retractall(cell_value(_,_,_)),
                forall(init_table(R,C,V), assertz(cell_value(R,C,V))),
                print_table,
                start_cell(Cell),
                search(Cell),
                print_table.

% uses search framework defined in https://www.cpp.edu/~jrfisher/www/prolog_tutorial/2_16.html
search(Cell) :- last_cell(Cell).
search(Cell) :- Cell, has_value(Cell),
                next_cell(Cell,NxtCell),
                search(NxtCell).
search(Cell) :- Cell, is_blank(Cell),
                %write("search "), write(Cell), nl,
                next_state(Cell),
                cell_valid(Cell),
                next_cell(Cell,NxtCell),
                search(NxtCell).

next_state(Cell) :- cell(R,C) = Cell,
                    width(W), between(1,W,Symb),
                    retractall(cell_value(R,C,_)),
                    %write(Cell), write(" -> "), write(Symb), nl,
                    assertz(cell_value(R,C,Symb)).
next_state(Cell) :- cell(R,C) = Cell,
                    %write("backtracking "), write(Cell), nl,
                    retractall(cell_value(R,C,_)),
                    fail.

next_cell(cell(R,C), X) :- C2 is C+1,
                           X = cell(R,C2), X, !.
next_cell(cell(R,_), X) :- R2 is R+1,
                           X = cell(R2,0), X, !.
next_cell(_, X) :- last_cell(X).

cell_valid(Cell) :- cell(R,C) = Cell,
                    findall(V, cell_value(R,_,V), RowVals),
                    sort(RowVals, SetRowVals),
                    length(RowVals, SizeRowVals), length(SetRowVals, SizeRowVals),
                    findall(V, cell_value(_,C,V), ColVals),
                    sort(ColVals, SetColVals),
                    length(ColVals, SizeColVals), length(SetColVals, SizeColVals),
                    findall(V, block_value(R,C,V), BlkVals),
                    sort(BlkVals, SetBlkVals),
                    length(BlkVals, SizeBlkVals), length(SetBlkVals, SizeBlkVals).

is_blank(Cell) :- cell(R,C) = Cell,
                  cell_value_with_def(R,C,V),
                  default_symbol(V).

has_value(Cell) :- cell(R,C) = Cell,
                   cell_value_with_def(R,C,V),
                   default_symbol(D), D \= V.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Print table %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

to_row(R,Rs) :- cell_indexes(Idxs),
                to_row_helper(R,Idxs,[],Rr),
                reverse(Rr,Rs).
to_row_helper(_,[],Rs,Rs) :- !.
to_row_helper(R,[H|Idxs],Acc,Rs) :- cell_value_with_def(R,H,V),
                                    to_row_helper(R,Idxs,[V|Acc],Rs).

to_table(T) :- cell_indexes(Idxs),
               to_table_helper(Idxs,[],Tr),
               reverse(Tr,T).
to_table_helper([],T,T) :- !.
to_table_helper([H|Idxs],Acc,T) :- to_row(H,Rs),
                                   to_table_helper(Idxs,[Rs|Acc],T).

print_table :- to_table(T), print_table(T).
print_table([]) :- !, nl.
print_table([R|T]) :- write(R), nl, print_table(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Initial table %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_table(0,0,5).
init_table(0,1,3).
init_table(0,4,7).
init_table(1,0,6).
init_table(1,3,1).
init_table(1,4,9).
init_table(1,5,5).
init_table(2,1,9).
init_table(2,2,8).
init_table(2,7,6).
init_table(3,0,8).
init_table(3,4,6).
init_table(3,8,3).
init_table(4,0,4).
init_table(4,3,8).
init_table(4,5,3).
init_table(4,8,1).
init_table(5,0,7).
init_table(5,4,2).
init_table(5,8,6).
init_table(6,1,6).
init_table(6,6,2).
init_table(6,7,8).
init_table(7,3,4).
init_table(7,4,1).
init_table(7,5,9).
init_table(7,8,5).
init_table(8,4,8).
init_table(8,7,7).
init_table(8,8,9).

