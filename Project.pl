%GRID_BUILD/2 
  %generates a list of lists both of length N using the predifend predicate %length   and a helper to create the internal list.
grid_build(N, Xss) :- 
   length(Xss, N),
   helperbuild(Xss,N). 
helperbuild([X|L],N):-
  length(X,N),helperbuild(L,N).
helperbuild([],_).

%GRID_GEN/2 
grid_gen(N,M):-
	grid_build(N,M),
	grid_gen_row(N,M).

%helper1 gets each row and puts it in helper2.
grid_gen_row(_,[]).
grid_gen_row(N,[H|T]):-
	grid_gen_element(N,H),
	grid_gen_row(N,T).

%helper2 gets each element in the row and puts it in helper3.
grid_gen_element(N,[]).
grid_gen_element(N,[H|T]):-
	grid_gen_value(N,H),
	grid_gen_element(N,T).

%helper3 sets the value of each element any value from 1 to N.

grid_gen_value(N,X):-
	N>0,
	N1 is N-1,
	((grid_gen_value(N1,X));(X is N)).


%NUM_GEN/3 generates a list containing all values from F to L in consecetive order and placeing it in R using findall predifined predicate.

num_gen(F,L,R):-
    findall(J,(between(F,L,J)),R).

%CHECK_NUM_GRID/1 
 %it gets the max value in the whole list by flattening the nested list to a   single one then getting its max element and generating a list with all the values less than that max using num_ge, switching the list to be a set to be able to use the subset predicate which finds whether the generated list is a subset from the original.
check_num_grid(X):- 
flatten(X,R),max_list(R,Max),M1 is Max -1,num_gen(1,M1,L1),list_to_set(R,S),subset(L1,S).


%ACCEPTABLE_PERMUTATION/2 first it fills the list with lists all having the values from 1 to R then using the helper it checks that no two values in the same index have the same value.
perm([H|T],L) :- 
perm(T,P), insert(H,P,L).
perm([],[]).
insert(X,L,[X|L]).
insert(X,[H|T],[H|T1]) :- insert(X,T,T1).

acceptable_permutation(X,R):-
perm(X,R),length(X,S),help(X,R,0,S).
acceptable_permutation([],[]).
help(X1,X2,I,S):-
    I<S,nth0(I,X1, E1),nth0(I,X2, E2),E1 \= E2,I1 is I+1,help(X1,X2,I1,S).
help(_,_,S,S).

%TRANSPOSE/1
trans([[]|_], []).
trans(M, [R1|R]) :- transpose_help(M, R1, Final),
                                 trans(Final, R).
transpose_help([], [], []).
transpose_help([[H|T]|R], [H|Hs], [T|Ts]) :- transpose_help(R, Hs, Ts).

%ACCEPTABLE_DISTRIBUTION/1 it gets a row then transpose the list and check whether this row and the transposed head list are equal or not.
duplicate([ ], [ ]).   
duplicate([H1|R1], [H2|R2]):-
    H1 == H2,
    duplicate(R1, R2).

acceptable_distribution(M):-
   M=[HM|TM],trans(M,RM),RM=[HR|TR], \+ duplicate(HM,HR),acceptable_distribution(TM).
acceptable_distribution([]). 

%DISTINCT_ROWS/1 takes every row of the matrix(head) to check whether its repeated again in the remainig list(tail).
distinct_rows(M):-
   M = [H|T],\+member(H,T),distinct_rows(T).
distinct_rows([]).

%DISTINCT_COLUMNS/1 it does the transpose of the list and call the distinct row predicate on it.
distinct_columns(M1):-
   trans(M1,R),distinct_rows(R).
%ROW_COL_MATCH/1 it checks whether the list is an acceptable distribution then checks whether each row of the matrix has a corresponding column with the same value using the transpose predicate.
row_col_match(M):-
acceptable_distribution(M),
 trans(M,Fixed),pred_helper(M,Fixed).

pred_helper([H|T],F):-
    member(H,F),pred_helper(T,F).
pred_helper([],_).
%HELSINKI/2
helsinki(N,M):-
	grid_gen(N,M),check_num_grid(M), 
distinct_rows(M),distinct_columns(M),row_col_match(M).

