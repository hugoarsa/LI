%% Write a Prolog predicate eqSplit(L,S1,S2) that, given a list of
%% integers L, splits it into two disjoint subsets S1 and S2 such that
%% the sum of the numbers in S1 is equal to the sum of S2. It should
%% behave as follows:
%%
%% ?- eqSplit([1,5,2,3,4,7],S1,S2), write(S1), write('    '), write(S2), nl, fail.
%%
%% [1,5,2,3]    [4,7]
%% [1,3,7]    [5,2,4]
%% [5,2,4]    [1,3,7]
%% [4,7]    [1,5,2,3]

intersection([],_,[]).
intersection([L1H|L1T],L2, [L1H|I]):-
	member(L1H,L2),!,
	intersection(L1T, L2, I).
intersection([_|L1T],L2, I):-
	intersection(L1T, L2, I).

union([],L,L).
union([X|L1],L2,   L3 ):-
	member(X,L2),!,
	union(L1,L2,L3).
union([X|L1],L2,[X|L3]):-
	union(L1,L2,L3).

subset([], []).
subset([E|Tail], [E|NTail]):-
    subset(Tail, NTail).
subset([_|Tail], NTail):-
    subset(Tail, NTail).

suma([],0).
suma([X|L],S) :- suma(L,S1), S is S1+X.

% in_A_but_not_in_B(+LstA, +LstB, +Lst_in_Construction, -Lst_Final)
% when A is finished, we unify Lst_in_Construction and Lst_Final
a_diff_b([], _, L, L).

% we check if the first element of A is in B
a_diff_b([H|T], B, LC, LF) :-
    member(H, B),
    a_diff_b(T, B, LC, LF).

% we check if the first element of A is in B
a_diff_b([H|T], B, LC, LF) :-
    \+member(H, B),
    a_diff_b(T, B, [H| LC], LF).

eqSplit(L,S1,S2):- subset(L,S1), a_diff_b(L,S1,[],S2), suma(S1,X), suma(S2,Y), X == Y.