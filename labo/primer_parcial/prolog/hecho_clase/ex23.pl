%% Example:
numbers([2,5,7,-2,2,9,3,4,1]).
maxSum(6).

pert_con_resto(X,L,Resto):- append(L1,[X|L2],L), append(L1,L2,Resto).

subset([], []).
subset([E|Tail], [E|NTail]):-
  subset(Tail, NTail).
subset([_|Tail], NTail):-
  subset(Tail, NTail).

%% subsetWithRest(L, Subset, Rest) holds
%% if Subset is a subset of L and Rest is the rest of the elements.
subsetWithRest([], [], []).
subsetWithRest([E|Tail], [E|STail], Rest):-subsetWithRest(Tail, STail, Rest).
subsetWithRest([X|Tail], STail, [X|Rest]):-subsetWithRest(Tail, STail, Rest).

subset([], []).
subset([E|Tail], [E|NTail]):-
  subset(Tail, NTail).
subset([_|Tail], NTail):-
  subset(Tail, NTail).

suma([],0).
suma([X|L],S) :- suma(L,S1), S is S1+X.

aux([], Sm, K).
aux([Y|L], Sm, K):- suma(Sm, X), Y + X > K, aux(L, Sm, K).
%% maxSubset(K, L, Sm) holds
%% if Sm is a subset of numbers of L such that
%% it sums at most K
%% and if we try to add any other element, the sum exceeds K.
maxSubset(K, L, Sm):-
subsetWithRest(L, Sm, Rest), suma(Sm, X), X =< K, aux(Rest, Sm, K).




main :-
numbers(L), maxSum(K),
maxSubset(K, L, Sm),
write(Sm), nl, fail.
main:- halt.