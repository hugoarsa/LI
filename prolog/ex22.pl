genList(0, []).
genList(N, L):- append([N],L1,L), N1 is N - 1, genList(N1,L1).

subconjunto([], []).
subconjunto([E|Tail], [E|NTail]):-
    subconjunto(Tail, NTail).
subconjunto([_|Tail], NTail):-
    subconjunto(Tail, NTail).

checkAll(_,[],_).
checkAll(X,[Y|S],L):- A is [X,Y], B is [Y,X], not(member(A,L)),not(member(B,L)),checkAll(X,S,L).

check([], _).
check([X|S], L):- checkAll(X, S, L), check(S,L).

li(N,M,L,S):- genList(N,L1), subconjunto(L1,S), length(S, X), X == M, check(S, L).

%li( 20, 4, [[8,11],[8,15],[11,6],[4,9],[18,13],[7,9],[16,8],[18,10],[6,17],[8,20]], S )

% auxMember([],[1,2,3,4]).
% auxMember([],[1,2,3,4]).