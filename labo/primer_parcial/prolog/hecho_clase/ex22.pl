genList(0, []).
genList(N, L):- append([N],L1,L), N1 is N - 1, genList(N1,L1).

subconjunto([], []).
subconjunto([E|Tail], [E|NTail]):-
    subconjunto(Tail, NTail).
subconjunto([_|Tail], NTail):-
    subconjunto(Tail, NTail).

subsetOfSize(0,_,[]):-!.
subsetOfSize(N,[X|L],[X|S]):- N1 is N-1, length(L,Leng), Leng>=N1, subsetOfSize(N1,L,S).
subsetOfSize(N,[_|L],   S ):-            length(L,Leng), Leng>=N,  subsetOfSize( N,L,S).

checkAll(X,[],_).
checkAll(X,[Y|S],L):- not(member([X,Y],L)),not(member([Y,X],L)),checkAll(X,S,L).

check([],_).
check([X|S], L):- checkAll(X, S, L), check(S,L).

li(N,M,L,S):- genList(N,L1), subsetOfSize(M,L1,S), check(S,L).
