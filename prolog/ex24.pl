%%==== Example: ========================================================
numVertices(10).
minCliqueSize(4).
vertices(Vs):- numVertices(N), findall(I,between(1,N,I),Vs).
vertex(V):- vertices(Vs), member(V,Vs).
edge(U,V):- edge1(U,V).
edge(U,V):- edge1(V,U).
edge1(9,8).
edge1(8,2).
edge1(7,4).
edge1(5,7).
edge1(4,2).
edge1(5,2).
edge1(2,7).
edge1(7,9).
edge1(2,9).
edge1(4,8).
edge1(4,9).
edge1(9,5).
edge1(4,5).
%%==========================================================

subconjunto([], []).
subconjunto([E|Tail], [E|NTail]):-
    subconjunto(Tail, NTail).
subconjunto([_|Tail], NTail):-
    subconjunto(Tail, NTail).

pert_con_resto(X,L,Resto):- append(L1,[X|L2],L), append(L1,L2,Resto).

main:- 
    vertices(Vs),
    minCliqueSize(Min),
    subconjunto(Vs, S), 
    length(S, X), X >= Min,
    isClique(S),
    write(S), nl, fail.
main:- halt.

aux([], S1).
aux([X|S],S1):- pert_con_resto(X,S1,Resto), aux2(X,Resto),aux(S,S1).

aux2(X,[]).
aux2(X,[Y|S]):- edge(X,Y), aux2(X,S).

isClique(S):- aux(S,S).
