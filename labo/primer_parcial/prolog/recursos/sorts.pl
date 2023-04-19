%merge sort

split([],[],[]):-!.
split(Lista,Izquierda,Derecha):-
    concat(Izquierda,Derecha,Lista),
    length(Izquierda,ElemIzda),
    length(Derecha,ElemDcha),
    ElemIzda = ElemDcha, !.
split(Lista,Izquierda,Derecha):-
    concat(Izquierda,Derecha,Lista),
    length(Izquierda,ElemIzda),
    length(Derecha,ElemDcha),
    ElemDcha1 is ElemDcha + 1,
    ElemIzda = ElemDcha1, !.

merge(L, [], L):- !.
merge([], L, L):- !.
merge([HL | L], [HR | R], [HL | Ordenada]):-
    HL =< HR, !,
    merge(L, [HR | R], Ordenada).
merge([HL | L], [HR | R], [HR | Ordenada]):-
    merge([HL | L], R, Ordenada).

mergeSort([],[]):- !.
mergeSort([X],[X]):- !.
mergeSort(Lista,ListaOrdenada):-
    split(Lista,L,R),
    mergeSort(L,LL),
    mergeSort(R,RR),
    merge(LL,RR,ListaOrdenada).

%insertion sort

ordenacionInsercion([], []).
ordenacionInsercion([X | Tail], Ordenada):-
  ordenacionInsercion(Tail, Tmp),
  insercion(X, Tmp, Ordenada),!.

insercion(X, [], [ X ]).
insercion(X, [ Y | L], [ X, Y | L]):-
  X =< Y.
insercion(X, [ Y | L], [ Y | LL]):-
  X > Y,
  insercion(X, L, LL).

%permutation sort

estaOrdenada([]).
estaOrdenada([_]):- !.
estaOrdenada([H,H_1|Tail]):-
	H =< H_1,
	estaOrdenada([H_1|Tail]).

ordenacionSeleccion(L1,L2):-
	permutation(L1,L2),
	estaOrdenada(L2).