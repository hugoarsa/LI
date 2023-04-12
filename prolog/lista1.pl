% exercise 1

prod([],1).
prod([X|L],P):- prod([L],Y), P is X * Y.

% exercici 2

prescalar([],[],0).
prescalar([X|L1],[Y|L2],P):-  prescalar([L1],[L2],Q), P is Q + X * Y.

% exercici 3 

uni([],L2,L2).
uni(L1,[],L1).
uni([X|L1],L2,U):- member(X,L2), !, uni(L1,L2,U).
uni([X|L1],L2,[X|U]):- uni(L1,L2,U).

inter([],_,[]).
inter([X|L1],L2,U):- member(X,L2), !, inter(L1,L2,U). %con el ! cribo todos los otros posibles
inter([_|L1],L2,U):- inter(L1,L2,U).

% exercici 4

last([X],X).
last([_|L],N):- last(L,N). 

ultimo(L,X):- append(_,[X],L).

reverse([X],[X]).
reverse(L,[X|R]):- append(L2,[X],L), reverse(L2,R).

%exercici 5

fib(1,1). %caso base 1
fib(2,1). %caso base 2 
fib(N,F):- N > 2, N1 is N-1, N2 is N-2, fib(N1,F1), fib(N2,F2), F is F1+F2. %caso recursivo

%exircici 6


dados(0,0,[]).
dados(P,N,[X|L]) :- N > 0, between(1,6,X), M is N-1, Q is P-X, dados(Q,M,L).
%empezamos con un total de puntos a crear p y dados n
%determinizamos un dado, dandole valor
%restamos ese valor del total para crear el nuevo total con ese determinizado
%restamos la cantidad de dados disponibles y pasamos al siguente

suma([],0).
suma([X|L],S) :- suma(L,S1), S is S1+X.

%esta funcion auxiliar permite L = Resto - [X], a la vez que Resto + [X] = L
pert_con_resto(X,L,Resto):- append(L1,[X|L2],L), append(L1,L2,Resto).

%exercici 7
suma_demas(L) :- pert_con_resto(X,L,R), suma(R,X), !. % si encontramos uno basta

%exercici 8
suma_ants(L) :-   append(L1,[X|_],L),  suma(L1,X), !.

%exercici 9
card(List):- cards(List, Res), write(Res).
 
%caso base evientemente consiste en lista vacía
cards([], []).
%por cada elemento de la lista con un numero de apariciones mayor a uno
%sustituimos 
cards([L|List], [[L, NumL]|Res]):- cards(List, ResList),
              pert_con_resto([L, NewNumL], ResList, Res),
              %append([L, NewNumL], Res, ResList), no pillo pq este no esta biens
              NumL is NewNumL+1.
cards([L|List], [[L, 1]|Res]):- cards(List, Res).

%exercici 10
esta_ordenada([]).
esta_ordenada([_]).
esta_ordenada([X,Y|L]):- X =< Y, esta_ordenada([Y|L]).

%exercici 11
ordenacion(L1,L2):- permutation(L1,L2), esta_ordenada(L2).

%exercici 12

escriu([]) :- write(' '),!.
escriu([X|L]) :- write(X), escriu(L).

nmembers(_,0,[]):- !.
nmembers(A,N,[X|L]):- member(X,A), N1 is N-1,nmembers(A,N1,L).


diccionario(A,N) :- nmembers(A,N,L), escriu(L), fail.

%exercici 13
palindromos2(L1):- permutation(L1,L), reverse(L,R), L = R,  write(R), nl, fail.


palindroms(L) :- permutation(L,P), es_palindromo(P), 
  write(P), nl, fail. 
palindroms(_). 

es_palindromo([]).
es_palindromo([_]) :- !. % regla adecuada
es_palindromo([X|L]) :- append(L1,[X],L), es_palindromo(L1). 

palindromos(L) :- setof(P,(permutation(L,P), es_palindromo(P)),S), write(S). 

%exercici 14

sendMoreMoney:-  Letters = [S, E, N, D, M, O, R, Y, _, _],
Numbers = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
    permutation(Letters, Numbers),
    S1 is 1000 * S + 100 * E + 10 * N + D +
            1000 * M + 100 * O + 10 * R + E,
    S1 is 10000 * M + 1000 * O + 100 * N + 10 * E + Y,
    writeLetters(Letters), ! .

writeLetters([S, E, N, D, M, O, R, Y, _, _]):-
    write('S = '), write(S), nl,
    write('E = '), write(E), nl,
    write('N = '), write(N), nl,
    write('D = '), write(D), nl,
    write('M = '), write(M), nl,
    write('O = '), write(O), nl,                                                 
    write('R = '), write(R), nl,
    write('Y = '), write(Y), nl,
    writeSuma([S, E, N, D, M, O, R, Y]).


writeSuma([S, E, N, D, M, O, R, Y]):-
    S1 is 1000 * S + 100 * E + 10 * N + D,
    S2 is 1000 * M + 100 * O + 10 * R + E,
    S3 is S1 + S2,
    S4 is  10000 * M + 1000 * O + 100 * N + 10 * E + Y,
    write('SEND = '), write(S1), 
    write(' + '), 
    write('MORE = '), write(S2), nl,
    write('      = '), write(S3), nl,
    write( '----------------------------------------' ), nl, 
    write('MONEY = '), write(S4).

%exercici 15

deriva(X,X,1):-!.
deriva(C,_,0):- number(C).
deriva(A+B,X,DA+DB):-         deriva(A,X,DA),deriva(B,X,DB).
deriva(A-B,X,DA-DB):-         deriva(A,X,DA),deriva(B,X,DB).
deriva(A*B,X,A*DB+B*DA):-     deriva(A,X,DA),deriva(B,X,DB).
deriva(sin(A),X,cos(A)*DA):-  deriva(A,X,DA).
deriva(cos(A),X,-sin(A)*DA):- deriva(A,X,DA).
deriva(eˆA,X,DA*eˆA):-        deriva(A,X,DA).
deriva(ln(A),X,DA*1/A):-      deriva(A,X,DA).

%exercici 16

%exercici 17

%exercici 18

%exercici 19

%exercici 20

flatten([], []):- !.
flatten(X, [X]):- X\=[_|_].
flatten([L|List], FList):- flatten(L, L2), flatten(List, List2), append(L2, List2, FList).

flattenNoRepetitions(L, RL):- flatten(L, L2), noRepes(L2, RL).

noRepes([], []):- !.
noRepes([L|List], [L|Res]):- \+member(L, List), noRepes(List, Res).
noRepes([_|List], Res):- noRepes(List, Res).

%exercici 22 esta aparte pero no hecho aun