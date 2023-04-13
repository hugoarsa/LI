%diccionario
diccionario(L, N):-
	findWords(L, N, Word),
	write(Word), write(' '), fail.

findWords(_, 0, []):- !.
findWords(L, N, [X | Word]):-
	member(X,L),
	N1 is N-1,
	findWords(L, N1, Word).

%sets

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

%%pert_con_resto(X,L,Resto) Resto = L - [X]
pert_con_resto(X,L,Resto):- append(L1,[X|L2],L), append(L1,L2,Resto).

%%subset(L,S) S subset of L
subset([], []).
subset([E|Tail], [E|NTail]):-
    subset(Tail, NTail).
subset([_|Tail], NTail):-
    subset(Tail, NTail).

%% subsetWithRest(L, Subset, Rest) Rest = L - Subset
subsetWithRest([], [], []).
subsetWithRest([E|Tail], [E|STail], Rest):-subsetWithRest(Tail, STail, Rest).
subsetWithRest([X|Tail], STail, [X|Rest]):-subsetWithRest(Tail, STail, Rest).

%%subsetOfSize(N,L,S). Creates all subsets S of L that have a size of N
subsetOfSize(0,_,[]):-!.
subsetOfSize(N,[X|L],[X|S]):- N1 is N-1, length(L,Leng), Leng>=N1, subsetOfSize(N1,L,S).
subsetOfSize(N,[_|L],   S ):-            length(L,Leng), Leng>=N,  subsetOfSize( N,L,S).

subseq([], []).
subseq([_|Xs], Ys) :-
    subseq(Xs, Ys).
subseq([X|Xs], [X|Ys]) :-
    prefix_subseq(Xs, Ys).

prefix_subseq(_, []).
prefix_subseq([X|Xs], [X|Ys]) :-
    prefix_subseq(Xs, Ys).


%listas

concat([],L,L).
concat([X|L1],L2,[X|L3]):-
	concat(L1,L2,L3).

last([A],A).
last([_|LT],T):-
	last(LT,T).

myReverse([], []).
myReverse([H | Tail], RL):-
	myReverse(Tail, Tmp),
	concat(Tmp, [H], RL).
