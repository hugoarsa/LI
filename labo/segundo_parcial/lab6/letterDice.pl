:- use_module(library(clpfd)).

%% A (6-sided) "letter dice" has on each side a different letter.
%% Find four of them, with the 24 letters abcdefghijklmnoprstuvwxy such
%% that you can make all the following words: bake, onyx, echo, oval,
%% gird, smug, jump, torn, luck, viny, lush, wrap.

%Some helpful predicates:

word( [b,a,k,e] ).
word( [o,n,y,x] ).
word( [e,c,h,o] ).
word( [o,v,a,l] ).
word( [g,i,r,d] ).
word( [s,m,u,g] ).
word( [j,u,m,p] ).
word( [t,o,r,n] ).
word( [l,u,c,k] ).
word( [v,i,n,y] ).
word( [l,u,s,h] ).
word( [w,r,a,p] ).

num(X,N):- nth1( N, [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,r,s,t,u,v,w,x,y], X ).

main:-
    % Domain

    length(D1,6),
    length(D2,6),
    length(D3,6),
    length(D4,6),
    D1 ins 1..24,
    D2 ins 1..24,
    D3 ins 1..24,
    D4 ins 1..24,

    append(D1, D2, T1),
    append(D3, D4, T2),
    append(T1, T2, Vars),
    all_distinct(Vars),

    % Constraints
   

    incompatiblePairs(IP),

    genIncompatibilityClauses(D1, IP),
    genIncompatibilityClauses(D2, IP),
    genIncompatibilityClauses(D3, IP),
    genIncompatibilityClauses(D4, IP),
   

    % Labelling
    label(Vars),
    % Print

    writeN(D1),
    writeN(D2),
    writeN(D3),
    writeN(D4), halt.
   
writeN(D):- findall(X,(member(N,D),num(X,N)),L), write(L), nl, !.

incompatiblePairs(IP) :- findall([N1,N2], (word(W), subsetOfSize(2, W, [X,Y]),num(X,N1),num(Y,N2)), IP).

getPairs(L, P):- findall(X, subsetOfSize(2, L, X), P).

genIncompatibilityClauses(_, []).    
genIncompatibilityClauses(D, [[A, B] | R]) :-
    getPairs(D, PD),
    genForAllPairs(PD, A, B),
    genIncompatibilityClauses(D, R).

genForAllPairs([], _, _).
genForAllPairs([[P1, P2] | R], A, B) :-
    (P1 #\= A #\/ P2 #\= B),
    (P1 #\= B #\/ P2 #\= A),
    % write(P1),write(P2),write(A),write(B),nl,
    genForAllPairs(R, A, B).

%%subsetOfSize(N,L,S). Creates all subsets S of L that have a size of N
subsetOfSize(0,_,[]):-!.
subsetOfSize(N,[X|L],[X|S]):- N1 is N-1, length(L,Leng), Leng>=N1, subsetOfSize(N1,L,S).
subsetOfSize(N,[_|L],   S ):-            length(L,Leng), Leng>=N,  subsetOfSize( N,L,S).