main:- 
    
    EstadoInicial = [[1,2,5,8],[],i], 
    
    EstadoFinal = [[],[1,2,5,8],f],
    
    between(1,1000,CosteMax), % Buscamos soluci ́on de coste 0; si no, de 1, etc.
    camino( CosteMax, EstadoInicial, EstadoFinal, [EstadoInicial], Camino ),
    reverse(Camino,Camino1), write(Camino1), write(" con coste "), write(CosteMax), nl, halt.
    
    
    camino( 0, E,E, C,C ). % Caso base: cuando el estado actual es el estado final.
    camino( CosteMax, EstadoActual, EstadoFinal, CaminoHastaAhora, CaminoTotal ):-
    CosteMax>0,
    
    unPaso( CostePaso, EstadoActual, EstadoSiguiente ), % En B.1 y B.2, CostePaso es 1.
    \+member( EstadoSiguiente, CaminoHastaAhora ),
    
    CosteMax1 is CosteMax-CostePaso,
    camino(CosteMax1,EstadoSiguiente,EstadoFinal, [EstadoSiguiente|CaminoHastaAhora], CaminoTotal).
    
    

    % Mover persona de origen a fin
    unPaso(Cost, [OI,DI,i], [OS,DS,f]):-
        select(X,OI,OS),
        Cost is X,
        append([X],DI,TMP2), sort(TMP2,DS).

    % Mover 2 personas de origen a fin
    unPaso(Cost, [OI,DI,i], [OS,DS,f]):-
        select(X,OI,TMP),
        select(Y,TMP,OS),
        Cost is max(X,Y),
        append([X,Y],DI,TMP2), sort(TMP2,DS).

    % Mover persona de origen a fin
    unPaso(Cost, [OI,DI,f], [OS,DS,i]):-
        select(X,DI,DS),
        Cost is X,
        append([X],OI,TMP2), sort(TMP2,OS).

    % Mover 2 personas de origen a fin
    unPaso(Cost, [OI,DI,f], [OS,DS,i]):-
        select(X,DI,TMP),
        select(Y,TMP,DS),
        Cost is max(X,Y),
        append([X,Y],OI,TMP2), sort(TMP2,OS).
