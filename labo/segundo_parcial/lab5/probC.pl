solucio(SOL) :-
    SOL = [ [1,_,_,_,_,_],
            [2,_,_,_,_,_],
            [3,_,_,_,_,_],
            [4,_,_,_,_,_],
            [5,_,_,_,_,_]],

    %%1 - El que vive en la casa roja es de Peru
    member([_,_,_,_,_,peru],SOL),

    %%2 - Al frances le gusta el perro
    member([_,_,_,perro,_,francia],SOL),

    %%3 - El pintor es japones
    member([_,_,pintor,_,_,japon],SOL),

    %%4 - Al chino le gusta el ron
    member([_,_,_,_,ron,china],SOL),

    %%5 - El hungaro vive en la primera casa
    member([1,_,_,_,_,hungria],SOL),

    %%6 - Al de la casa verde le gusta el conac
    member([_,verde,_,_,conac,_],SOL),

    %%7 - La casa verde esta justo a la izquierda de la blanca
    member([NumV,verde,_,_,_,_],SOL),
    member([NumB,blanca,_,_,_,_],SOL),
    NumV is NumB - 1,

    %%8 - El escultor cria caracoles
    member([_,_,escultor,caracol,_,_],SOL),

    %%9 - El de la casa amarilla es actor
    member([_,amarilla,actor,_,_,_],SOL),

    %%10 - El de la tercera casa bebe cava
    member([3,_,_,_,cava,_],SOL),

    %%11 - El que vive al lado del actor tiene un caballo
    member([Num1_11,_,actor,_,_,_],SOL),
    member([Num2_11,_,_,caballo,_,_],SOL),
    nextTo(Num1_11,Num2_11),

    %%12 - El hungaro vive al lado de la casa azul
    member([Num1_12,_,_,_,_,hungria],SOL),
    member([Num2_12,azul,_,_,_,_],SOL),
    nextTo(Num1_12,Num2_12),

    %%13 - Al notario la gusta el whisky
    member([_,_,notario,_,whisky,_],SOL),

    %%14 - El que vive al lado del medico tiene un ardilla
    member([Num1_14,_,medico,_,_,_],SOL),
    member([Num2_14,_,_,ardilla,_,_],SOL),
    nextTo(Num1_14,Num2_14),

    write(SOL), !.

nextTo(Num1,Num2):-
    (Num2 is Num1 - 1; Num2 is Num1 + 1).