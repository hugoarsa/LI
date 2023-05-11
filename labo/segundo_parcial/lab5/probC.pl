%instrucciones(L) --> <instrucciones>.
instrucciones(L) --> instruccion(L).


%instrucciones(L) --> <instruccion> ; <instrucciones>.
%instrucciones(L) --> append(L1,[;|L2],L), instruccion(L1), instrucciones(L2) %%amb append/3
instrucciones(L) --> append([L1,[;],L2],L), instruccion(L1), instrucciones(L2). %%amb append/2
