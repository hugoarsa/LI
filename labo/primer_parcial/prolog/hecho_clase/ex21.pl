
log( B, N, L ):- between(1,N,L), N1 is B ** L, N2 is B ** (L + 1), N1 =< N, N2 > N. 