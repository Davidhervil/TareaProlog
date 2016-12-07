
% Recorrer una lista con backtracking.
recorrer([X|Xs],X).
recorrer([X|Xs],R) :- recorrer(Xs,R).







% Generar lista de números de 1 a N
genList(1,[1]) :- !.
genList(N,X) :- N1 is N-1,
                genList(N1,X1),
                append(X1, [N], X),
                !.

% Combinar uno con todos
combinar(A, [B], [[A,B]]).
combinar(A, [B|Bs], X) :-     combinar(A,Bs,X1),
                             append([[A,B]], X1, X).
% Crear tablero completo
tablero([A],B,X)    :-     combinar(A,B,X).
tablero([A|As],B,X) :-     tablero(As,B,X1),
                        combinar(A,B,X2),
                        append(X1,X2,X).
