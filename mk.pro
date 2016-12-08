% Representa una posicion donde un caballo esta comiendo
% Se supone que X y Y seran numeros
nom(X,Y).

% Representa un caballo en la posicion X,Y.
% Se asume que X,Y son numeros
k(X,Y).

% Predicado que Unifica en Noms las posibles opciones de comer
% de un caballo que se encuentre e la casilla X,Y
caballo_come_en(X, Y, Noms) :-
	NX1 is X - 1, NY1 is Y - 2,
    NX2 is X - 1, NY2 is Y + 2, 
    NX3 is X + 1, NY3 is Y - 2,
    NX4 is X + 1, NY4 is Y + 2,
    NX5 is X - 2, NY5 is Y - 1,
    NX6 is X - 2, NY6 is Y + 1,
    NX7 is X + 2, NY7 is Y - 1,
    NX8 is X + 2, NY8 is Y + 1,
    Nom1 = nom(NX1,NY1),
    Nom2 = nom(NX2,NY2),
    Nom3 = nom(NX3,NY3),
    Nom4 = nom(NX4,NY4),
    Nom5 = nom(NX5,NY5),
    Nom6 = nom(NX6,NY6),
    Nom7 = nom(NX7,NY7),
    Nom8 = nom(NX8,NY8),
    Noms = [ Nom1, Nom2, Nom3, Nom4,
             Nom5, Nom6, Nom7, Nom8
	    ].

% Predicado que dada una posicion X,Y en un tablero de NxN devuelve
% en Xf y Yf la siguiente. Se recorre el tablero fila por fila.
nextpos(N,X0,N,Xf,Yf)  :- Xf is X0 + 1, Xf =< N, Yf = 1.
nextpos(N,X0,Y0,Xf,Yf) :- Y0 =< N, Xf = X0, Yf is Y0 + 1, Yf =< N.

% Predicado que determina si un caballo k sobrevive el esado de posiciones
% donde potencialmente puede ser comido.
sobrevive(k(X,Y),[nom(ChompX,ChompY)|Death]) :- ChompX \= X,ChompY \= Y,
						sobrevive(k(X,Y),Death).

% Predicado que triunfa si NewOne sobrevive las comidas EatHere de los Knights
% y NewParty es la lista de de Knights con NewONe metido y EatsNow las nuevas
% posiciones donde los caballos comen.
meteruno(Knights,EatHere,NewOne,EatsHere,NewParty,EatsNow) :- NewOne = k(X,Y),
							sobrevive(NewOne,EatHere),
							append([NewOne],Knights,NewParty),
							append(EatsHere,EatHere,EatsNow).
							
%While? maybe
dale()
%While?
arre(N,Max,Lista):- nextpos(1,1,X,Y),
		    dale([k()],)

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
