:- dynamic(solucion/3).

slice(F0,C0,F1,C1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Operaciones sobre rectangulos %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Area de un cuadrado A x B
area(H,W,X) :- X is H*W.

% Crea rectangulos con un elemento y una lista.
rectangulos(Elem, [], []). 
rectangulos(Elem, [L|Ls], X) :- rectangulos(Elem,Ls,X1), 
								append([Elem*L],X1,X).

% Dadas dos listas, crea todos los rectangulos posibles
rectangulos_todos([], _, []).
rectangulos_todos([E|Es], L, X) :- 	rectangulos(E,L,X1),
							   		rectangulos_todos(Es,L,X2), 
							  		append(X1,X2,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Operaciones sobre listas  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Crear una lista de Numeros
gen_number_list(Start,Start,[Start]).
gen_number_list(Start,End,X):-  End > Start, 
								Next is Start + 1,
								gen_number_list(Next,End,X1), 
								append([Start],X1,X), !. 

% Obtener el minimo elemento de una lista.
min([X], X) :- !.
min([X,Y|Tail], N):- X > Y , min([Y|Tail], N).
min([X,Y|Tail], N):- X =< Y, min([X|Tail], N).

% Obtener el maximo elemento de una lista.
max([X], X) :- !.
max([X,Y|Tail], N):- X > Y , max([X|Tail], N).
max([X,Y|Tail], N):- X =< Y, max([Y|Tail], N).

% Sumar todos los elementos de una lista.
list_sum([Item], Item).
list_sum([Item1,Item2 | Tail], Total) :- ItemR is Item1+Item2,
    								     list_sum([ItemR|Tail], Total), !.

% Remover un elemento de una lista.
remv(_, [], []).
remv(X, [X|T], T1) 		:- remv(X, T, T1).
remv(X, [H|T], [H|T1])  :- X \= H, remv(X, T, T1).

% Obtener la(s) sublista(s) de una lista.
subset([], L).
subset([X|Xs], L) :- member(X, L), subset(Xs, L).

% Remover los elementos simetricos de la lista.
remove_inv([],[]).
remove_inv([A*B|Ls], Result) :- remv(B*A,Ls,L1),
								remove_inv(L1,Result1),
								append([A*B],Result1,Result).

% Genera una lista con todos los sub rectangulos validos.
valid_sub_squares(N,[],[]).
valid_sub_squares(N, [H*W|Ls],Result) 	:-	subSquare(H,W,N), 
											valid_sub_squares(N,Ls,Result1),
											append([H*W],Result1,Result),!.
valid_sub_squares(N,[L|Ls],Result) 		:-	valid_sub_squares(N,Ls,Result).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Operaciones aritmeticas
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Division con techo.
halfUp(N, X) :- Resto is N mod 2, X is N // 2 + Resto.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Verificaciones sobre rectangulos %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Indica si el rectangulo es valido menor o igual a la mitad
subSquare(Heigth, Width, N) :- Total is N*N,
							   halfUp(Total, Half), 
							   Sub is Heigth * Width,
							   Half >= Heigth * Width.  


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Verificaciones sobre Listas %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Indica si el area de los sub rectangulos
% completa el area del cuadrado original.
valid_list(N,Rectangles) :-	list_sum(Rectangles, Total),
							Total =:= N*N.


gen_sub_squares(N,X) :-	gen_number_list(1,N,Numbers), 
						rectangulos_todos(Numbers, Numbers, X1),
						valid_sub_squares(N,X1,X2),
						remove_inv(X2,X).

squares_to_area([],[]).
squares_to_area([L|Ls], Result) :- 	A is L,
									squares_to_area(Ls,Result1),
									append([A],Result1,Result).

valid_sublist(N,Squares,Sub) :- 	sublist(Sub,Squares), 
				      				squares_to_area(Sub,X1),
									list_sum(X1,X2),
									N*N =:= X2.

winner_sublist(N,Squares) :-   valid_sublist(N,Squares,Sub),
							   bin_packing(N,[],Sub,X1),
							   squares_to_area(Sub,X2),
							   get_score(X2,X3),
							   solucion(N,_,A),
							   X3 < A,
							   retract(solucion(N,B,A)),
							   asserta(solucion(N,X1,X3)),
							   fail.


not(P) :- call(P), !, fail. 
not(P).

get_score(List,X) :- min(List,X1), max(List,X2), X is X2 - X1.

mondrian(N,Min,How,X1) :- asserta(solucion(N,[],N)), gen_sub_squares(N,X1), not(winner_sublist(N,X1)), solucion(N,How,Min).

prueba(N,Sub) :- gen_sub_squares(N,X1), valid_sublist(N,X1,Sub1), sort(Sub1,Sub2), squares_to_area(Sub2,Sub).



% Genera las dos posibilidades de un rectangulo
tetris(A*B,A*B).
tetris(A*B,B*A).

% Seleccionamos un rectangulo y lo colocamos en el tablero,
% Elegimos otro rectangulo y lo ponemos al lado,
% Vemos si no se pasa.
% Repetir.

% crear_slide([],A*B,X) :- slide(0,0,A,B).


bin_packing(_,Puestos, [], Puestos).
bin_packing(N, [] , 		Rects, X) :- select(A*B,Rects,Nrect),
										 tetris(A*B,A1*B1),
										 bin_packing(N, [slice(0,0,A,B)], Nrect, X).  
bin_packing(N, [P|Puestos], Rects, X) :- select(A*B, Rects, Nrect),
										 tetris(A*B,A1*B1),
										 si_cabe(N,P,A1*B1,Pnuevo),
										 no_solapan_todos(Pnuevo,[P|Puestos]),
										 bin_packing(N,[Pnuevo,P|Puestos],Nrect, X).
										 

solapan(slice(X,Y,A,B),slice(X1,Y1,A1,B1)) :- 
	A > X1, A1 > X, B > Y1, B1 > Y. 

no_solapan_todos(_,[]).
no_solapan_todos(S,[L|Ls]) :- not(solapan(S,L)),
							  no_solapan_todos(S,Ls). 

%Derecha
si_cabe(N, slice(F0,C0,F1,C1), A*B, slice(F1,C0,A1,NY)) :-	NX is N - F1, 
															A =< NX, 
															NY is C0+B,
															NY =< N, 
															A1 is F1 + A.  %Derecha
%IZQUIERDA
si_cabe(N, slice(F0,C0,F1,C1), A*B, slice(A1,C0,F0,NY)) :-  A =< F0,
															A1 is F0 - A,
															NY is C0+B,
															NY =< N.

%Arriba
si_cabe(N, slice(F0,C0,F1,C1), A*B, slice(F0,NY,A1,C0)) :-  B =< C0,
															NX is N - F0,
															A =< NX,
															NY is C0 - B,
															A1 is F0 + A.

%Abajo
si_cabe(N, slice(F0,C0,F1,C1), A*B, slice(F0,C1,NX,NY)) :- NY is C1 + B,
														   NY =< N,
														   NX is F0 + A,
														   NX =< N.




















