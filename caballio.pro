:- dynamic(sol/2).

not(P) :- call(P), !, fail. 
not(P).

% Recorrer una lista con backtracking.
recorrer([],[]).
recorrer([X|Xs],[X|Xs]).
recorrer([X|Xs],R) :- recorrer(Xs,R).

% Generar lista de números de 1 a N
genList(N,X) :- genList1(N,[],X).
genList1(0,L,L) :- !. 
genList1(N,R,L) :- N > 0, N1 is N-1, genList1(N1,[N|R],L).

% Combinar uno con todos
combinar(A, [B], [[A,B]])	:- !.
combinar(A, [B|Bs], X)	:-  combinar(A,Bs,X1),
                            append([[A,B]], X1, X).
% Crear tablero completo
tablero([A],B,X)    :-  combinar(A,B,X), !.
tablero([A|As],B,X) :-	tablero(As,B,X1),
                        combinar(A,B,X2),
                        append(X2,X1,X).

% Intersecta dos listas
intersect(_,[],[]).
intersect([],_,[]).
intersect([A|As],B,[A|X]) 	:- member(A,B) , intersect(As,B,X), !.
intersect([A|As],B,X) 		:- intersect(As,B,X), !.

% Realiza la suma de dos pares
suma([A,A1],[B,B1],[X,X1]) :- X is A+B, X1 is A1+B1.

% Genera todos los movimientos validos
moves(A,N,X) :- prueba(N,T), 
				maplist(suma(A),[[2,1],[2,-1],
								 [1,-2],[1,2],
								 [-2,1],[-2,-1],
								 [-1,-2],[-1,2]]
								 ,X1),
				intersect(X1,T,X).

% Resta la primera lista de la segunda (B-A)
deleteM([],B,B).
deleteM([A|As],B,X) :- delete(B,A,X1), deleteM(As,X1,X).

max(A,B,A) :- A>B.
max(A,B,B) :- B>=A. 
				
prueba(N,X) :- 	genList(N,A), 
				tablero(A,A,X).

%Pasar esto a recursivo de cola.
raux(N,[],[]).
raux(N,A,X) :- 	recorrer(A,[T|Ts]),			% Agarramos primer elemento con el resto para seguir.
				moves(T,N,COME),			% Hallamos los movimientos
				delete(Ts,T,T1),			% Quitamos el actual y los que come
				deleteM(COME,T1,T2),		% Quitamos los que come
				raux(N,T2,X1),				% llamamos con las posiciones restantes.
				append([T],X1,X).

resolv(N,X) :- prueba(N,T), raux(N,T,X).
start(N):- asserta(sol(N,0)), end(N). 

end(N):-	resolv(N,X1), 
		  	length(X1,L),
		 	sol(N,A), 
		 	L > A, 
		 	retract(sol(N,A)), 
		 	asserta(sol(N,L)), 
		 	fail.

result(N,L,X) :- not(start(N)), 
				 sol(N,L),
				 resolv(N,X),
				 length(X,L). 
