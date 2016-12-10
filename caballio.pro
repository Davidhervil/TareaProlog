:- dynamic(soluc/2).
% Para resolvr el problema de los caballos en lugar de comenzar con un tablero
% vacio e ir colocando caballos con backtracking se decidio comenzar de un
% talero lleno de caballos e ir escogiendo aquellos que seran los sobrevivientes
% con backtracking

% Representa un caballo en la posicion X,Y.
% Se asume que X y Y seran numeros
k(X,Y).

% Predicado que triunfa cuando el predicado P no triunfa.
% OJO revisar si es verdaderamente NOT
not(P) :- call(P), !, fail. 
not(P).

% Predicado que triunfa cuando en R se encuentra una copia de la lista
% que esta de primer parametro y al volverse a invocar devuelve la lista pero 
% sin el primer elemento. Se utiliza para ir seleccionando diferentes caballos
% en el tablero.
nextElem([],[]).
nextElem([X|Xs],[X|Xs]).
nextElem([X|Xs],R) :- nextElem(Xs,R).

% Predicado que triunfa si N es par
even(N) :- 0 =:= N mod 2.

% Predicado que triunfa si N es impar
odd(N) :- 1 =:= N mod 2.

% Predicado que triunfa cuando R unifica con la lista de segundo parametro sin
% el elemento Item. SI el elemento ocurre mas de una vez solo se elimina la primera
% ocurrencia
eliminar(Item,[],[]).
eliminar(Item, [Item|Ls], Ls).
eliminar(Item, [L|Ls]   , R) :- L \= Item, eliminar(Item, Ls, R1), append([L],R1,R).  

% Predicado que triunfa cuando la lista de tercer parametro unifica con la segunda
% habiendo eliminado en ella las primeras ocurrecias de los elementos de la primera
% lista
eliminarMultiple([],B,B).
eliminarMultiple([A|As], B, R) :- eliminar(A,B,R1), eliminarMultiple(As,R1,R).

% Predicado que triunfa cuando en X hay una lista con los numeros de 1 a N
genList(N,X) :- genList1(N,[],X).
genList1(0,L,L) :- !. 
genList1(N,R,L) :- N > 0, N1 is N-1, genList1(N1,[N|R],L).


% Predicado que triunfa cuando en el tercer parametro, lista de k-ballos,
% las posiciones de los mismos son el resultado de hacer todas las combinaciones  
% de A con los elementos de la segunda lista.
% Empareja un elemento A con todos los elementos de una lista B
emparejar(A, [B], [k(A,B)]) :- !.
emparejar(A, [B|Bs], R) 	:- emparejar(A,Bs,R1), append([k(A,B)], R1, R). 

% Genera en la lista R todos los k-ballos cuyas posiciones son la combinacion de
% los elementos de la lista A y la B.
emparejarTodos([A],B,R) 	 :- emparejar(A,B,R).
emparejarTodos([A|As], B, R) :- As \= [],
								emparejarTodos(As, B, R1),
								emparejar(A, B, R2),
								append(R2,R1,R).

combinar(N,N1,R) :- genList(N,Alto), genList(N1,Ancho), emparejarTodos(Alto,Ancho,R). 

construirTablero(N, T) :- N > 0, even(N), N1 is N//2    , combinar(N,N1,T).
construirTablero(N, T) :- N > 0, odd(N) , N1 is N//2 + 1, combinar(N, N1, T).

suma(k(A,A1), k(B,B1), k(X,X1)) :- X is A + B, X1 is A1 + B1.

come(Caballo, Sitios) :- maplist(suma(Caballo),
								[k(2,1),k(2,-1),
								k(1,-2),k(1,2),
								k(-2,1),k(-2,-1),
								k(-1,-2),k(-1,2)]
								,Sitios).

verificarEntorno(X,Y,Xg,Yg) :- Xg >= X.
verificarEntorno(X,Y,Xg,Yg) :- Xg < X, Yg > Y.

lalal(N,[],[]).
lalal(N, Tablero, TableroR) :- 	nextElem(Tablero, [Actual|TableroN]),
								Actual 	  = k(X1,Y1),
								X2 is N - X1 + 1,
								Y2 is N - Y1 + 1,
								verificarEntorno(X1,Y1,X2,Y2),
								Otro	  = k(X2,Y2),
								come(Actual, Muertos1),
								come(Otro  , Muertos2),
								eliminarMultiple(Muertos1,TableroN,Tablero1),
								eliminarMultiple(Muertos2,Tablero1,Tablero2),
								eliminarMultiple([Actual,Otro],Tablero2,Tablero3),
								lalal(N, Tablero3, R1),
								lalal1(Actual,Otro,R1,TableroR).

lalal1(Actual, Actual, Tablero, [Actual|Tablero]):-!.
lalal1(Actual, Otro, Tablero, [Actual,Otro|Tablero]).

arre1(N,L) :- construirTablero(N,T),!, lalal(N,T,L).

start(N):- asserta(soluc(N,0)), end(N). 

end(N):-	arre1(N,X1), 
		  	length(X1,L),
		 	soluc(N,A), 
		 	L > A,
		 	retract(soluc(N,A)), 
		 	asserta(soluc(N,L)), 
		 	fail.

arre(N,L,X) :-   not(start(N)), 
				 soluc(N,L),
				 arre1(N,X),
				 length(X,L). 