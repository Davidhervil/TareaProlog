:- dynamic(soluc/2).

% Caballito Indomable :D
k(X,Y).

not(P) :- call(P), !, fail. 
not(P).

nextElem([],[]).
nextElem([X|Xs],[X|Xs]).
nextElem([X|Xs],R) :- nextElem(Xs,R).

even(N) :- 0 =:= N mod 2.
odd(N) :- 1 =:= N mod 2.

eliminar(Item,[],[]).
eliminar(Item, [Item|Ls], Ls).
eliminar(Item, [L|Ls]   , R) :- L \= Item, eliminar(Item, Ls, R1), append([L],R1,R).  

eliminarMultiple([],B,B).
eliminarMultiple([A|As], B, R) :- eliminar(A,B,R1), eliminarMultiple(As,R1,R).

% Generar lista de números de 1 a N
genList(N,X) :- genList1(N,[],X).
genList1(0,L,L) :- !. 
genList1(N,R,L) :- N > 0, N1 is N-1, genList1(N1,[N|R],L).


% Empareja un elemento A con todos los elementos de una lista B
emparejar(A, [B], [k(A,B)]) :- !.
emparejar(A, [B|Bs], R) 	:- emparejar(A,Bs,R1), append([k(A,B)], R1, R). 

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
