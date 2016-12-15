:- dynamic(span/2).
%           ;               ,           
%         ,;                 '.         
%        ;:                   :;        
%       ::                     ::       
%       ::                     ::       
%       ':                     :        
%        :.                    :        
%     ;' ::                   ::  '     
%    .'  ';                   ;'  '.    
%   ::    :;                 ;:    ::   
%   ;      :;.             ,;:     ::   
%   :;      :;:           ,;"      ::   
%   ::.      ':;  ..,.;  ;:'     ,.;:   
%    "'"...   '::,::::: ;:   .;.;""'    
%        '"""....;:::::;,;.;"""         
%    .:::.....'"':::::::'",...;::::;.   
%   ;:' '""'"";.,;:::::;.'""""""  ':;   
%  ::'         ;::;:::;::..         :;  
% ::         ,;:::::::::::;:..       :: 
% ;'     ,;;:;::::::::::::::;";..    ':.
%::     ;:"  ::::::"""'::::::  ":     ::
% :.    ::   ::::::;  :::::::   :     ; 
%  ;    ::   :::::::  :::::::   :    ;  
%   '   ::   ::::::....:::::'  ,:   '   
%    '  ::    :::::::::::::"   ::       
%       ::     ':::::::::"'    ::       
%       ':       """""""'      ::       
%        ::                   ;:        
%        ':;                 ;:"        
%-hrr-     ';              ,;'          
%            "'           '"            

% Grafo arana, grafo arana, al mundo salva con su telarana... (8)
% Grafo arana, grafo arana, al mundo salva con su telarana... (8)
% Grafo arana, grafo arana, al mundo salva con su telarana... (8)
edge(g1,a,b).
edge(g1,b,c).
edge(g1,a,c).
edge(g1,d,a).
edge(g1,d,b).
edge(g1,d,c).

edge(g2,a,b).
edge(g2,a,c).
edge(g2,d,a).
edge(g2,e,a).

edge(g3,f,g).
edge(g3,g,i).
edge(g3,f,i).
edge(g3,h,i).
edge(g3,h,f).
edge(g3,a,h).
edge(g3,a,b).
edge(g3,b,c).
edge(g3,c,g).
edge(g3,d,c).
edge(g3,b,d).
edge(g3,d,f).
edge(g3,a,e).
edge(g3,a,j).
edge(g3,j,e).
edge(g3,a,l).
edge(g3,k,j).
edge(g3,l,k).
edge(g3,l,o).
edge(g3,m,l).
edge(g3,n,l).

edge(g4,a,b).
edge(g4,b,c).
edge(g4,c,d).
edge(g4,d,a).
edge(g4,a,c).
edge(g4,d,b).

edge(g5,a,b).
edge(g5,b,c).
edge(g5,c,d).
edge(g5,d,e).
edge(g5,e,a).
edge(g5,a,c).
edge(g5,a,d).
edge(g5,b,d).
edge(g5,b,e).
edge(g5,c,e).

% Elimina un elemento de una lista.
remv(_, [], []).
remv(X, [X|T], T1) :- remv(X, T, T1).
remv(X, [H|T], [H|T1])  :- X \= H, remv(X, T, T1).

% Quitar elemntos repetidos de una lista..
quitarRepetidos([], []) :- !.
quitarRepetidos([H|T], [H|T1]) :- remv(H, T, T2), 
				quitarRepetidos(T2, T1), !.

resta(R,[],R).
resta(L,[X|Xs],R) :- remv(X,L,R1),resta(R1,Xs,R).

desmarcar([],_).
desmarcar([X|Resto],Rpr) :- retract(ya(Rpr,X)),desmarcar(Resto,Rpr).

% Dado un grafo, devuelve los nodos que lo componen.
listNodes(Grafo, X) :-  findall(Nodo, edge(Grafo, Nodo, _), X1),
						findall(Nodo, edge(Grafo, _, Nodo), X2),
						append(X1, X2, X3),
						quitarRepetidos(X3,X).

buscarHecho(Grafo, H, edge(Grafo,H,X), X) :- edge(Grafo,H,X).  
buscarHecho(Grafo, H, edge(Grafo,X,H), X) :- edge(Grafo,X,H).

listar_aristas(Grafo,Aristas) :- findall(edge(Grafo,A,B),edge(Grafo,A,B),Aristas).

listar_en_izquierda(Grafo,Nodo,Aristas) :- findall(edge(Grafo,Nodo,B),edge(Grafo,Nodo,B),Aristas).
listar_en_derecha(Grafo,Nodo,Aristas) :- findall(edge(Grafo,A,Nodo),edge(Grafo,A,Nodo),Aristas).

remember(X,[X|L],L).
remember(X,[_|L],R):- remember(X,L,R).

%OJO que se puede optimizar sacando listar_aristas, y utilizando asserts y recordar los retract adecuados.
%UTILIZAR GRADOS PARA DISTINGURILOS.
arbol_cobertura(Grafo,Arbol) :- listar_aristas(Grafo,Aristas),
				listNodes(Grafo,Nodos),
				length(Nodos,N),
				member(Arista,Aristas),
				remv(Arista,Aristas,Resto),
				armar(N,[Arista],Arbol,Resto).

arbol_cobertura(N,Aristas,Llevo,Arbol) :-remember(Arista,Aristas,Reto),
					valido(Arista,Llevo),
					%arbol_cobertura(N,Aristas,[Arista|Llevo],Arbol).
					armar(N,[Arista|Llevo],Arbol,Resto).
arbol_cobertura(N,Ar,A,A):-
	length(A,Arb),
	N1 is N -1,
	Arb = N1.

armar(N,Arbol,Arbol,_) :-
	length(Arbol,Arb),
	N1 is N - 1,
	Arb = N1,!.

armar(N,Arbol,ArbolExpandido,Aristas) :-
	length(Aristas,Ar),
	N1 is N - 1,	
	length(Arbol,Arb),
	Posibles is Arb + Ar,
	Posibles >= N1,
	expande(Arbol,Ar2,Aristas,Resto),
	arbol_cobertura(N,Resto,Ar2,ArbolExpandido).

%ojo con remember
expande(Arbol,[Edge|Arbol],Aristas,Resto) :- 
	member(Edge,Aristas),
	valido(Edge,Arbol),
	remv(Edge,Aristas,Resto).

valido(edge(_,A,B),Arbol) :- pertenece(A,Arbol), \+(pertenece(B,Arbol)). %puede que pueda haber un cut aqui
valido(edge(_,A,B),Arbol) :- \+(pertenece(A,Arbol)), pertenece(B,Arbol).

pertenece(Nodo,[edge(_,Nodo,_)|_]):-!.%OJO CON EL GRAFO, si el arbol existe entonces el grafo ya esta
pertenece(Nodo,[edge(_,_,Nodo)|_]):-!.%OJO CON EL GRAFO, si el arbol existe entonces el grafo ya esta
pertenece(Nodo,[_|Resto]):-pertenece(Nodo,Resto).

spider(Grafo,Arbol) :- arbol_cobertura(Grafo,Arbol), aranha(Grafo,Arbol).
%spider(Grafo,_) :- retractall(span(Grafo,S)).

aranha(Grafo,Span) :- listNodes(Grafo,Nodos), grados(Nodos,Span,Grados),
					  check_grados(Grados,ok).

check_grados([],ok).
check_grados([],ya).
check_grados([Grado|Grados],ok) :- Grado < 3, ! , check_grados(Grados,ok).
check_grados([Grado|Grados],ya) :- Grado < 3, ! , check_grados(Grados,ya).
check_grados([Grado|Grados],ok) :- Grado >= 3, check_grados(Grados,ya).

grados([],_,[]).
grados([Nodo|Nodos],Arbol,[G|Grados]) :- grado(Nodo,Arbol,G),grados(Nodos,Arbol,Grados).

grado(Nodo,Arbol,Grado) :- grado(0,Nodo,Arbol,Grado).
grado(G,_,[],G).
grado(Acc,Nodo,[edge(_,Nodo,_)|Resto],Grado) :- G is Acc +1, grado(G,Nodo,Resto,Grado).
grado(Acc,Nodo,[edge(_,_,Nodo)|Resto],Grado) :- G is Acc +1, grado(G,Nodo,Resto,Grado).
grado(Acc,Nodo,[edge(_,A,B)|Resto],Grado) :- Nodo\= A, Nodo \= B, grado(Acc,Nodo,Resto,Grado).


gencoberts(Grafo) :- arbol_cobertura(Grafo,Arbol),asserta(span(Arbol)).

cubremeste(L):- findall(span(A),span(A),L),retractall(span(S)).
