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

% Elimina un elemento de una lista.
remv(_, [], []).
remv(X, [X|T], T1) 		:- remv(X, T, T1).
remv(X, [H|T], [H|T1])  :- X \= H, remv(X, T, T1).

% Quitar elemntos repetidos de una lista..
quitarRepetidos([], []) :- !.
quitarRepetidos([H|T], [H|T1]) :- 	remv(H, T, T2), 
									quitarRepetidos(T2, T1), !.

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

%OJO que se puede optimizar sacando listar_aristas, y utilizando asserts y recordar los retract adecuados.
arbol_cobertura(Grafo,Arbol) :- listar_aristas(Grafo,Aristas),
								member(Arista,Aristas),
								armar([Arista],Arbol,Aristas).

armar(Arbol,Arbol,Aristas):- \+(expande(Arbol,_,Aristas)),!. %verificar si esto se puede reemplazar sin el lado derecho
armar(Arbol,ArbolExpandido,Aristas) :- expande(Arbol,Ar2,Aristas),
										 armar(Ar2,ArbolExpandido,Aristas).

expande(Arbol,[Edge|Arbol],Aristas) :- member(Edge,Aristas), valido(Edge,Arbol).

valido(edge(_,A,B),Arbol) :- pertenece(A,Arbol), \+(pertenece(B,Arbol)). %puede que pueda haber un cut aqui
valido(edge(_,A,B),Arbol) :- \+(pertenece(A,Arbol)), pertenece(B,Arbol).

pertenece(Nodo,[edge(_,Nodo,_)|_]):-!.%OJO CON EL GRAFO, si el arbol existe entonces el grafo ya esta
pertenece(Nodo,[edge(_,_,Nodo)|_]):-!.%OJO CON EL GRAFO, si el arbol existe entonces el grafo ya esta
pertenece(Nodo,[_|Resto]):-pertenece(Nodo,Resto).

spider(Grafo,Arbol) :- arbol_cobertura(Grafo,Arbol), aranha(Grafo,Arbol).

aranha(Grafo,Span) :- listNodes(Grafo,Nodos), grados(Nodos,Span,Grados),
					  check_grados(Grados,ok).

check_grados([],ok).
check_grados([],ya).
check_grados([Grado|Grados],ok) :- Grado < 3, ! , check_grados(Grados,ok).
check_grados([Grado|Grados],ya) :- Grado < 3, ! , check_grados(Grados,ya).
check_grados([Grado|Grados],ok) :- Grado = 3, check_grados(Grados,ya).

grados([],_,[]).
grados([Nodo|Nodos],Arbol,[G|Grados]) :- grado(Nodo,Arbol,G),
										 grados(Nodos,Arbol,Grados).

grado(Nodo,Arbol,Grado) :- grado(0,Nodo,Arbol,Grado).
grado(G,_,[],G).
grado(Acc,Nodo,[edge(_,Nodo,_)|Resto],Grado) :- G is Acc +1, grado(G,Nodo,Resto,Grado).
grado(Acc,Nodo,[edge(_,_,Nodo)|Resto],Grado) :- G is Acc +1, grado(G,Nodo,Resto,Grado).
grado(Acc,Nodo,[edge(_,A,B)|Resto],Grado) :- Nodo\= A, Nodo \= B, grado(Acc,Nodo,Resto,Grado).
