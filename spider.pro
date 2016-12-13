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


not(P) :- call(P), !, fail. 
not(P).

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

arbol_cobertura(Grafo,Arbol) :- listar_aristas(Grafo,Aristas),
								member(Arista,Aristas),
								expande([Arista],Arbol,Aristas).

expande(Arbol,Arbol,Aristas):- \+(unionfind(Arbol,_,Aristas)). %verificar si esto se puede reemplazar sin el lado derecho
expande(Arbol,ArbolExpandido,Aristas) :- unionfind(Arbol,Ar2,Aristas),
										 expande(Ar2,ArbolExpandido,Aristas).

unionfind(Arbol,[Edge|Arbol],Aristas) :- member(Edge,Aristas), valido(Edge,Arbol).

valido(edge(_,A,B),Arbol) :- pertenece(A,Arbol), \+(pertenece(B,Arbol)). %puede que pueda haber un cut aqui
valido(edge(_,A,B),Arbol) :- \+(pertenece(A,Arbol)), pertenece(B,Arbol).

pertenece(Nodo,[edge(_,Nodo,_)|_]):-!.
pertenece(Nodo,[edge(_,_,Nodo)|_]):-!.
pertenece(Nodo,[_|Resto]):-pertenece(Nodo,Resto).