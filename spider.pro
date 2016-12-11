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
edge(g1,d,a).
edge(g1,d,b).
edge(g1,d,c).
edge(g1,a,b).
edge(g1,b,c).
edge(g1,a,c).

edge(g2,a,b).
edge(g2,b,c).
edge(g2,a,c).

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

% Hallar un camino que pase por todos los vertices
genTree(Grafo, [], _,[]).
genTree(Grafo, L, E, X) :- 	buscarHecho(Grafo, E, Hecho, E1),
							member(E1,L), 
							remv(E1,L,Ls1),
							genTree(Grafo, Ls1, E1, X1),
							append([Hecho],X1,X).

genTree1(Grafo, L, E, X) :- buscarHecho(Grafo, E, Hecho, E1),
							member(E1,L),
							remv(E1,L,Ls),
							genTree1(Grafo, Ls, E, X1),
							append([Hecho],X1,X).
genTree1(Grafo, L, E, X) :- genTree(Grafo, L, E, X).


prueba(Grafo,X) :-  listNodes(Grafo, Nodos), 
					member(Node,Nodos),
					remv(Node,Nodos,Nodes),
					genTree1(Grafo, Nodes, Node, X).

check(Grafo,X,Y) :- findall(Arbol,prueba(Grafo,Arbol),Arboles), length(Arboles,X), 
					quitarRepetidos(Arboles,Rept), length(Rept,Y).



