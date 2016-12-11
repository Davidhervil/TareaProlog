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

edge(g1,b,a).
edge(g1,b,c).
edge(g1,c,a).

nextElem([],[]).
nextElem([X|Xs],[X|Xs]).
nextElem([X|Xs],R) :- nextElem(Xs,R).

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

% Hallar un camino que pase por todos los vertices
genTree(Grafo, [], _, _,[]):- !.

genTree(Grafo, L, [], E, X) :- 	edge(Grafo, E, E1),
								member(E1,L),
								remv(E1,L,Ls1),
								V1 = [E],
								genTree(Grafo, Ls1, V1, E, X1),
								append([edge(Grafo,E,E1)],X1,X).

genTree(Grafo, L, [], E, X) :- 	edge(Grafo, E1, E),
								member(E1,L),
								remv(E1,L,Ls1),
								V1 = [E],
								genTree(Grafo, Ls1, V1, E, X1),
								append([edge(Grafo,E1,E)],X1,X).

genTree(Grafo, L, V, E, X) 	:- 	
								edge(Grafo, E, E1),
								append(L,V,Ls), 
								member(E1,Ls), 
								remv(E1,L,Ls1),
								genTree(Grafo, Ls1, V, E1, X1),
								append([edge(Grafo,E,E1)],X1,X).
genTree(Grafo, L, V, E, X) 	:- 	
								edge(Grafo, E1, E),
								append(L,V,Ls), 
								member(E1,Ls), 
								remv(E1,L,Ls1),
								genTree(Grafo, Ls1, V, E1, X1),
								append([edge(Grafo,E1,E)],X1,X).












prueba(Grafo,X) :-  listNodes(Grafo, [Node|Nodes]), 
					genTree(Grafo, Nodes, [], Node, X).

check(Grafo,X,Y) :- findall(Arbol,prueba(Grafo,Arbol),Arboles), length(Arboles,X), 
					quitarRepetidos(Arboles,Rept), length(Rept,Y).



