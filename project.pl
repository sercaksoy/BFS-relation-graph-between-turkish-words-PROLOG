:-include('isimler.pl').
:-include('fiiller.pl'). % Database pl files loaded.

% que(node)
% queue for the BFS
% algoirthm.

% visited(Parent,Relation,Node)
% to trace back from goal
% recording each node with its
% parent and relation.

% counter(Size)
% This measures the
% length of the path.

% lengths()
% - X to Y
% - X to Z
% - Y to Z
	
found(Concept1,Relation,Concept2) :- iliski(Concept1,Relation,Concept2).
found(Concept1,Relation,Concept2) :- iliski(Concept2,Relation,Concept1). % Undirected graph structure.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bfs(Start,Goal):-					  % Classical bfs implementation ;
	assert(que(Start)),				  % first node added to queue,
	assert(visited(root,root,Start)), % also it visited,
	not(bfs_loop(Start,Goal)),		  % while loop among the queue,
	not(print_parent_visit(Goal,0)),  % back trace to write path,
	count(T), % T is our length		  % path length,
	assert(lengths(T)),				  % recording each length,
	not(clear_counter()),nl,          
	clear_program().				  % clear program to prepare next run.

bfs_loop(Start,Goal):-
	que(_),							  % if queue is not empty,
	not(visited(_,_,Goal)),			  % if node has not visited,
	retract(que(X)), % poll from que  % pop the node from queue,
	bfs_sub_loop(X),				  % loop for each linked nodes,
	bfs_loop(Start,Goal).			  


bfs_sub_loop(X):-
	found(X,Relation,Y),			  % find links,
	not(visited(_,_,Y)),			  % which have not visited yet,
	assert(que(Y)),					  % enqueue,
	assert(visited(X,Relation,Y)),    % record the path,
	fail.

bfs_sub_loop(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

r_bfs(Start,Goal,RelationList):-
	assert(que(Start)),
	assert(visited(root,root,Start)),
	not(r_bfs_loop(Start,Goal,RelationList)),
	not(print_parent_visit(Goal,0)),
	count(T), % T is our length
	assert(lengths(T)),
	not(clear_counter()),nl,
	clear_program().

r_bfs_loop(Start,Goal,RelationList):-
	que(_),
	not(visited(_,_,Goal)),
	retract(que(X)), % poll from que
	r_bfs_sub_loop(X,RelationList),
	r_bfs_loop(Start,Goal,RelationList).


r_bfs_sub_loop(X,RelationList):-
	found(X,Relation,Y),				  % Specified relations are allowed
	not(visited(_,_,Y)),
	member(Relation,RelationList),		  % occured relation has to be a member,
	assert(que(Y)),
	assert(visited(X,Relation,Y)),
	fail.

r_bfs_sub_loop(_,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear_program():-
	retract(que(_)),fail.
clear_program():-
	retract(visited(_,_,_)),fail.
clear_counter():-
	retract(counter(_)),fail.

count(T):-
	retract(counter(X)),
	T is X-1,!.	
		
greatest(X,Y,Z,Greatest):-				  % Finding the greatest value between 3 variables.
	X >= Y,
	X >= Z,!,
	Greatest = X.
greatest(X,Y,Z,Greatest):-
	Y >= X,
	Y >= Z,!,
	Greatest = Y.
greatest(_,_,Z,Greatest):-
	Greatest = Z.

total_len():-
	retract(lengths(X)),
	retract(lengths(Y)),
	retract(lengths(Z)),
	greatest(X,Y,Z,Greatest),
	Total is (X + Y + Z - Greatest),	  % Sum of all minus greatest is the shortest path length.
	write('Total length of the path is: '),
	write(Total). 
	
print_list([]).
print_list([Head|Tail]):-
	write(Head),
	write(' '),
	print_list(Tail).

member(Element,[Element| _ ] ).
member(Element,[ _ |Tail] ) :-
	member(Element,Tail).	

print_parent_visit(Goal,K):-
	visited(Parent,Relation,Goal),
	A is K+1,
	asserta(counter(A)),
	Parent \= root,
	write(Parent),
	write(' '),
	write(Relation),
	write(' '),
	write(Goal),nl,
	print_parent_visit(Parent,A).

relation_list(T,List):-
	T > 0,
	K is T-1,
	write('Iliski ismi giriniz:'),
	read(Relation),
	append(List,[Relation],NewList),
	relation_list(K,NewList).

relation_list(0,RelationList):-
	write(RelationList),nl,
	write('kavram 1 :'),nl,
	read(X),
	write('kavram 2 :'),nl,
	read(Y),
	write('kavram 3 :'),nl,
	read(Z),	
	% X to Y
		not(r_bfs(X,Y,RelationList)),
	% X to Z
		not(r_bfs(X,Z,RelationList)),
	% Y to Z
		not(r_bfs(Y,Z,RelationList)),
	total_len().
	

program():- % Only selected relations can be used.
	write('kac adet bag kullanilacak :(hepsi icin 0 giriniz)'),nl,
	read(T),
	(T \= 0),!,
	write('Iliski ismi giriniz:'),
	read(Relation),
	append([],[Relation],RelationList),
	K is T-1,
	relation_list(K,RelationList).
		
program():- % All relations can be used.
	write('kavram 1 :'),nl,
	read(X),
	write('kavram 2 :'),nl,
	read(Y),
	write('kavram 3 :'),nl,
	read(Z),	
	% X to Y
		not(bfs(X,Y)),
	% X to Z
		not(bfs(X,Z)),
	% Y to Z
		not(bfs(Y,Z)),
	total_len().
