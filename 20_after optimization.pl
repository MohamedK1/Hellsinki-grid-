grid_build(N,M):-
	gridBuildHelper(N,N,M,[]).

gridBuildHelper(0,_,R,R).
gridBuildHelper(Counter,N,Result,Acc):-
     Counter>0,
	 length(L,N),
	 Counter1 is Counter-1,
	 gridBuildHelper(Counter1,N,Result,[L|Acc]).
	 
%checkSmallerN(L,N) is true if all the elements in L is smaller than or equal to N 
checkSmallerN([],_).
checkSmallerN([H|T],N):-H=<N,checkSmallerN(T,N).

num_gen(L,L,[L]).
num_gen(F,L,[F|R1]):-F<L, F1 is F+1 ,num_gen(F1,L,R1).

% we need to check what we need from empty lists
checkNumList(L):-
   max_list(L,R),num_gen(1,R,X),intersection(X,L,X).

%this is another one to for check_num_grid(G).
is_subset([],_).
is_subset([H|T],L):-member(H,L),is_subset(T,L).

flatten([],[]).
flatten([H|T],R):- is_list(H),flatten(H,R1),flatten(T,R2),append(R1,R2,R).
flatten([H|T],[H|R]):- \+is_list(H),flatten(T,R).

check_num_grid(L):-flatten(L,FlattenedL),max_list(FlattenedL,Max),length(L,Length),Max=<Length,num_gen(1,Max,R),is_subset(R,FlattenedL),!.

% getNelement(N,L,R) is true  if R is the Nth element in L
getNelement(1,[H|_],H).
getNelement(N,[_|T],R):-N>1,N1 is N-1,getNelement(N1,T,R).

% getColoumn or Row (Number of R Or C,Grid,Result) is true if Result is the Nth coloumn or row of the Grid.
getColoumn(_,[],[]).
getColoumn(N,[H|T],[R|R1]):- getNelement(N,H,R),getColoumn(N,T,R1).

getRow(1,[H|_],H).
getRow(N,[_|T],R):-N>1,N1 is N-1,getRow(N1,T,R).

acceptable_distribution(G):-
                          length(G,Max),
						  Max>=1,
						  acceptable_distribution(1,Max,G).

acceptable_distribution(Start,Max,_):-Start>Max.
acceptable_distribution(Start,Max,G):-
                           Start=<Max,
						   getColoumn(Start,G,C),
						   getRow(Start,G,R),
						   R\=C,
						   Start1 is Start+1,
						   acceptable_distribution(Start1,Max,G).

%trans (G,Result) succceed if Result is the transpose of G
trans(G,Result):-length(G,Max),trans(1,Max,G,Result,[]).						   

%trans (Counter,Max,Grid,Result,Acc) succeed when Acc=[] and Result is the transpose of Grid and Max is passed as the length of G(no of rows)					   
trans(_,_,[],[],_).
trans(Counter,Max,G,R,Acc):-Max>=Counter,
		getColoumn(Max,G,Coloumn),
		Max1 is Max-1,
		trans(Counter,Max1,G,R,[Coloumn|Acc]).

trans(Counter,Max,_,Acc,Acc):-Max<Counter.

distinct_rows([]).
distinct_rows([H|T]):- \+member(H,T),distinct_rows(T).

distinct_columns(G):- trans(G,R),distinct_rows(R).

%genListUpdated R is a list containg all possible lists from 1 to N with their permutations
genListUpdated(N,R):-num_gen(1,N,L),length(R,N),genList2(L,R).

genList2(L,R):-R=[H|T],member(H,L),genList2(L,T).
genList2(_,[]).

%listOfGenList(L,N) succeed of L is a List containing all lists with all possible combinations for numbers form 1 to N

listOfGenList(L,N):- setof(X,genListUpdated(N,X),L).

%permuteList(L,R) succeed if L is List of Lists
%then R is a list containing all possible combinations(without repetition) of every list in L 

permuateList([],[]).
permuateList([H|T],Result):- setof(X,permutation(H,X),HResult),permuateList(T,TResult),append(HResult,TResult,Result).

% a predicate to combine listOfGenList and permuteList in one Call
%succeed when  L is a list that contains lists for all possible selection and arrangement of numbers from 1 to N
 
getAllPossibleList(N,L):- listOfGenList(L,N).

% gridGen2 takes M as a Grid and bind every row in it with an element form L 
gridGen2([],_).
gridGen2([H|T],L):- member(H,L),gridGen2(T,L).

%checkAllRowsColoums(M,TM) takes a matrix M and its transpose TM to check that every row in M is row in TM which implies all rows are coloumns

checkAllRowsColoumns([],_). 
checkAllRowsColoumns([H|T],TM):- member(H,TM),checkAllRowsColoumns(T,TM).

checkAllRowsColoumns(M):- trans(M,TM),checkAllRowsColoumns(M,TM). 

%row_col_match(G) succeed if each row is a coloumn with different numbers

row_col_match(G):- trans(G,TG),length(G,Length),row_col_match(1,Length,G,TG).

row_col_match(Start,Max,_,_):-Start>Max.
row_col_match(Start,Max,G,TG):-
                           Start=<Max,
						   getRow(Start,TG,C),
						   getRow(Start,G,R),
						   R\=C,
						   member(R,TG),
						   Start1 is Start+1,
						   row_col_match(Start1,Max,G,TG).
						   
notCorresponding([],[]).
notCorresponding([H1|T1],[H2|T2]):-H1\=H2,notCorresponding(T1,T2).

acceptable_permutation(L,R):-permutation(L,R),notCorresponding(L,R).

%getPost(L,Target,R) succeed if R is the postion of Target in the List L
getPos([],_,0).
getPos([Target|_],Target,1).
getPos([H|T],Target,R):- H\=Target,getPos(T,Target,R1),R is R1+1.

%getPosList(L,R) succeed if R is a list of postions of the numbers from 1 to N in the List L
getPosList(_,Start,N,[]):-Start>N.
getPosList(L,Start,N,[R|Remain]):-Start=<N,getPos(L,Start,R),Start1 is Start+1, getPosList(L,Start1,N,Remain).

getPosList(L,R):- length(L,N),getPosList(L,1,N,R).

bindRowsColoumns([],_,[]).
bindRowsColoumns([H|T],TG,[HPos|TPos]):- getNelement(HPos,TG,Coloumn),H=Coloumn,bindRowsColoumns(T,TG,TPos).

% it takes N and M and Binds the rows and coloumns of M to each other
finalBinding(N,M):- trans(M,TM),num_gen(1,N,RowsOrder),acceptable_permutation(RowsOrder,ColoumnsOrder),
						getPosList(ColoumnsOrder,Pos),bindRowsColoumns(M,TM,Pos).

helsinki(N,M):- grid_build(N,M),finalBinding(N,M),getAllPossibleList(N,L),gridGen2(M,L),distinct_rows(M),check_num_grid(M).




