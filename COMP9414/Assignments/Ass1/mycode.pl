% COMP9414 Assignment 1 - Prolog Programming
%StudentID:z5142340

% ----------------------------------------------------------------- %
%Question1:
  %sums the squares of only the negative numbers in a list of number

%Function sumsq_neg: return the sums of the squares of negative numbers
sumsq_neg([], 0).

sumsq_neg([H | T], Sum):-
  H < 0,
  sumsq_neg(T, Result),
  Sum is H * H + Result.

sumsq_neg([H | T], Sum):-
  not(H < 0),
  sumsq_neg(T, Sum).


% ----------------------------------------------------------------- %
%Question2:
  % succeeds if every person in Who_List likes every item in What_List

%base case 
%either Who_List or What_List is empty
all_like_all([], _).
all_like_all(_, []).

all_like_all(Who_List, What_List):-
  [Person | Other_Person] = Who_List,
  [Fruit | Other_Fruit] = What_List,
  person_like_all(Person, [Fruit | Other_Fruit]),
  all_like_all(Other_Person, [Fruit | Other_Fruit]).

%base case 
person_like_all(_, []).
%check person's likes
person_like_all(Person, [Fruit | Other_Fruit]):-
  likes(Person, Fruit),
  person_like_all(Person, Other_Fruit).

% ----------------------------------------------------------------- %
%Question3:
  %Write a predicate sqrt_table(N, M, Result) that binds Result to the list of pairs consisting of a number and its square root, from N down to M, where N and M are non-negative integers, and N >= M.
%When M<N, this function returns the squeares of N
sqrt_table(N, M, Result):-
  N>M,
  N>0,
  M>0,
  SqrtN is sqrt(N),
  NN is N - 1,
  sqrt_table(NN, M, NResult),
  Result =  [[N,SqrtN] | NResult].
%When M=N, this function returns the squeares of N 
sqrt_table(N, M, Result):-
  M=N,
  N>0,
  M>0,
  SqrtN is sqrt(N),
  Result = [[N,SqrtN]].

% ----------------------------------------------------------------- %
%Question4:
  %Write a predicate chop_up(List, NewList) that takes List and binds NewList to List with all sequences of successive increasing whole numbers replaced by a two-item list containing only the first and last number in the sequence. 
%Base case
%The last element in list
chop_up([H|[]],[H|[]]).
%No elements in list
chop_up([],[]).

%Element is not consequent 
chop_up(List, NewList):-
  [H | T] = List,
  [NH | _NT] = T,
  Temp is H+1,
  not(NH = Temp),
  First is H,
  chop_up(T, SubList),
  NewList = [First | SubList].
%Element is not consequent 
chop_up(List, NewList):-
  [H | T] = List,
  [NH | _NT] = T,
  Temp is H+1,
  NH = Temp,
  find_last(T, Final, NextL),
  First is H,
  chop_up(NextL, SubList),
  NewList = [[First, Final] | SubList].

%directly return the last elements of the list
find_last([H|[]], Final, NextL):-
  NextL=[],
  Final is H.
%check continuous figures
find_last([H|T], Final, NextL):-
  [NH | _NT] = T,
  Temp is H+1,
  NH=Temp,
  find_last(T, Final, NextL).
%return the last number,and return rest list
find_last([H|T], Final, NextL):-
  [NH | _NT] = T,
  Temp is H+1,
  not(NH = Temp),
  NextL = T,
  Final is H.
% ----------------------------------------------------------------- %
%Question5:
  %Write a predicate tree_eval(Value, Tree, Eval) that binds Eval to the result of evaluating the expression-tree Tree, with the variable z set equal to the specified Value.
%base case: final branch is z
tree_eval(Value, tree(empty, z, empty), Eval):-
Eval = Value.
%base case: final branch is a number
tree_eval(_Value, tree(empty, Num, empty), Eval):-
number(Num), %pass if it is a number
Eval = Num.

%there are 4 kinds of case:'+','-','*'and'/'
  %for example: calculate '+' case
  %Eval is LE +RE. LE is the Eval of the left subtree. 
tree_eval(Value, tree(L, Op, R), Eval):-
Op = '+',
tree_eval(Value, L, LE),
tree_eval(Value, R, RE),
Eval is LE + RE .

tree_eval(Value,tree(L, Op, R), Eval):-
Op = '-',
tree_eval(Value, L, LE),
tree_eval(Value, R, RE),
Eval is LE - RE .

tree_eval(Value, tree(L, Op, R), Eval):-
Op = '*',
tree_eval(Value, L, LE),
tree_eval(Value, R, RE),
Eval is LE * RE .

tree_eval(Value, tree(L, Op, R), Eval):-
Op = '/',
tree_eval(Value, L, LE),
tree_eval(Value, R, RE),
Eval is LE / RE .
