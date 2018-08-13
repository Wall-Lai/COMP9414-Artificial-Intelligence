% ##############################COMP9414-Assignment3###############################
% UCS (Code from Openlearning)
solve(Start, Solution, G, N)  :-
    ucsdijkstra([[Start,Start,0]], [], Solution, G, 1, N).
:- dynamic s/3.
ucsdijkstra([[Node,Pred,G]|_Generated], Expanded, Path, G, N, N)  :-
    goal(Node),
    build_path([[Node,Pred]|Expanded], Path).
ucsdijkstra([[Node,Pred,G]| Generated], Expanded, Solution, G1, L, N) :-
    extend(Node, G, Expanded, NewLegs),
    M is L + 1,
    insert_legs(Generated, NewLegs, Generated1),
    ucsdijkstra(Generated1, [[Node,Pred]|Expanded], Solution, G1, M, N).
extend(Node, G, Expanded, NewLegs) :-
    findall([NewNode, Node, G1], (s(Node, NewNode, C)
    , not(head_member(NewNode, Expanded))
    , G1 is G + C
    ), NewLegs).
insert_one_leg([], Leg, [Leg]).
insert_one_leg([Leg1|Generated], Leg, [Leg1|Generated]) :-
    Leg  = [Node,_Pred, G ],
    Leg1 = [Node,_Pred1,G1],
    G >= G1, ! .
insert_one_leg([Leg1|Generated], Leg, [Leg,Leg1|Generated]) :-
    Leg  = [_Node, _Pred, G ],
    Leg1 = [_Node1,_Pred1,G1],
    G < G1, ! .
insert_one_leg([Leg1|Generated], Leg, [Leg1|Generated1]) :-
    insert_one_leg(Generated, Leg, Generated1).
% Path search (Code from Openlearning)
insert_legs(Generated, [], Generated).
insert_legs(Generated, [Leg|Legs], Generated2) :-
   insert_one_leg(Generated, Leg, Generated1),
   insert_legs(Generated1, Legs, Generated2).
head_member(Node,[[Node,_]|_]).
head_member(Node,[_|Tail]) :-
  head_member(Node,Tail).
build_path([[Next,Start],[Start,Start]], [Next,Start]).
build_path([[C,B],[B,A]|Expanded],[C,B,A|Path]) :-
   build_path([[B,A]|Expanded],[B,A|Path]), ! .
build_path([Leg,_SkipLeg|Expanded],Path) :-
   build_path([Leg|Expanded],Path).


% ###################################QUESTION1###################################
% Write a Prolog procedure initial_intentions(Intentions) which binds Intentions 
% to intents(L,[]).

% There are two rules for s().
% s(_goal,_goal,1) is used to deal with the land or dropped.
% s(_goal,_goal,1) is used to deal with the water.
s(goal(X1,Y1),goal(X2,Y2),1):-
	land_or_dropped(X2,Y2),
	distance((X1,Y1),(X2,Y2),1).
s(goal(Xc,Yc),goal(Xn,Yn),999):-		
	Xcl is Xc - 1,
	Xcr is Xc + 1,
	findbt(Xcl, Xcr, Xn),
	Ycl is Yc - 1,
	Ycr is Yc + 1, 
	findbt(Ycl, Ycr, Yn),
	distance((Xc,Yc),(Xn,Yn),1),
	not(land_or_dropped(Xn,Yn)).
findbt(X, Y, BT) :- 
	X =< Y,
	BT = X.
findbt(X, Y, BT) :- 
	X < Y,
	XX is X + 1,
	findbt(XX, Y, BT).
% Get goal().
get_goal(goal(X,Y),Xa,Ya):-
	X is Xa,
	Y is Ya.
initial_intentions(intents(L,[])):-
	agent_at(Xa,Ya),
	monster(Xm,Ym),
	get_goal(OlGoal,Xa,Ya),
	assert(goal(OlGoal)),
	solve(goal(Xm,Ym),Path, _G ,_N),
	findL(Path,L),
	retract(goal(OlGoal)).
% Get the Paths and delete the path of distance greater than 999
findL([],[]).
findL(OrlPath,L):-
	findall(X, (X = [goal(Ga, Gb),[]], member(goal(Ga, Gb), OrlPath), not(land(Ga, Gb))), L),
	retractall(s(_,_,999)).


% ###################################QUESTION2###################################
% Write a Prolog procedure trigger(Percepts, Goals) which takes a list of percepts,
% each of the form stone(X,Y), and converts it into a corresponding list of goals, 
% each of the form goal(X,Y). 

% Base case
trigger([], []).
trigger([stone(X, Y)|Stones], [goal(X, Y)|Goals]) :-
   trigger(Stones, Goals).


% ###################################QUESTION3###################################
% Write a Prolog procedure incorporate_goals(Goals, Intentions, Intentions1) which
% return the updated Intentions of the agent.
% The procedure has two inputs:
% a set of Goals([goal(X1,Y1)]...)
% the current Intentions of the agent(Int_drop,Int_pick).eg: the form of Int_drop 
% is [goal(X,y),plan].

% use goal(X,Y) to get its two coordinates.
get_goal_x_y(goal(X,Y),Xg,Yg):-
	Xg is X,
	Yg is Y.
% Check whether there is a current goal in intentions.
is_member(Goal, [Head|_]) :-
   	member(Goal, Head).
is_member(Goal, [Head|Tail]) :-
    	not(member(Goal, Head)),
	is_member(Goal, Tail).
% Base case.
incorporate_goals([],Same,Same).
% Current goal is in the Intentions.
incorporate_goals([Goal|Tail],Intentions,Intentions1):-
	is_member(Goal, Intentions),
	retractall(s(_,_,999)),
	incorporate_goals(Tail,Intentions,Intentions1).
% The goal is not in Intentions. The Intentions list needs to insert a this goal.
incorporate_goals([Goal|Tail],Intentions,Intentions1):-
	not(is_member(Goal, Intentions)),
	agent_at(Xa,Ya),
	get_goal(OlGoal,Xa,Ya),
	assert(goal(OlGoal)),
	calculate_goal(Goal,Intentions,UpdIntentions),
	retract(goal(OlGoal)),
	retractall(s(_,_,999)),
	incorporate_goals(Tail,UpdIntentions,Intentions1).
% Calculate the distance between the current location and the current goal,
% and the result Dis1 is entered insert_goal() as a parameter.
calculate_goal(Goal,intents(Same1,Same2),intents(Same1,Same2)):-
	not(solve(Goal,_Path,_G,_N)).
calculate_goal(Goal,intents(Same,PickPart),intents(Same,PickPart1)):-
	get_goal_x_y(Goal,Xg,Yg),
	agent_at(Xa,Ya),
	distance((Xa,Ya),(Xg,Yg),Dis1),
	insert_goal(Goal,Dis1,PickPart,PickPart1).
% Compare the distances:
% If Dis1 is greater than or equal to Dis2, the current goal will be inserted.
% If Dis1 is less than Dis2, the procedure does not do anything.
insert_goal(Same,_Dis1,[],[[Same,[]]]).
insert_goal(Same1,_Dis1,[[Same1,Same2]|Same3],[[Same1,Same2]|Same3]).
insert_goal(Goal,Dis1,[[PkGoal,Same]|Same1],[[Goal,[]],[PkGoal,Same]|Same1]):-
	get_goal_x_y(PkGoal,Xg,Yg),
	agent_at(Xa,Ya),
	distance((Xa,Ya),(Xg,Yg),Dis2),
	Dis1<Dis2.
insert_goal(Goal,Dis1,[[PkGoal,Same]|Tail],[[PkGoal,Same]|Tail1]):-
	get_goal_x_y(PkGoal,Xg,Yg),
	agent_at(Xa,Y0),
	distance((Xa,Y0),(Xg,Yg),Dis2),
	Dis1>=Dis2,
	insert_goal(Goal,Dis1,Tail,Tail1).


% ###################################QUESTION4###################################
% Write a Prolog procedure get_action(Intentions, Intentions1, Action) which
% takes the agent's current Intentions and calculates an action.

% Firstly check whether the current agent holds a stone.
% IF the agent is currently holding a stone, its next step will be to drop the stone.
% Otherwise, the agent use select_action_pick() procedure to pick a stone.
get_action(Intentions,Intentions1,Action):-
	agent_stones(X),
(	X =:= 1 ->
	select_action_drop(Intentions,Intentions1,Action)
; 
	select_action_pick(Intentions,Intentions1,Action)
).
select_action_drop(intents(DropPart,Same),intents(DropPart1,Same),Action):-
	agent_at(Xa,Ya),
	get_goal(OlGoal,Xa,Ya),
	assert(goal(OlGoal)),
	DP is 1,
	get_action_dp(DP,DropPart,DropPart1,Action),
	retract(goal(OlGoal)).
select_action_pick(intents(Same,PickPart),intents(Same,PickPart1),Action):-
	agent_at(Xa,Ya),
	get_goal(OlGoal,Xa,Ya),
	assert(goal(OlGoal)),
	DP is 0,
	get_action_dp(DP,PickPart,PickPart1,Action),
	retract(goal(OlGoal)).
% Base case
get_action_dp(_DP,[],[],move(Xa,Ya)):-agent_at(Xa,Ya).
% If the first action is applicable, the plan will be updated.
get_action_dp(_DP,[[goal(X,Y),[Plan1|Same]]|Tail],[[goal(X,Y),Same]|Tail],Plan1):-
	applicable(Plan1).
% If there are no related plans, new plans will be added.
get_action_dp(DP,[[goal(X,Y),[]]|Tail],[[goal(X,Y),NewPlan]|Tail],Action):-
	solve(goal(X,Y),Path,_G,_N),
	append([_|OrgPaths],[_],Path),
	get_move(OrgPlan, OrgPaths),
(	DP =:=1 ->
	append(OrgPlan,[drop(X,Y)],[Action|NewPlan])
;	
	append(OrgPlan,[pick(X,Y)],[Action|NewPlan])
).
% If the first action is not applicable, agent will build a new plan.
get_action_dp(DP,[[goal(X,Y),[Plan1|_PTail]]|Tail],[[goal(X,Y),NewPlan]|Tail],Action):-
	not(applicable(Plan1)),	
	solve(goal(X,Y),Path,_G,_N),
	append([_|OrgPaths],[_],Path),
	get_move(OrgPlan, OrgPaths),
(	DP =:=1 ->
	append(OrgPlan,[drop(X,Y)],[Action|NewPlan])
;	
	append(OrgPlan,[pick(X,Y)],[Action|NewPlan])
).
% get move(x,y)
get_move([], []).
get_move([move(X, Y)|Moves], [goal(X, Y)|Goals]) :-
   trigger(Moves, Goals).


% ###################################QUESTION5###################################
% Write a Prolog procedure update_intentions(Observation, Intentions, Intentions1) which
% updates the agent's intentions.

% In the case of at() observation, there are no changes for agent's intentions.
update_intentions(at(_X,_Y),Same,Same).
% In the case of a dropped() observation, the agent removes the corresponding plan. 
update_intentions(dropped(_Xd,_Yd),intents([_|DropParts],PickPart),intents(DropParts,PickPart)).
% In the case of a picked() observation. 
update_intentions(picked(_Xp,_Yp),intents(DropPart,[_|PickParts]),intents(DropPart,PickParts)).
