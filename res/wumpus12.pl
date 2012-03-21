%-------------------------------------------------------------------------------
% Wumpus World Simulator
%
% v0.9 - Jan. 31, 2006
%
% A Prolog Implementation of the Wumpus World described in
% Artificial Intelligence : A Modern Approach (Russel - Norvig) 6.2, 7.2, 7.3
%-------------------------------------------------------------------------------


%-------------------------------------------------------------------------------
% Notes:
%
% 1. Scoring
%	-time 		(1 time == 1 step)
%	-agent_dead 		10000
%	+got gold		1000
%	+wumpus_dead		0
%
% 3. Our deggrees levels turn in counter clockwise
%-------------------------------------------------------------------------------


%-------------------------------------------------------------------------------
% The Main Program, Schedule :
%
% "schedule." :
% 	launch a hole evolution until get an agent result : 5 ends are possible
%	you must to refer at the_end(Smile) to get the success degree
%


schedule :-
	initialize_general,
	format("Let the game begin.~n",[]),
	description_total,
	retractall(is_situation(_,_,_,_,_)),
	time(T),agent_location(L),agent_orientation(O),
	assert(is_situation(T,L,O,[],i_know_nothing)),
	format("I'm conquering the World Ah!Ah!...~n",[]),
	Step.

Step :-
	agent_healthy,		% If I'm computing... so I am...
	agent_in_cave,
	
	is_nb_visited,		% count the number of room visited
	
	agent_location(L),
	retractall(is_visited(L)),
	assert(is_visited(L)),
	
	description,		% display a short summary of my state
	
	%----------------------------------
	make_percept_sentence(Percept),	% I percept...
	format("I feel ~p, ",[Percept]),
	%----------------------------------
	tell_KB(Percept),		% I learn...(infer)
	%----------------------------------
	%ask_KB(Percept,Action),		% I think...(compute)
	ask_KB(Action),
	format("I'm doing : ~p~n",[Action]),
	%----------------------------------
	apply(Action),			% I do...
	%----------------------------------
	
	short_goal(SG),		% the goal of my current action 
% format("Short goal~p ~n",[SG]),
	
	time(T),		% Time update 
	New_T is T+1,
	retractall(time(_)),
	assert(time(New_T)),
	
	agent_orientation(O),
	assert(is_situation(New_T,L,O,Percept,SG)),
	% we keep in memory to check :
	% time, agent_location, agent_Orientation,perception, short_goal.
	
	step,
	!.
	
step :-	
	format("the game is finished.~n",[]),	% either dead or out the cave
	
	agent_score(S),
	time(T),
	New_S is S - T,
	retractall(agent_score(_)),
	assert(agent_score(New_S)),
		
	description_total,
	the_end(MARK),
	display(MARK).
	
	
%-------------------------------------------------------------------------------
% A few declarations
%

:- dynamic([
	short_goal/1,
	is_situation/5,			% tool to cheek a "situation"
	time/1,
	nb_visited/1,			% number of room visited
	score_climb_with_gold/1,
	score_grab/1,
	score_wumpus_dead/1,
	score_agent_dead/1,
	land_extent/1,			% size of the land
	wumpus_location/1,
	wumpus_healthy/0,
	gold_location/1,
	pit_location/1,
	agent_location/1,		% state of agent
	agent_orientation/1,
	agent_healthy/0,
	agent_hold/0,
	agent_arrows/1,
	agent_goal/1,
	agent_score/1,
	agent_in_cave/0,		
	is_wumpus/2,		% agent's knowledge about Wumpus_location
	is_pit/2,		% agent's knowledge about pit_location
	is_gold/1,		% agent's knowledge about gold_location
	is_wall/1,		% agent's knowledge about wall_location
	is_dead/0,		% agent's knowledge about Wumpus_health
	is_visited/1]).		% agent's knowledge about room visited
	
% create a map with the initial features described in the section 6.2
initialize_land(fig62):-
	retractall(land_extent(_)),	
	retractall(wumpus_location(_)),
	retractall(wumpus_healthy),
	retractall(gold_location(_)),
	retractall(pit_location(_)),
	assert(land_extent(5)),
	assert(wumpus_location([1,3])),
	assert(wumpus_healthy),
	assert(gold_location([2,3])),
	assert(pit_location([3,1])),
	assert(pit_location([3,3])),
	assert(pit_location([4,4])).
	
% create a map test
initialize_land(test):-
	retractall(land_extent(_)),	
	retractall(wumpus_location(_)),
	retractall(wumpus_healthy),
	retractall(gold_location(_)),
	retractall(pit_location(_)),
	assert(land_extent(5)),
	assert(wumpus_location([3,2])),
	assert(wumpus_healthy),
	assert(gold_location([4,3])),
	assert(pit_location([3,3])),
	assert(pit_location([4,4])),
	assert(pit_location([3,1])).

% create an agent with the initial features described in the section 6.2
initialize_agent(fig62):-	
	retractall(agent_location(_)),
	retractall(agent_orientation(_)),
	retractall(agent_healthy),
	retractall(agent_hold),
	retractall(agent_arrows(_)),
	retractall(agent_goal(_)),
	retractall(agent_score(_)),
	retractall(is_wumpus(_,_)),
	retractall(is_pit(_,_)),
	retractall(is_gold(_)),
	retractall(is_wall(_)),
	retractall(is_dead),
	retractall(is_visited(_)),
	assert(agent_location([1,1])),
	assert(agent_orientation(0)),
	assert(agent_healthy),
	assert(agent_arrows(1)),
	assert(agent_goal(find_out)),
	assert(agent_score(0)),	
	assert(agent_in_cave).
		
% initialization general
initialize_general :-
	initialize_land(test),		% Which map you wish
	initialize_agent(fig62),
	retractall(time(_)),
	assert(time(0)),
	retractall(nb_visited(_)),
	assert(nb_visited(0)),
	retractall(score_agent_dead(_)),
	assert(score_agent_dead(10000)),
	retractall(score_climb_with_gold(_)),
	assert(score_climb_with_gold(1000)),
	retractall(score_grab(_)),
	assert(score_grab(0)),
	retractall(score_wumpus_dead(_)),
	assert(score_wumpus_dead(0)),
	retractall(is_situation(_,_,_,_,_)),
	retractall(short_goal(_)).


%-------------------------------------------------------------------------------
% Display
%

description :-
	
	agent_location([X,Y]),
	degree(O),
	format(">I'm in ~p, turned to ~p, ",[[X,Y],O]).
	
description_total :-
	agent_healthy_state(AFG),
	format("| -> Agent health : ~p~n",[AFG]),	
	wumpus_healthy_state(WFS),
	format("| -> Wumpus Health : ~p~n",[WFS]),
	time(T),
	format("| -> Time : ~p~n",[T]),
	nb_visited(N),
	format("| -> Number of visited room : ~p~n",[N]),
	land_extent(LE),
	E is (LE - 1) * (LE - 1),
	format("| -> Number of total room : ~p~n",[E]),
	agent_score(SC),
	format("| -> Score : ~p~n",[SC]),
	agent_hold_state(AHS),
	format("| -> Gold : ~p~n",[AHS]),
	agent_goal(G),
	format("| -> Strategy Goal : ~p~n",[G]),
	agent_in_the_cave_state(ACS),
	format("| -> Body agent is : ~p~n",[ACS]).
	
degree(east) :- agent_orientation(0).
degree(north) :- agent_orientation(90).
degree(west) :- agent_orientation(180).
degree(south) :- agent_orientation(270).	
	
agent_in_the_cave_state(in_the_cave) :- agent_in_cave,!.
agent_in_the_cave_state(out_the_cave).

agent_healthy_state(perfect_health) :- 
	agent_healthy,
	agent_courage,
	!.
	
agent_healthy_state(a_little_tired_but_alive) :- 
	agent_healthy,
	!.	
agent_healthy_state(dead).

agent_hold_state(with_me) :- agent_hold,!.
agent_hold_state(not_found).

wumpus_healthy_state(alive) :- wumpus_healthy,!.
wumpus_healthy_state(wumpus_is_dead_baby).

the_end('=D I am the BEST') :-
	no(agent_in_cave),
	agent_hold,
	is_dead,
	!.
	
the_end('=) Pfftt too easy') :-
	no(agent_in_cave),
	agent_hold,
	no(is_dead),
	!.	
	
the_end(':) Too tired') :-
	no(agent_in_cave),
	no(agent_hold),
	is_dead,
	!.
	
the_end(':| I am sure Wumpus has moved !!!') :-
	no(agent_in_cave),
	no(agent_hold),
	no(is_dead),
	!.
		
the_end(':( No Comment') :-
	agent_in_cave,
	no(agent_hold),
	no(is_dead).	


%-------------------------------------------------------------------------------
% Perception 
%			Problem with adjacent...return only one value.
%

make_percept_sentence([Stench,Bleeze,Glitter,Bump,Scream]) :-
	stenchy(Stench),
	bleezy(Bleeze),
	glittering(Glitter),
	bumped(Bump),
	heardscream(Scream).

stenchy(yes) :-
	wumpus_location(L1),
	agent_location(L2),
	adjacent(L1,L2),
	!.
stenchy(no).

bleezy(yes) :- 
	pit_location(L1),
	agent_location(L2), 
	adjacent(L1,L2),
	!.
bleezy(no).

glittering(yes) :-
	agent_location(L),
	gold_location(L),
	!.
glittering(no).

bumped(yes) :-			% I feel a wall when I'm closed of a wall
	agent_location(L),
	wall(L),
	!.
bumped(no).

heardscream(yes) :-
	no(wumpus_healthy), 
	!.
heardscream(no).


%-------------------------------------------------------------------------------
% Tell_KB : 
% inform the Knowledge Base of our agent about the features of the world
%

tell_KB([Stench,Bleeze,Glitter,yes,Scream]) :- 
	add_wall_KB(yes),!.

tell_KB([Stench,Bleeze,Glitter,Bump,Scream]) :-
%	agent_location(L),	% update only if unknown could be great
%	no(is_visited(L)),	% but the wumpus dead changes : percept
	add_wumpus_KB(Stench),
	add_pit_KB(Bleeze),
	add_gold_KB(Glitter),
%	add_wall_KB(Bump),
	add_scream_KB(Scream).


% update our knowledge about wumpus presence

add_wumpus_KB(no) :-
	agent_location(L1),
	assume_wumpus(no,L1), 		% I'm not in a wumpus place
	location_toward(L1,0,L2),	% I'm sure there are no Wumpus in
	assume_wumpus(no,L2),		% each adjacent room. >=P
	location_toward(L1,90,L3),	
	assume_wumpus(no,L3),
	location_toward(L1,180,L4),	
	assume_wumpus(no,L4),
	location_toward(L1,270,L5),	
	assume_wumpus(no,L5),
	!.
add_wumpus_KB(yes) :-	
	agent_location(L1),	% I don't know if I'm in a wumpus place
	location_toward(L1,0,L2),% And It's possible there are Wumpus in 
	assume_wumpus(yes,L2),		% each adjacent room. <=|
	location_toward(L1,90,L3),	
	assume_wumpus(yes,L3),
	location_toward(L1,180,L4),	
	assume_wumpus(yes,L4),
	location_toward(L1,270,L5),	
	assume_wumpus(yes,L5).
	
assume_wumpus(no,L) :-
	retractall(is_wumpus(_,L)),
	assert(is_wumpus(no,L)),
	!.
	
assume_wumpus(yes,L) :- 		% before I knew there is no Wumpus,
	is_wumpus(no,L),		% so Wumpus can't be now ... =)
	!.				% ... Except if it's able to move 
	
assume_wumpus(yes,L) :- 		
	wall(L),			% Wumpus can't be in a wall
%	wumpus_healthy,			
	retractall(is_wumpus(_,L)),
	assert(is_wumpus(no,L)),
	!.
	
assume_wumpus(yes,L) :- 
	wumpus_healthy,			% so...
	retractall(is_wumpus(_,L)),
	assert(is_wumpus(yes,L)),
	!.
	
assume_wumpus(yes,L) :-
	retractall(is_wumpus(_,L)),
	assert(is_wumpus(no,L)).	% because Wumpus is dead >=]	
	
	
% update our knowledge about pit presence
	
add_pit_KB(no) :-
	agent_location(L1),
	assume_pit(no,L1), 		% I'm not in a pit {:o 
	location_toward(L1,0,L2),	% And I'm sure there are no pit in
	assume_pit(no,L2), 		% each adjacent room. >=P
	location_toward(L1,90,L3),	
	assume_pit(no,L3),
	location_toward(L1,180,L4),	
	assume_pit(no,L4),
	location_toward(L1,270,L5),	
	assume_pit(no,L5),
	!.
add_pit_KB(yes) :-	
	agent_location(L1),	% I don't know if I'm in a pit place.
	location_toward(L1,0,L2),	% It's possible there are Pit in
	assume_pit(yes,L2),		% each adjacent room. <=|
	location_toward(L1,90,L3),	
	assume_pit(yes,L3),
	location_toward(L1,180,L4),	
	assume_pit(yes,L4),
	location_toward(L1,270,L5),	
	assume_pit(yes,L5).
	
assume_pit(no,L) :-
	retractall(is_pit(_,L)),
	assert(is_pit(no,L)),
	!.
	
assume_pit(yes,L) :- 			% before I knew there is no pit,
	is_pit(no,L),			% so pit can't be now ... =)
	!.
	
assume_pit(yes,L) :- 
	wall(L),			% No Pit in a wall...
	retractall(is_pit(_,L)),
	assert(is_pit(no,L)),
	!.
	
assume_pit(yes,L) :- 
	retractall(is_pit(_,L)),
	assert(is_pit(yes,L)).	
	
% update our knowledge about gold presence

add_gold_KB(yes) :-
	agent_location(L),
	retractall(is_gold(L)), 	% no purpose here, I know ;)
	assert(is_gold(L)),
	!.
add_gold_KB(no).		

% update our knowledge about wall presence

add_wall_KB(yes) :-			% here I know where there is wall
	agent_location(L),		% because I'm in ...	
	retractall(is_wall(L)),	
	assert(is_wall(L)),
	!.					
add_wall_KB(no).
		
% update our knowledge about wumpus health 

add_scream_KB(yes) :-
	retractall(is_dead),
	assert(is_dead),
	!.
add_scream_KB(no).


%-------------------------------------------------------------------------------
% Ask_KB
%

ask_KB(Action) :- make_action_query(Strategy,Action).

make_action_query(Strategy,Action) :- act(strategy_reflex,Action),!.
make_action_query(Strategy,Action) :- act(strategy_find_out,Action),!.
make_action_query(Strategy,Action) :- act(strategy_go_out,Action),!.


% Strategy Reflex

act(strategy_reflex,rebound) :-		% back at the last location
	agent_location(L),
	is_wall(L),
	is_short_goal(rebound),!.
	
act(strategy_reflex,die) :- 
	agent_healthy,
	wumpus_healthy,
	agent_location(L),
	wumpus_location(L),
	is_short_goal(die_wumpus),
	!.
	
act(strategy_reflex,die) :- 
	agent_healthy,
	agent_location(L),
	pit_location(L),
	is_short_goal(die_pit),
	!.
		
act(strategy_reflex,shoot) :-		% I shoot Wumpus only if I think  
	agent_arrows(1),		% that we are in the same X
	agent_location([X,Y]),		% it means I assume Wumpus and me
	location_ahead([X,NY]),		
	is_wumpus(yes,[X,WY]),		% are in the same column
	dist(NY,WY,R1),			% And If I don't give him my back...
	dist(Y,WY,R2),			% <=> If I'm in the good orientation
	inf_equal(R1,R2),		% to shoot him... HE!HE!
	is_short_goal(shoot_forward_in_the_same_X),
	!.
	
act(strategy_reflex,shoot) :-		% I shoot Wumpus only if I think  
	agent_arrows(1),		% that we are in the same y
	agent_location([X,Y]),
	location_ahead([NX,Y]),
	is_wumpus(yes,[WX,Y]),
	dist(NX,WX,R1),			% And If I don't give him my back...
	dist(X,WX,R2),
	inf_equal(R1,R2),
	is_short_goal(shoot_forward_in_the_same_Y),
	!.	
	
act(strategy_reflex,grab) :-
	agent_location(L),		
	is_gold(L),			
	is_short_goal(grab_gold),	
	!.
	
act(strategy_reflex,climb) :-		% climb with gold
	agent_location([1,1]),	
	agent_hold,	
%	format("I'm going out~n",[]),	
	is_short_goal(nothing_more),	
	!.
			
% Strategy to find out gold	
	
% And there is a good room adjacent	

act(strategy_find_out,forward) :-			
	agent_goal(find_out),
	agent_courage,
	good(_),			% I'm interested by a good somewhere
	location_ahead(L),		% this somewhere is just
	good(L),			% the room in front of me.
	no(is_wall(L)),
	is_short_goal(find_out_forward_good_good),
	!.
	
act(strategy_find_out,turnleft) :-			
	agent_goal(find_out),
	agent_courage,
	good(_),			% I'm interested,...
	agent_orientation(O),
	Planned_O is (O+90) mod 360,
	agent_location(L),
	location_toward(L,Planned_O,Planned_L),
	good(Planned_L),		% directly by my left side.
	no(is_wall(Planned_L)),
	is_short_goal(find_out_turnleft_good_good),
	!.
	
act(strategy_find_out,turnright) :-
	agent_goal(find_out),
	agent_courage,
	good(_),			% I'm interested,
	agent_orientation(O),
	Planned_O is abs(O-90) mod 360,
	agent_location(L),
	location_toward(L,Planned_O,Planned_L),
	good(Planned_L),		% directly by my right side.
	no(is_wall(Planned_L)),
	is_short_goal(find_out_turnright_good_good),
	!.	
		
% And there is a good room but not adjacent	
	
act(strategy_find_out,forward) :- 			
	agent_goal(find_out),
	agent_courage,		
	good(_),			% I'm interested by good somewhere	
	location_ahead(L),		% I'm looking for this better...
	medium(L),			% I use medium room to go to
	no(is_wall(L)),
	is_short_goal(find_out_forward_good_medium),
	!.
	
act(strategy_find_out,turnleft) :- 	
	agent_goal(find_out),
	agent_courage,		
	good(_),			% I'm interested by good somewhere
	agent_orientation(O),
	Planned_O is (O+90) mod 360,	% my leftside can help me :
	agent_location(L),
	location_toward(L,Planned_O,Planned_L),
	medium(Planned_L),		% I use medium room to go to
	no(is_wall(Planned_L)),
	is_short_goal(find_out_turnleft_good_medium),
	!.
	
act(strategy_find_out,turnright) :- 		
	agent_goal(find_out),
	agent_courage,
	good(_),			% I'm interested by good somewhere
	agent_orientation(O),
	Planned_O is abs(O-90) mod 360, % my rightside can help me
	agent_location(L),
	location_toward(L,Planned_O,Planned_L),
	medium(Planned_L),		% I use medium room 
	no(is_wall(Planned_L)),
	is_short_goal(find_out_turnright_good_medium),
	!.
	
act(strategy_find_out,turnleft) :-	% I want to change completely  
	agent_goal(find_out),		% my direction ( + 180 )
	agent_courage,
	good(_),			% while I don't find it, I look for
	is_short_goal(find_out_180_good_),!.	
	
	
% I'm not tired but nowhere is good room and my goal is always find gold
% So I'm testing risky and deadly room...
	
% First the risky room	
	
act(strategy_find_out,forward) :-	% I don't know any more good room 
	agent_goal(find_out),	 	% Now I'm not interested anymore by
	agent_courage, 	
	location_ahead(L),		% looking for a risky room better 
	risky(L),			% than a deadly room, .
	no(is_wall(L)),			% Can't be a wall !!!
	is_short_goal(find_out_forward__risky),
	!.
	
act(strategy_find_out,turnleft) :-
	agent_courage,		
	agent_goal(find_out),		% so I test by following priority :
	agent_orientation(O),		% risky(forward), risky(turnleft),
	Planned_O is (O+90) mod 360,	% risky(turnright), deadly(forward)
	agent_location(L),
	location_toward(L,Planned_O,Planned_L),
	risky(Planned_L),
	no(is_wall(Planned_L)),		% Can't be a wall !!!
	is_short_goal(find_out_turnleft__risky),
	!.
	
act(strategy_find_out,turnright) :-
	agent_courage,
	agent_goal(find_out),
	agent_orientation(O),
	Planned_O is abs(O-90) mod 360,	
	agent_location(L),
	location_toward(L,Planned_O,Planned_L),
	risky(Planned_L),
	no(is_wall(Planned_L)),		% Can't be a wall !!!
	is_short_goal(find_out_turnright__risky).
	
% Second the deadly room		

act(strategy_find_out,forward) :-
	agent_courage,			
	agent_goal(find_out),		
	location_ahead(L),		
	deadly(L),
	no(is_wall(L)),			% Can't be a wall !!!
	is_short_goal(find_out_forward__deadly),
	!.	
	
act(strategy_find_out,turn_left) :-
	agent_courage,			
	agent_goal(find_out),
	agent_orientation(O),
	Planned_O is (O+90) mod 360,
	agent_location(L),
	location_toward(L,Planned_O,Planned_L),
	deadly(Planned_L),
	no(is_wall(Planned_L)),		% Can't be a wall !!!	
	is_short_goal(find_out_turnleft__deadly),
	!.
	
act(strategy_find_out,turn_right) :-
	agent_courage,		
	agent_goal(find_out),
	agent_orientation(O),
	Planned_O is abs(O-90) mod 360,
	agent_location(L),
	location_toward(L,Planned_O,Planned_L),
	no(is_wall(Planned_L)),		% Can't be a wall !!!	
	deadly(Planned_L),
	is_short_goal(find_out_turnright__deadly),!.	
	
% Strategy to go out : I follow the wall helping the chance...
% I perfom this strategy in order to go out because
% I found gold
% I'm tired
% agent_hold or no(agent_courage) is True.

act(strategy_go_out,climb) :-
	agent_location([1,1]),
	is_short_goal(go_out___climb),
	!.	
	
act(strategy_go_out,forward) :-
	location_ahead(L),
	medium(L),
	no(is_wall(L)),
	is_short_goal(go_out_forward__medium),
	!.

act(strategy_go_out,turnleft) :-	% I'm interested by my left side
	agent_orientation(O),
	Planned_O is (O+90) mod 360,
	agent_location(L),
	location_toward(L,Planned_O,Planned_L),
	medium(Planned_L),
	no(is_wall(Planned_L)),
	is_short_goal(go_out_turnleft__medium),
	!.
	
act(strategy_go_out,turnright) :-	% I'm interested by my right side
	agent_orientation(O),
	Planned_O is abs(O-90) mod 360,
	agent_location(L),
	location_toward(L,Planned_O,Planned_L),
	medium(Planned_L),
	no(is_wall(Planned_L)),
	is_short_goal(go_out_turnright__medium),
	!.
			
act(strategy_go_out,turnleft) :-	% I want to change completely  
	is_short_goal(go_out_180__).	% my orientation ( 180 )



%-------------------------------------------------------------------------------
% carry out : result(Action)
%

apply(rebound) :-
	agent_location(L),
	agent_orientation(O),		
	Back_O is (O+180) mod 360,
	location_toward(L,Back_O,L2),
	retractall(agent_location(_)),
	assert(agent_location(L2)).	% back at the last location
	
apply(die) :-
	agent_location(L1),
	wumpus_location(L1),		% If I'm dead =( , I'm not dead =)... 
	retractall(is_wumpus(yes,_)),	% we can use a function "restore"
	assert(is_wumpus(yes,L)),	% which gives a new life
	agent_score(S),
	score_agent_dead(SAD),
	New_S is S - SAD,
	assert(agent_score(New_S)),
	retractall(agent_healthy),
%	give_new_life,
 	format("Killed by Wumpus...~n",[]),
	!.
		
apply(die) :-	
	agent_location(L1),
	pit_location(L1),		% If I'm dead =( , I'm not dead =)...
	retractall(is_pit(_,L)),	% we can use a function "restore"
	assert(is_pit(yes,L)),		% which gives a new life
	agent_score(S),
	score_agent_dead(SAD),
	New_S is S - SAD,
	assert(agent_score(New_S)),
	retractall(agent_healthy),
%	give_new_life,
	format("Fallen in a pit...~n",[]),
	!.

apply(shoot) :-				% Now we check if actually there is
	agent_location([X,Y]),
	location_ahead([X,NY]),		
	wumpus_location([X,WY]),	% Wumpus, because it could be only
	dist(NY,WY,R1),			% a supposition.
	dist(Y,WY,R2),
	inf_equal(R1,R2),
	
	retractall(wumpus_location(_)),
	retractall(wumpus_healthy),
	retractall(agent_arrows(_)),
	assert(agent_arrows(0)),
	
	is_wumpus(yes,WL),
	assert(is_wumpus(no,WL)),
	retractall(is_wumpus(yes,_)),
	assert(is_dead),
	
	agent_score(S),	
	score_wumpus_dead(SWD),
	New_S is S + SWD,
	retractall(agent_score(S)),
	assert(agent_score(New_S)),
	format("There can be only One~n",[]),
	!.
	
apply(shoot) :-				% Now we check if actually there is
	agent_location([X,Y]),
	location_ahead([NX,Y]),	
	wumpus_location([WX,Y]),	% Wumpus, because it could be only
	dist(NX,WX,R1),			% a supposition.
	dist(X,WX,R2),
	inf_equal(R1,R2),
	
	retractall(wumpus_location(_)),
	retractall(wumpus_healthy),
	retractall(agent_arrows(_)),
	assert(agent_arrows(0)),
	
	is_wumpus(yes,WL),
	assert(is_wumpus(no,WL)),
	retractall(is_wumpus(yes,_)),
	assert(is_dead),
	
	agent_score(S),	
	score_wumpus_dead(SWD),
	New_S is S + SWD,
	retractall(agent_score(S)),
	assert(agent_score(New_S)),
	format("There can be only One~n",[]),
	!.
	
apply(shoot) :-				% Wumpus is missed
	format("Ouchh, I fail Grrrr  >=}...~n",[]),	
	retractall(agent_arrows(_)),	% I can infer some informations !!! 
	assert(agent_arrows(0)),	
	agent_location([X,Y]),		% I can assume that Wumpus...
	location_ahead([X,NY]),
	is_wumpus(yes,[X,WY]),
	retractall(is_wumpus(yes,[X,WY])),	
	assert(is_wumpus(no,[X,WY])),	% ...is not in the supposed room.
	!.
	
apply(shoot) :-				% Wumpus is missed
	format("Ouchh, I fail Grrrr  >=}...~n",[]),	
	retractall(agent_arrows(_)),	% I can infer some informations !!! 
	assert(agent_arrows(0)),	
	agent_orientation(TO),		% I can assume that Wumpus...
	agent_location([X,Y]),
	location_ahead([NX,Y]),
	is_wumpus(yes,[WX,Y]),
	retractall(is_wumpus(yes,[WX,Y])),	
	assert(is_wumpus(no,[WX,Y])),	%  ...is not in the supposed room
	!.			
			
apply(climb) :-				% I win or I lose, 
	agent_hold,			% It depends on my hold...
	agent_score(S),	
	score_climb_with_gold(SC),
	New_S is S + SC,
	retractall(agent_score(S)),
	assert(agent_score(New_S)),
	retractall(agent_in_cave),
	!.
	
apply(climb) :-	
	retractall(agent_in_cave),
	!.	
	
apply(grab) :-
	agent_score(S),
	score_grab(SG),
	New_S is S + SG,
	retractall(agent_score(S)),
	assert(agent_score(New_S)),
	retractall(gold_location(_)),	% no more gold at this place
	retractall(is_gold(_)),		% The gold is with me!
	assert(agent_hold),		% money, money,  :P 
	retractall(agent_goal(_)),
	assert(agent_goal(go_out)),	% Now I want to go home
	format("Yomi! Yomi! Give me the money >=}...~n",[]),
	!.				
	
apply(forward) :-
	agent_orientation(O),
	agent_location(L),
	location_toward(L,O,New_L),
	retractall(agent_location(_)),
	assert(agent_location(New_L)),
	!.
	
apply(turnleft) :-	
	agent_orientation(O),
	New_O is (O + 90) mod 360,
	retractall(agent_orientation(_)),
	assert(agent_orientation(New_O)),
	!.
	
apply(turnright) :-	
	agent_orientation(O),
	New_O is abs(O - 90) mod 360,
	retractall(agent_orientation(_)),
	assert(agent_orientation(New_O)),
	!.

give_new_life :-
	agent_orientation(O),			% first go out the trap.
	New_O is (O+90) mod 360,		% Choosen arbitrary.
	retractall(agent_orientation(_)),
	assert(agent_orientation(New_O)),
	apply(forward),				% one step forward
	assert(agent_healthy).			% WE ARE GOD !!!


%-------------------------------------------------------------------------------
% More Definitions and Axioms
%

no(P) :- 
	P,
	!,
	fail. 
no(P).

location_toward([X,Y],0,[New_X,Y]) :- New_X is X+1.
location_toward([X,Y],90,[X,New_Y]) :- New_Y is Y+1.
location_toward([X,Y],180,[New_X,Y]) :- New_X is X-1.
location_toward([X,Y],270,[X,New_Y]) :- New_Y is Y-1.

adjacent(L1,L2) :- location_toward(L1,_,L2).

location_ahead(Ahead) :-
	agent_location(L),
	agent_orientation(O),
	location_toward(L,O,Ahead).
	
inf_equal(X,Y) :- X < Y,!.
inf_equal(X,Y) :- X = Y.

dist(X,Y,R) :- 
	inf_equal(X,Y),	
	R is Y - X,
	!.
dist(X,Y,R) :- R is X - Y.

wall([X,LE]) :- inf_equal(LE,0).		% there is wall
wall([LE,Y]) :- inf_equal(LE,0).		% there is wall
wall([X,Y]) :- land_extent(LE), inf_equal(LE,X).% there is wall
wall([X,Y]) :- land_extent(LE), inf_equal(LE,Y).% there is wall

action(forward).
action(turnleft).
action(turnright).
action(grab).
action(climb).
action(shoot).
action(die).

is_short_goal(X) :-
	retractall(short_goal(_)),
	assert(short_goal(X)).

is_nb_visited :-
	nb_visited(N),
	agent_location(L),
	no(is_visited(L)),
	retractall(nb_visited(_)),
	New_nb_visited is N + 1,
	assert(nb_visited(New_nb_visited)),
	!.
	
is_nb_visited.
	
agent_courage :-	% we choose arbitrory thanks to a lot of tries.
			% we could compute nb_visited / max_room_to_visit
	time(T),		% time 	
	nb_visited(N),		% number of visted room
	land_extent(LE),	% size of the land
	E is LE * LE,  		% maximum of room to visit
	NPLUSE is E * 2,
% 	NPLUSE is E * 2 + N,	
	inf_equal(T,NPLUSE).

% A location is estimated thanks to ... good, medium, risky, deadly.	

good(L) :-				% a wall can be a good room !!!
	is_wumpus(no,L),
	is_pit(no,L),
	no(is_visited(L)).
	
medium(L) :- 				% Obviously if is_visited(L) ->
	is_visited(L).			% is_wumpus(no,L) and is_pit(no,L)

risky(L) :- 
	no(deadly(L)).
		
deadly(L) :-
	is_wumpus(yes,L),
	is_pit(yes,L),
	no(is_visited(L)).
