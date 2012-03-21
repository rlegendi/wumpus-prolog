% wumpus_world.pl

:- module(wumpus,[initialize/2, execute/2, display_world/0]).

% AM
:- use_module(library(lists)).

% Wumpus World Simulator v2.3
%
% Written by Larry Holder (holder@cse.uta.edu)
%
% A Prolog implementation of the Wumpus world described in Russell and
% Norvig's "Artificial Intelligence: A Modern Approach", Section 6.2.
% A few enhancements have been added:
%   - random wumpus world generator
%
% See comments on the following interface procedures:
%
%   evaluate_agent(Trials,Score,Time)
%   initialize(World,Percept)

%WNb
% modified for SWI-Prolog
% without the following enhancements:
%     natural language hint, movement of wumpus, image processing, several tries per world
% Walter Nauber
% 02.02.01
% changes: max_agent_actions = 8*n*n

:- load_files([utils]).  % Basic utilities

:- dynamic  wumpus_world_default_extent/1, max_agent_actions/1.   
:- dynamic  wumpus_world_extent/1,  wumpus_location/2, wumpus_health/1,  gold/2,  pit/2.
:- dynamic  agent_location/2,  agent_orientation/1,  agent_in_cave/1,  agent_health/1.
:- dynamic  agent_gold/1,  agent_arrows/1,  agent_score/1.

wumpus_world_default_extent(4).  % Default size of the cave is 4x4
%WNe


gold_probability(0.10).  % Probability that a location has gold
pit_probability(0.15).   % Probability that a non-(1,1) location has a pit
max_agent_actions(128).  % Maximum actions per trial allowed by agent


% evaluate_agent(Trials,Score,Time): Performs Trials trials, where each
%   trial involves generating a random wumpus world, initializing the
%   agent, running the agent until it dies or leaves the cave, and then
%   recording the score and time spent running the agent.  The total
%   score and time are returned in Score and Time (millisecs).
%
%   This procedure requires the external definition of two procedures:
%
%     init_agent: Called after new world is initialized.  Should perform
%                 any needed agent initialization.
%
%     run_agent(Percept,Action): Given the current Percept, this procedure
%                 should return an appropriate Action, which is then
%                 executed.

evaluate_agent(Trials,Score,Time) :-
  run_agent_trials(Trials,1,Score,Time).

% run_agent_trials(Trials,NextTrial,Score,Time): Runs trials from NextTrial
%   to Trial and returns the total Score and Time (millisecs) spent inside
%   calls to init_agent and run_agent.

run_agent_trials(Trials,NextTrial,0,0) :-
  NextTrial > Trials.

run_agent_trials(Trials,NextTrial,Score,Time) :-
  NextTrial =< Trials,
  format("Trial ~d~n",[NextTrial]),
  initialize(random,Percept),
%WN
%  statistics(runtime,[T1|_]),
  statistics(cputime,T1),
%WN
  init_agent,                         % needs to be defined externally
%WN
%  statistics(runtime,[T2|_]),

  statistics(cputime,T2),
%WN
  run_agent_trial(1,Percept,Time1),
  agent_score(Score1),
  NextTrial1 is NextTrial + 1,
  run_agent_trials(Trials,NextTrial1,Score2,Time2),
  Score is Score1 + Score2,
  Time is Time1 + Time2 + (T2 - T1).


% run_agent_trial(NumActions,Percept,Time):  Continues to ask for and
%   execute actions from run_agent(Percept,Action) until either the
%   agent dies, leaves the cave or executes the maximum M actions as
%   defined by max_agent_actions(M).  In any case, the total time
%   spent during calls to run_agent is returned in Time (millisecs).

run_agent_trial(_,_,0) :-             % trial over when agent dies or
  ( agent_health(dead) ;              %   leaves cave
    agent_in_cave(no) ),
  !.

run_agent_trial(NumActions,_,0) :-    % agent allowed only N actions as
  max_agent_actions(N),               %   defined by max_agent_actions(N)
  NumActions > N,
  !.

run_agent_trial(NumActions,Percept,Time) :-
%WN
%  statistics(runtime,[T1|_]),
  statistics(cputime,T1),

%WN
  run_agent(Percept,Action),          % needs to be defined externally
%WN
%  statistics(runtime,[T2|_]),
  statistics(cputime,T2),

%WN
  execute(Action,Percept1),
  NumActions1 is NumActions + 1,
  run_agent_trial(NumActions1,Percept1,Time1),
  Time is Time1 + (T2 - T1).

% initialize(World,Percept): initializes the Wumpus world and our fearless
%   agent according to the given World and returns the Percept from square
%   1,1.  World can be either 'fig62' for Figure 6.2 of Russell and Norvig,
%   or 'random' to generate a random world.

initialize(World,[Stench,Breeze,Glitter,no,no]) :-
  initialize_world(World),
  initialize_agent,
  stench(Stench),
  breeze(Breeze),
  glitter(Glitter).
%  display_action(initialize).



% initialize_world(World): Initializes the Wumpus world.  World is either
%   fig62, which generates the wumpus world in Figure 6.2 of [Russell &
%   Norvig], or World=random, which generates a random world according to
%   the following guidelines:
%
%   Size: The size of the wumpus world is fixed at 4x4, but can be set
%         arbitrarily using different values for wumpus_world_extent(E).
%
%   Wumpus Location: The initial wumpus location is chosen at random
%                    anywhere in the cave except location (1,1).
%
%   Pit Location: Each square has a pit with probability P set by
%                 pit_probability(P), except location (1,1), which
%                 will never have a pit.
%
%   Gold Location: Each square has gold with probability P set by
%                  gold_probability(P).  At least one square will have
%                  gold; no more than one gold piece per square.
%
% wumpus_world_extent(E): defines world to be E by E
% wumpus_location(X,Y): the Wumpus is in square X,Y
% wumpus_health(H): H is 'dead' or 'alive'
% gold(X,Y): there is gold in square X,Y
% pit(X,Y): there is a pit in square X,Y

initialize_world(fig62) :-
  ww_retractall,
  retractall(ww_initial_state(_)),
  assert(ww_initial_state([])),
  addto_ww_init_state(wumpus_world_extent(4)),
  addto_ww_init_state(wumpus_location(1,3)),
  addto_ww_init_state(wumpus_health(alive)),
  addto_ww_init_state(gold(2,3)),
  addto_ww_init_state(pit(3,1)),
  addto_ww_init_state(pit(3,3)),
  addto_ww_init_state(pit(4,4)),
  ww_initial_state(L),
  assert_list(L).

initialize_world(random) :-
  ww_retractall,
  retractall(ww_initial_state(_)),
  assert(ww_initial_state([])),
%WNb
  wumpus_world_default_extent(E),
  retract(max_agent_actions(_)),
  M is (8*E*E), 
  assert(max_agent_actions(M)),
  addto_ww_init_state(wumpus_world_extent(E)),
  all_squares(E,AllSqrs),
  gold_probability(PG),             % place gold
  place_objects(gold,PG,AllSqrs,GRestSqrs),
  at_least_one_gold(E,AllSqrs,GRestSqrs,PSqrs),
  delete(AllSqrs,[1,1],AllSqrs1),   % deletes element [1,1] from list AllSqrs
  random_member([WX,WY],AllSqrs1),  % initialize wumpus
  delete(PSqrs,[1,1],PSqrs1),       % deletes element [1,1] from list PSqrs
  delete(PSqrs1,[WX,WY],PSqrs2),    % deletes element [WX,WY] from list PSqrs1
  pit_probability(PP),              % place pits
  place_objects(pit,PP,PSqrs2,_),
  
%WNe
  addto_ww_init_state(wumpus_location(WX,WY)),
  addto_ww_init_state(wumpus_health(alive)),
  ww_initial_state(L),
  assert_list(L).


% initialize_agent: agent is initially alive, destitute (except for one
%   arrow), in grid 1,1 and facing to the right (0 degrees).

initialize_agent :-
  retractall(agent_location(_,_)),
  retractall(agent_orientation(_)),
  retractall(agent_in_cave(_)),
  retractall(agent_health(_)),
  retractall(agent_gold(_)),
  retractall(agent_arrows(_)),
  retractall(agent_score(_)),
  assert(agent_location(1,1)),
  assert(agent_orientation(0)),
  assert(agent_in_cave(yes)),
  assert(agent_health(alive)),
  assert(agent_gold(0)),
  assert(agent_arrows(1)),
  assert(agent_score(0)).


% ww_retractall: Retract all wumpus world information, except about the
%   agent.

ww_retractall :-
  retractall(wumpus_world_extent(_)),
  retractall(wumpus_location(_,_)),
  retractall(wumpus_health(_)),
  retractall(gold(_,_)),
  retractall(pit(_,_)).


% addto_ww_init_state(Fact): Adds Fact to the list L stored in
%   ww_initial_state(L).

addto_ww_init_state(Fact) :-
  retract(ww_initial_state(L)),
  assert(ww_initial_state([Fact|L])).


% assert_list(L): Assert all facts on list L.

assert_list([]).

assert_list([Fact|Facts]) :-
  assert(Fact),
  assert_list(Facts).


% all_squares(Extent,AllSqrs): AllSqrs is the list of all possible
%   squares [X,Y] in a wumpus world of size Extent by Extent.

all_squares(Extent,AllSqrs) :-
  all_squares_1(Extent,1,1,AllSqrs).

all_squares_1(Extent,Extent,Extent,[[Extent,Extent]]).

all_squares_1(Extent,Row,Extent,[[Row,Extent]|RestSqrs]) :-
  Row < Extent,
  Row1 is Row + 1,
  all_squares_1(Extent,Row1,1,RestSqrs).

all_squares_1(Extent,Row,Col,[[Row,Col]|RestSqrs]) :-
  Col < Extent,
  Col1 is Col + 1,
  all_squares_1(Extent,Row,Col1,RestSqrs).


% place_objects(Object,P,Squares): For each square in Squares, place
%   Object at square with probability P.

%WNb
place_objects(_,_,[],[]).

place_objects(Object,P,[Square|Squares],RestSquares) :-
  maybe(P),   % succeeds with probability P
  !,
  Fact =.. [Object|Square],
  addto_ww_init_state(Fact),
  place_objects(Object,P,Squares,RestSquares).

place_objects(Object,P,[Square|Squares],[Square|RestSquares]) :-
  place_objects(Object,P,Squares, RestSquares).


% at_least_one_gold(Extent,AllSqrs,GRestSqrs,PSqrs):
% Ensures that at least on gold piece is somewhere in the wumpus world

at_least_one_gold(_,AllSqrs,GRestSqrs,GRestSqrs) :-
  \+ AllSqrs=GRestSqrs,
  !.

at_least_one_gold(E,AllSqrs,AllSqrs,GRestSqrs) :-
   random3(1,E,X),
   random3(1,E,Y),
   delete(AllSqrs,[X,Y],GRestSqrs),
%WNe
  addto_ww_init_state(gold(X,Y)).


%------------------------------------------------------------------------
% execute(Action,Percept): executes Action and returns Percept
%
%   Action is one of:
%     goforward: move one square along current orientation if possible
%     turnleft:  turn left 90 degrees
%     turnright: turn right 90 degrees
%     grab:      pickup gold if in square
%     shoot:     shoot an arrow along orientation, killing wumpus if
%                in that direction
%     climb:     if in square 1,1, leaves the cave and adds 1000 points
%                for each piece of gold
%
%   Percept = [Stench,Breeze,Glitter,Bump,Scream]
%             The five parameters are either 'yes' or 'no'.

execute(_,[no,no,no,no,no]) :-
  agent_health(dead), !,         % agent must be alive to execute actions
  format("You are dead!~n",[]).

execute(_,[no,no,no,no,no]) :-
  agent_in_cave(no), !,         % agent must be in the cave
  format("You have left the cave.~n",[]).

execute(goforward,[Stench,Breeze,Glitter,Bump,no]) :-
  decrement_score,
  goforward(Bump),        % update location and check for bump
  update_agent_health,    % check for wumpus or pit
  stench(Stench),         % update rest of percept
  breeze(Breeze),
  glitter(Glitter),
  display_action(goforward).

execute(turnleft,[Stench,Breeze,Glitter,no,no]) :-
  decrement_score,
  agent_orientation(Angle),
  NewAngle is (Angle + 90) mod 360,
  retract(agent_orientation(Angle)),
  assert(agent_orientation(NewAngle)),
  stench(Stench),
  breeze(Breeze),
  glitter(Glitter),
  display_action(turnleft).

execute(turnright,[Stench,Breeze,Glitter,no,no]) :-
  decrement_score,
  agent_orientation(Angle),
  NewAngle is (Angle + 270) mod 360,
  retract(agent_orientation(Angle)),
  assert(agent_orientation(NewAngle)),
  stench(Stench),
  breeze(Breeze),
  glitter(Glitter),
  display_action(turnright).

execute(grab,[Stench,Breeze,no,no,no]) :-
  decrement_score,
  get_the_gold,
  stench(Stench),
  breeze(Breeze),
  display_action(grab).

execute(shoot,[Stench,Breeze,Glitter,no,Scream]) :-
  decrement_score,
  shoot_arrow(Scream),
  stench(Stench),
  breeze(Breeze),
  glitter(Glitter),
  display_action(shoot).

execute(climb,[no,no,no,no,no]) :-  % climb works
  agent_location(1,1), !,
  decrement_score,
  agent_gold(G),
  retract(agent_score(S)),
  S1 is (S + (1000 * G)),
  assert(agent_score(S1)),
  retract(agent_in_cave(yes)),
  assert(agent_in_cave(no)),
  display_action(climb),
  format("I am outta here.~n",[]).

execute(climb,[Stench,Breeze,Glitter,no,no]) :-
  decrement_score,
  stench(Stench),
  breeze(Breeze),
  glitter(Glitter),
  display_action(climb),
  format("You cannot leave the cave from here.~n",[]).

% decrement_score: subtracts one from agent_score for each move

decrement_score :-
  retract(agent_score(S)),
  S1 is S - 1,
  assert(agent_score(S1)).


% stench(Stench): Stench = yes if wumpus (dead or alive) is in a square
%   directly up, down, left, or right of the current agent location.

stench(yes) :-
  agent_location(X,Y),
  X1 is X + 1,
  X0 is X - 1,
  Y1 is Y + 1,
  Y0 is Y - 1,
  ( wumpus_location(X1,Y) ;
    wumpus_location(X0,Y) ;
    wumpus_location(X,Y1) ;
    wumpus_location(X,Y0) ;
    wumpus_location(X,Y) ),
  !.

stench(no).


% breeze(Breeze): Breeze = yes if a pit is in a square directly up, down,
%   left, or right of the current agent location.

breeze(yes) :-
  agent_location(X,Y),
  X1 is X + 1,
  X0 is X - 1,
  Y1 is Y + 1,
  Y0 is Y - 1,
  ( pit(X1,Y) ;
    pit(X0,Y) ;
    pit(X,Y1) ;
    pit(X,Y0) ;
    pit(X,Y)  ),
  !.

breeze(no).


% glitter(Glitter): Glitter = yes if there is gold in the current agent
%   location.

glitter(yes) :-
  agent_location(X,Y),
  gold(X,Y),
  !.

glitter(no).


% kill_wumpus: pretty obvious

kill_wumpus :-
  retract(wumpus_health(alive)),
  assert(wumpus_health(dead)).


% goforward(Bump): Attempts to move agent forward one unit along
%   its current orientation.

goforward(no) :-
  agent_orientation(Angle),
  agent_location(X,Y),
  new_location(X,Y,Angle,X1,Y1),
  wumpus_world_extent(E),         % check if agent off world
  X1 > 0,
  X1 =< E,
  Y1 > 0,
  Y1 =< E,
  !,
  retract(agent_location(X,Y)),   % update location
  assert(agent_location(X1,Y1)).

goforward(yes).     % Ran into wall, Bump = yes


% new_location(X,Y,Orientation,X1,Y1): returns new coordinates X1,Y1
%   after moving from X,Y along Orientation: 0, 90, 180, 270 degrees.

new_location(X,Y,0,X1,Y) :-
  X1 is X + 1.

new_location(X,Y,90,X,Y1) :-
  Y1 is Y + 1.

new_location(X,Y,180,X1,Y) :-
  X1 is X - 1.

new_location(X,Y,270,X,Y1) :-
  Y1 is Y - 1.


% update_agent_health: kills agent if in a room with a live wumpus or a
%   pit.

update_agent_health :-
  agent_health(alive),
  agent_location(X,Y),
  wumpus_health(alive),
  wumpus_location(X,Y),
  !,
  retract(agent_health(alive)),
  assert(agent_health(dead)),
  retract(agent_score(S)),
  S1 is S - 1500, %WN
  assert(agent_score(S1)),
  format("You are Wumpus food!~n",[]).

update_agent_health :-
  agent_health(alive),
  agent_location(X,Y),
  pit(X,Y),
  !,
  retract(agent_health(alive)),
  assert(agent_health(dead)),
  retract(agent_score(S)),
  S1 is S - 1500,  %WN
  assert(agent_score(S1)),
  format("Aaaaaaaaaaaaaaaaaaa!~n",[]).

%WNb
update_agent_health :-
  agent_health(alive),
  agent_location(X,Y),
  pit(X,Y),
  !,
  retract(agent_health(alive)),
  assert(agent_health(dead)),
  retract(agent_score(S)),
  S1 is S - 1500,  %WN
  assert(agent_score(S1)),
  format("Aaaaaaaaaaaaaaaaaaa!~n",[]).
%WNe

update_agent_health.


% get_the_gold: adds gold to agents loot if any gold in the square

get_the_gold :-
  agent_location(X,Y),
  gold(X,Y), !,                   % there's gold in this square!
  agent_gold(NGold),              %   add to agents loot
  NGold1 is NGold + 1,
  retract(agent_gold(NGold)),
  assert(agent_gold(NGold1)),
  format("You now have ~d piece(s) of gold!~n",NGold1),
  retract(gold(X,Y)).             %   delete gold from square

get_the_gold.


% shoot_arrow(Scream): If agent has an arrow, then shoot it in the
%   direction the agent is facing and listen for Scream.

shoot_arrow(Scream) :-
  agent_arrows(Arrows),
  Arrows > 0, !,                  % agent has an arrow and will use it!
  Arrows1 is Arrows - 1,          %   update number of arrows
  retract(agent_arrows(Arrows)),
  assert(agent_arrows(Arrows1)),
  format("You now have ~d arrow(s).~n",Arrows1),
  agent_location(X,Y),
  agent_orientation(Angle),
  propagate_arrow(X,Y,Angle,Scream).

shoot_arrow(no).


% propagate_arrow(X,Y,Angle,Scream): If wumpus is at X,Y then hear its
%   woeful scream as you vanquish the creature.  If not, then move arrow
%   one square along Angle and try again.  If arrow hits a wall, then
%   you missed.

propagate_arrow(X,Y,_,yes) :-
  wumpus_location(X,Y), !,
%WNb
  kill_wumpus,
  retract(agent_score(S)),
  S1 is (S + 500),
  assert(agent_score(S1)).
%WNe

propagate_arrow(X,Y,0,Scream) :-
  X1 is X + 1,
  wumpus_world_extent(E),
  X1 =< E,
  !,
  propagate_arrow(X1,Y,0,Scream).

propagate_arrow(X,Y,90,Scream) :-
  Y1 is Y + 1,
  wumpus_world_extent(E),
  Y1 =< E,
  !,
  propagate_arrow(X,Y1,90,Scream).

propagate_arrow(X,Y,180,Scream) :-
  X1 is X - 1,
  X1 > 0,
  !,
  propagate_arrow(X1,Y,180,Scream).

propagate_arrow(X,Y,270,Scream) :-
  Y1 is Y - 1,
  Y1 > 0,
  !,
  propagate_arrow(X,Y1,270,Scream).

propagate_arrow(_,_,_,no).


% display_world: Displays everything known about the wumpus world,

display_world :-
  nl,
  wumpus_world_extent(E),
%WNb
  display_rows(E,E).
/*  
  wumpus_health(WH),
  agent_orientation(AA),
  agent_health(AH),
  agent_arrows(N),
  agent_gold(G),
  format('wumpus_health(~w)~n',[WH]),
  format('agent_orientation(~d)~n',[AA]),
  format('agent_health(~w)~n',[AH]),
  format('agent_arrows(~d)~n',[N]),
  format('agent_gold(~d)~n',[G]).
*/
%WNe

display_rows(0,E) :-
  !,
  display_dashes(E).

display_rows(Row,E) :-
  display_dashes(E),
  display_row(Row,E),
  Row1 is Row - 1,
  display_rows(Row1,E).

display_row(Row,E) :-
  display_square(1,Row,E).

display_square(X,_,E) :-
  X > E,
  !,
  format('|~n',[]).

display_square(X,Y,E) :-
  format('|',[]),
  display_info(X,Y),
  X1 is X + 1,
  display_square(X1,Y,E).

display_info(X,Y) :-
%WNb
  agent_orientation(AO),
  display_agent(AO,AC),
  display_location_fact(agent_location,X,Y,AC),
  display_location_fact(gold,X,Y,'G'),
  display_location_fact(pit,X,Y,'P'),
  write(' '),
  wumpus_health(WH),
  display_wumpus(WH,WC),
  display_location_fact(wumpus_location,X,Y,WC).
%WNe

display_location_fact(Functor,X,Y,Atom) :-
  Fact =.. [Functor,X,Y],
  Fact,
  !,
  format('~w',[Atom]). %WN

display_location_fact(_,_,_,_) :-
  format(' ',[]).

display_dashes(E) :-
  RowLen is (E * 6) + 1, %WN
  name('-',[Dash]),
  format('~*c~n',[RowLen,Dash]).

%WNb
%display_wumpus(Wumpus_Health,Wumpus_Char)

display_wumpus(alive,'W').
display_wumpus(dead, 'd').

%display_agent(Agent_Orientation,Agent_Char)

display_agent(  0,'>').
display_agent( 90,'^').
display_agent(180,'<').
display_agent(270,'V').
%WNe

% display_action(Action): Updates display after Action taken and
%   new percept generated.

display_action(Action) :-
  format("~nExecuting ~w~n",[Action]).
%  (((\+ hidden_cave(yes)) -> display_world);true).

