%%%-------------------------------------------------------------------
%%% @author Maayan Belzer, Nir Tapiero
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Jun 2020 12:01 AM
%%%-------------------------------------------------------------------
-module(cars).
-author("Maayan Belzer, Nir Tapiero").

-behaviour(gen_statem).

%% API
-export([start_link/0,start/5,start/8]).

%% gen_statem callbacks
-export([
  init/1,
  format_status/2,
  state_name/3,
  handle_event/4,
  terminate/3,
  code_change/4,
  callback_mode/0
]).

%%Events
-export([close_to_car/2,close_to_junc/4,accident/2,turn/2,bypass/1,far_from_car/1]).
-export([stop/2,kill/1,add_sensor/3,switch_comp/3,check_response/2]).

%% States
-export([drive_straight/3,idle/3,turning/3,stopping/3,bypassing/3,send_msg/2,first_state/3]).


-define(SERVER, ?MODULE).

-record(cars_state, {bypassCounter = 0,turnCounter = 0,nextTurnDir  ,nextTurnRoad, speed,lightPid,sensor1,sensor2,monitor}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
  gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).
start(Name,CarMonitor,Type,Start,PC) ->
  gen_statem:start({local, Name}, ?MODULE, [Name,CarMonitor,Start,Type,PC], []).
start(Name,CarMonitor,Type,Start,Location,Con,PC,Nav) -> gen_statem:start({local, Name}, ?MODULE, [Name,CarMonitor,Start,Type,Location,Con,PC,Nav], []).


%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {CallbackMode, StateName, State} |
%%                     {CallbackMode, StateName, State, Actions} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([Name,CarMonitor,Start,Type,PC]) -> % initialize car when starting the program
  put(name,Name), % put Name of car, car monitor, initial location and speed in process dictionary
  put(carMon,CarMonitor),
  put(start,Start),
  put(speed ,Type),
  ets:insert(cars,{self(),Start,Name,Start,Type,nal,PC,null}), % insert the car to the cars ets
  CarMonitor! {add_to_monitor,self()}, % add the car to the monitor
  {ok,first_state, #cars_state{speed = Type,monitor = CarMonitor},10}; % send to first state

init([Name,CarMonitor,Start,Type,Location,Con,PC,Nav]) -> % initialize the car when it moved to different PC
  put(name,Name),
  put(carMon,CarMonitor),
  put(start,Start),
  put(speed ,Type),

  ets:insert(cars,{self(),Location,Name,Start,Type,Con,PC,Nav}),
  CarMonitor! {add_to_monitor,self()},

  SensorPid = spawn(sensors,close_to_car,[self(),ets:first(cars)]), % spawn all car sensors, add them to their ets and put them in process dictionary
  SensorPid2 = spawn(sensors,close_to_junction,[self(),ets:first(junction)]),
  SensorPid3 = spawn(sensors,outOfRange,[self()]),
  SensorPid4 = spawn(sensors,car_accident,[self(),ets:first(cars)]),
  SensorPid5 = spawn(sensors,car_dev,[self()]),
  ets:insert(sensors,{SensorPid,self()}), ets:insert(sensors,{SensorPid2,self()}),
  ets:insert(sensors,{SensorPid3,self()}), ets:insert(sensors,{SensorPid4,self()}),
  ets:insert(sensors,{SensorPid5,self()}),
  put(sensor1 ,SensorPid), put(sensor2 ,SensorPid2),
  put(sensor3,SensorPid3), put(sensor4,SensorPid4),
  put(sensor5,SensorPid5),
  Monitor = CarMonitor,
  Monitor ! {add_to_monitor,SensorPid}, Monitor ! {add_to_monitor,SensorPid2}, % add sensors to monitor
  Monitor ! {add_to_monitor,SensorPid3}, Monitor ! {add_to_monitor,SensorPid4},
  Monitor ! {add_to_monitor,SensorPid5},

  case Con of % check in which state the car was and send it to the same state
    {drive_straight} -> {ok,drive_straight,#cars_state{},Type};
    {idle} ->{ok,idle, #cars_state{},Type} ;
    {turning,C,Dir,Road} ->{ok,turning, #cars_state{turnCounter = C, nextTurnDir = Dir, nextTurnRoad = Road},Type} ;
    {bypassing,C} -> {ok,bypassing, #cars_state{bypassCounter = C},Type}


  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it needs to find out
%% the callback mode of the callback module.
%%
%% @spec callback_mode() -> atom().
%% @end
%%--------------------------------------------------------------------
callback_mode() ->
  state_functions.

%% Events
close_to_car(Pid,OtherCar) -> gen_statem:cast(Pid,{ctc,Pid,OtherCar}). % car is close to another car
close_to_junc(Pid,LightState,{R,J},LP) -> gen_statem:cast(Pid,{ctj,Pid,LightState,{R,J},LP}). % car is close to junction
accident(Pid,OtherCar) ->   io:format("ACCIDENT between ~p and ~p ~n",[Pid,OtherCar]),gen_statem:cast(Pid,{acc,Pid,OtherCar}). % car got in accident
turn(Pid,{Dir, Road}) ->gen_statem:cast(Pid,{turn,Pid,{Dir, Road}}). % car needs to turn
bypass(Pid) -> gen_statem:cast(Pid,{byp,Pid}). % car needs to bypass
far_from_car(Pid) -> gen_statem:cast(Pid,{far,Pid}). % car is now far from a different car
stop(Pid,OtherCar) -> gen_statem:cast(Pid,{stop,Pid,OtherCar}). % car needs to stop
kill(Pid) ->  gen_statem:cast(Pid,{kill,Pid}). % kill car
send_msg(Pid,{From,Msg}) -> gen_statem:cast(Pid,{send,Pid,From,Msg}). % receive message from another car
add_sensor(Pid,Sensor,Type) -> gen_statem:cast(Pid,{add_sensor,Pid,Sensor,Type}). % recover sensor that fell down
switch_comp(Pid,From,To) -> gen_statem:cast(Pid,{switch,Pid,From,To}). % car moved to different PC





%%--------------------------------------------------------------------
%% @private
%% @doc
%% Called (1) whenever sys:get_status/1,2 is called by gen_statem or
%% (2) when gen_statem terminates abnormally.
%% This callback is optional.
%%
%% @spec format_status(Opt, [PDict, StateName, State]) -> term()
%% @end
%%--------------------------------------------------------------------
format_status(_Opt, [_PDict, _StateName, _State]) ->
  Status = some_term,
  Status.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name.  If callback_mode is statefunctions, one of these
%% functions is called when gen_statem receives and event from
%% call/2, cast/2, or as a normal process message.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Actions} |
%%                   {stop, Reason, NewState} |
%%    				 stop |
%%                   {stop, Reason :: term()} |
%%                   {stop, Reason :: term(), NewData :: data()} |
%%                   {stop_and_reply, Reason, Replies} |
%%                   {stop_and_reply, Reason, Replies, NewState} |
%%                   {keep_state, NewData :: data()} |
%%                   {keep_state, NewState, Actions} |
%%                   keep_state_and_data |
%%                   {keep_state_and_data, Actions}
%% @end
%%--------------------------------------------------------------------
state_name(_EventType, _EventContent, State) ->
  NextStateName = next_state,
  {next_state, NextStateName, State}.

first_state(timeout,10,State = #cars_state{}) -> % the first state of the car, spawn all sensors, add them to ets, process dictionary and monitor
  SensorPid = spawn(sensors,close_to_car,[self(),ets:first(cars)]),
  SensorPid2 = spawn(sensors,close_to_junction,[self(),ets:first(junction)]),
  SensorPid3 = spawn(sensors,outOfRange,[self()]),
  SensorPid4 = spawn(sensors,car_accident,[self(),ets:first(cars)]),
  SensorPid5 = spawn(sensors,car_dev,[self()]),

  ets:insert(sensors,{SensorPid,self()}), ets:insert(sensors,{SensorPid2,self()}),
  ets:insert(sensors,{SensorPid3,self()}), ets:insert(sensors,{SensorPid4,self()}),
  ets:insert(sensors,{SensorPid5,self()}),
  put(sensor1 ,SensorPid), put(sensor2 ,SensorPid2),
  put(sensor3,SensorPid3), put(sensor4,SensorPid4),put(sensor5,SensorPid5),
  Monitor = State#cars_state.monitor,
  Monitor ! {add_to_monitor,SensorPid}, Monitor ! {add_to_monitor,SensorPid2},
  Monitor ! {add_to_monitor,SensorPid3}, Monitor ! {add_to_monitor,SensorPid4},Monitor ! {add_to_monitor,SensorPid5},
  NextStateName = drive_straight,
  {next_state, NextStateName, State,get(speed)}.

drive_straight(cast,{send,Who,From,Msg},State = #cars_state{})-> % send message to communication tower or to a different car
  {Bool1,To} = check_comms_d(Who,ets:first(comms)),
  case Bool1 of % if there is a close comm tower, send the message to it, else, send to a close car
    true -> communication_tower:receive_message(To,From,Msg);
    _-> {Bool2,To2} = check_close_car(Who,ets:first(cars),From),
      case Bool2 of
        true -> cars:send_msg(To2,{From,Msg});

        _-> [{_,[To3]}] = ets:lookup(comms,ets:first(comms)),
          communication_tower:receive_message(To3,From,Msg)
      end
  end,

  ets:update_element(cars,self(),[{6,{drive_straight}}]) ,
  NextStateName = drive_straight,
  {next_state, NextStateName, State,get(speed)};

drive_straight(cast,{ctc,Pid,OtherCar},State = #cars_state{}) -> % car got close to another car, send message to comm tower or another car
  {Bool1,To} = check_comms_d(Pid,ets:first(comms)), % check if there is a close communication tower
  case Bool1 of
    true -> communication_tower:receive_message(To,Pid,{s_close_to_car,OtherCar}); % if there is, send the message to it
    _-> {Bool2,To2} = check_close_car(Pid,ets:first(cars),Pid), % if there isn't check if there is a nearby car
      case Bool2 of
        true -> cars:send_msg(To2,{Pid,{s_close_to_car,OtherCar}}),
          spawn(cars,check_response,[self(),{s_close_to_car,OtherCar}]),
          io:format("sent message to ~p from ~p~n",[To2,Pid]);
        _->  server:s_close_to_car(null,Pid,OtherCar)
      end
  end,
  ets:update_element(cars,self(),[{6,{idle}}]) ,
  NextStateName = idle, ets:update_element(cars,self(),[{6,{idle}}]) ,
  {next_state, NextStateName, State,get(speed)};

drive_straight(cast,{ctj,Pid,T,{R,J},LP},_) -> % car got close to junction, send message to comm tower or another car and keep going\stop according to light
  case T of
    green -> NextStateName = idle,ets:update_element(cars,self(),[{6,{idle}}]) , % if the light is green, keep going straight in state idle and send message

      {Bool1,To} = check_comms_d(Pid,ets:first(comms)),% if there is a close comm tower, send the message to it, else, send to a close car
      case Bool1 of
        true -> communication_tower:receive_message(To,Pid,{s_light,{R,J}});
        _-> {Bool2,To2} = check_close_car(Pid,ets:first(cars),Pid),
          case Bool2 of
            true -> cars:send_msg(To2,{Pid,{s_light,{R,J}}}),
              spawn(cars,check_response,[self(),{s_light,{R,J}}]),
              io:format("sent message to ~p from ~p~n",[To2,Pid]);
            _->  server:s_light(null,Pid,{R,J})
          end
      end,
      {next_state, NextStateName, #cars_state{lightPid = LP},get(speed)};
    _ -> NextStateName = stopping,ets:update_element(cars,self(),[{6,{drive_straight}}]) , % if light is not green, stop and send message
      {Bool1,To} = check_comms_d(Pid,ets:first(comms)),% if there is a close comm tower, send the message to it, else, send to a close car
      case Bool1 of
        true -> communication_tower:receive_message(To,Pid,{s_light,{R,J}});
        _-> {Bool2,To2} = check_close_car(Pid,ets:first(cars),Pid),
          case Bool2 of
            true -> cars:send_msg(To2,{Pid,{s_light,{R,J}}}),
              spawn(cars,check_response,[self(),{s_light,{R,J}}]),
              io:format("sent message to ~p from ~p~n",[To2,Pid]);
            _->  server:s_light(null,Pid,{R,J})
          end
      end,
      {next_state, NextStateName, #cars_state{lightPid = LP}}

  end;

drive_straight(cast,{acc,Pid,_},_) -> % car got in an accident, kill sensors and car

  K1 = get(sensor1), K2 = get(sensor2), K3 = get(sensor3), K4 = get(sensor4),K5 = get(sensor5),
  exit(K1,kill),exit(K2,kill),exit(K3,kill),exit(K4,kill),exit(K5,kill), % kill sensors
  ets:delete(sensors,K1), ets:delete(sensors,K2), ets:delete(sensors,K3), ets:delete(sensors,K4),ets:delete(sensors,K5), % delete sensors from ets
  E1 = get(name),
  E2 = get(carMon),
  E3 = get(start),
  E4  = get(speed),
  timer:sleep(2500),
  server:deleteCar(Pid),
  server:deletesmoke(Pid),
  {stop,{accident,E1,E2,E3,E4}}; % send car data to monitor when dying



drive_straight(timeout,20,State = #cars_state{}) -> % go straight with speed 20, update ets according to direction
  [{P,[{X,Y},D,R,Type,Turn],_,_,_,_,_,_}] = ets:lookup(cars,self()),
  if
    D == up -> ets:update_element(cars,P,[{2,[{X,Y -1 },D,R,Type,Turn]}]) ; % check the direction and change the coordinates accordingly
    D == down ->ets:update_element(cars,P,[{2,[{X,Y +1 },D,R,Type,Turn]}]) ;
    D == right ->ets:update_element(cars,P,[{2,[{X + 1,Y },D,R,Type,Turn]}]) ;
    true -> ets:update_element(cars,P,[{2,[{X - 1,Y},D,R,Type,Turn]}])
  end,
  NextStateName = drive_straight,ets:update_element(cars,self(),[{6,{drive_straight}}]) ,
  {next_state, NextStateName, State,20};

drive_straight(timeout,10,State = #cars_state{}) -> % go straight with speed 10, update ets according to direction
  [{P,[{X,Y},D,R,Type,Turn],_,_,_,_,_,_}] = ets:lookup(cars,self()),
  if
    D == up -> ets:update_element(cars,P,[{2,[{X,Y -1 },D,R,Type,Turn]}]) ;% check the direction and change the coordinates accordingly
    D == down ->ets:update_element(cars,P,[{2,[{X,Y +1 },D,R,Type,Turn]}]) ;
    D == right ->ets:update_element(cars,P,[{2,[{X + 1,Y },D,R,Type,Turn]}]) ;
    true -> ets:update_element(cars,P,[{2,[{X - 1,Y},D,R,Type,Turn]}])
  end,
  NextStateName = drive_straight,ets:update_element(cars,self(),[{6,{drive_straight}}]) ,
  {next_state, NextStateName, State,10};

drive_straight(cast,{stop,_},State = #cars_state{}) -> % car receives stop message, goes to stop state
  NextStateName = stopping,ets:update_element(cars,self(),[{6,{drive_straight}}]) ,
  {next_state, NextStateName, State};

drive_straight(cast,{add_sensor,_,Sensor,Type},State = #cars_state{}) -> % revive sensor that has fallen
  case Type of
    close_to_car -> erase(sensor1), put(sensor1,Sensor);
    car_accident -> erase(sensor4), put(sensor4,Sensor)
  end,
  NextStateName = drive_straight,ets:update_element(cars,self(),[{6,{drive_straight}}]) ,
  {next_state, NextStateName, State,get(speed)};


drive_straight(cast,{switch,Pid,From,To},_) -> % car moved PC, kill sensors, delete them from ets and send message to server with relevant info to start in different PC
  K1 = get(sensor1), K2 = get(sensor2), K3 = get(sensor3), K4 = get(sensor4),K5 = get(sensor5),
  exit(K1,kill),exit(K2,kill),exit(K3,kill),exit(K4,kill),exit(K5,kill), % kill sensors
  ets:delete(sensors,K1), ets:delete(sensors,K2), ets:delete(sensors,K3), ets:delete(sensors,K4),ets:delete(sensors,K5), % delete sensors from ets

  E1 =get(name),
  E3 = get(start),
  E4  = get(speed),
  [{_,C,_,_,_,_,_,Nav}] = ets:lookup(cars,Pid),
  Con =  {drive_straight},
  case To of
    pc_1 -> server:deleteCar(Pid), {stop,{move_to_comp1,E1,E3,E4,C,From,To,Con,Nav}};
    pc_2 -> server:deleteCar(Pid), {stop,{move_to_comp2,E1,E3,E4,C,From,To,Con,Nav}};
    pc_3 -> server:deleteCar(Pid), {stop,{move_to_comp3,E1,E3,E4,C,From,To,Con,Nav}};
    pc_4 -> server:deleteCar(Pid), {stop,{move_to_comp4,E1,E3,E4,C,From,To,Con,Nav}}
  end;

drive_straight(cast,{kill,Pid},_) -> % car received kill message, kill sensors and die while sending car details to monitor
  K1 = get(sensor1), K2 = get(sensor2), K3 = get(sensor3), K4 = get(sensor4),K5 = get(sensor5),
  exit(K1,kill),exit(K2,kill),exit(K3,kill),exit(K4,kill),exit(K5,kill), % kill sensors
  ets:delete(sensors,K1), ets:delete(sensors,K2), ets:delete(sensors,K3), ets:delete(sensors,K4),ets:delete(sensors,K5), % delete sensors from ets
  E1 =get(name),
  E2 = get(carMon),
  E3 = get(start),
  E4  = get(speed),
  server:deleteCar(Pid),
  {stop,{outOfRange,E1,E2,E3,E4}};

drive_straight(cast,_,State = #cars_state{}) -> % remove unwanted messages
  NextStateName = drive_straight,ets:update_element(cars,self(),[{6,{drive_straight}}]) ,
  {next_state, NextStateName, State,get(speed)}.


idle(cast,{byp,Pid},State = #cars_state{}) -> % car received bypass message, go to bypassing state
  ets:update_element(cars,Pid,[{6,{bypassing,State#cars_state.bypassCounter}}]) ,
  NextStateName = bypassing,
  {next_state, NextStateName, State,get(speed)};

idle(cast,{acc,Pid,_},_) -> % car got in accident
  K1 = get(sensor1), K2 = get(sensor2), K3 = get(sensor3), K4 = get(sensor4),K5 = get(sensor5),
  exit(K1,kill),exit(K2,kill),exit(K3,kill),exit(K4,kill),exit(K5,kill), % kill sensors
  ets:delete(sensors,K1), ets:delete(sensors,K2), ets:delete(sensors,K3), ets:delete(sensors,K4),ets:delete(sensors,K5), % delete sensors from ets
  E1 = get(name),
  E2 = get(carMon),
  E3 = get(start),
  E4  = get(speed),
  timer:sleep(2500),
  server:deleteCar(Pid),
  server:deletesmoke(Pid),
  {stop,{accident,E1,E2,E3,E4}};% send car data to monitor when dying

idle(timeout,20,State = #cars_state{}) -> % keep going straight until different event has happened
  [{P,[{X,Y},D,R,Type,Turn],_,_,_,_,_,_}] = ets:lookup(cars,self()),
  if
    D == up -> ets:update_element(cars,P,[{2,[{X,Y -1 },D,R,Type,Turn]}]) ;% check the direction and change the coordinates accordingly
    D == down ->ets:update_element(cars,P,[{2,[{X,Y +1 },D,R,Type,Turn]}]) ;
    D == right ->ets:update_element(cars,P,[{2,[{X + 1,Y },D,R,Type,Turn]}]) ;
    true -> ets:update_element(cars,P,[{2,[{X - 1,Y},D,R,Type,Turn]}])
  end,
  NextStateName = idle, ets:update_element(cars,self(),[{6,{idle}}]) ,
  {next_state, NextStateName, State,20};

idle(timeout,10,State = #cars_state{}) ->
  [{P,[{X,Y},D,R,Type,Turn],_,_,_,_,_,_}] = ets:lookup(cars,self()),
  if
    D == up -> ets:update_element(cars,P,[{2,[{X,Y -1 },D,R,Type,Turn]}]) ;% check the direction and change the coordinates accordingly
    D == down ->ets:update_element(cars,P,[{2,[{X,Y +1 },D,R,Type,Turn]}]) ;
    D == right ->ets:update_element(cars,P,[{2,[{X + 1,Y },D,R,Type,Turn]}]) ;
    true -> ets:update_element(cars,P,[{2,[{X - 1,Y},D,R,Type,Turn]}])
  end,
  NextStateName = idle, ets:update_element(cars,self(),[{6,{idle}}]) ,
  {next_state, NextStateName, State,10};

idle(cast,{turn,_,{Dir, Road}},State = #cars_state{}) -> % car received turn message, check if it should go straight or turn and change states accordingly
  [{_,[{_,_},D,_,_,_],_,_,_,_,_,_}] = ets:lookup(cars,self()),
  case D == Dir of
    true ->NextStateName1 = drive_straight, % if the car receives the same direction that it is already going, go straight
      {next_state, NextStateName1, State,get(speed)};
    _ ->  NextStateName = turning,ets:update_element(cars,self(),[{6,{turning,State#cars_state.turnCounter,Dir,Road}}]), % if the direction is different, go to turning
      {next_state, NextStateName, #cars_state{nextTurnDir = Dir,nextTurnRoad = Road},get(speed)}

  end;

idle(cast,{stop,_,OtherCar},State = #cars_state{}) -> % car received stop message when close to another car, spawn a sensor and go to stop state
  SensorPid = spawn(sensors,far_from_car,[self(),OtherCar]), % spawn far from car sensor
  ets:insert(sensors,{SensorPid,self()}), % insert to ets
  Monitor = get(carMon),
  Monitor ! {add_to_monitor,SensorPid}, % add sensor to monitor
  NextStateName = stopping, ets:update_element(cars,self(),[{6,{drive_straight}}]) , % go to stopping
  {next_state, NextStateName, State};

idle(cast,{send,Who,From,Msg},State = #cars_state{})-> % send message to another car or comm tower
  {Bool1,To} = check_comms_d(Who,ets:first(comms)),% if there is a close comm tower, send the message to it, else, send to a close car
  case Bool1 of
    true -> communication_tower:receive_message(To,From,Msg);
    _-> {Bool2,To2} = check_close_car(Who,ets:first(cars),From),
      case Bool2 of
        true -> cars:send_msg(To2,{From,Msg});
        _->[{_,[To3]}] = ets:lookup(comms,ets:first(comms)),
          communication_tower:receive_message(To3,From,Msg)
      end
  end,
  NextStateName = idle, ets:update_element(cars,self(),[{6,{idle}}]) ,
  {next_state, NextStateName, State,get(speed)};

idle(cast,{add_sensor,_,Sensor,Type},State = #cars_state{}) -> % revive fallen sensor
  case Type of
    close_to_car -> erase(sensor1), put(sensor1,Sensor);
    car_accident -> erase(sensor4), put(sensor4,Sensor)
  end,
  NextStateName = idle, ets:update_element(cars,self(),[{6,{idle}}]) ,
  {next_state, NextStateName, State,get(speed)};

idle(cast,{switch,Pid,From,To},_) -> % switch PC
  K1 = get(sensor1), K2 = get(sensor2), K3 = get(sensor3), K4 = get(sensor4),K5 = get(sensor5),
  exit(K1,kill),exit(K2,kill),exit(K3,kill),exit(K4,kill),exit(K5,kill), % kill sensors
  ets:delete(sensors,K1), ets:delete(sensors,K2), ets:delete(sensors,K3), ets:delete(sensors,K4),ets:delete(sensors,K5),  % delete sensors from ets
  Con =  {idle},
  E1 =get(name),
  E3 = get(start),
  E4  = get(speed),
  [{_,C,_,_,_,_,_,Nav}] = ets:lookup(cars,Pid),
  case To of
    pc_1 -> server:deleteCar(Pid), {stop,{move_to_comp1,E1,E3,E4,C,From,To,Con,Nav}};
    pc_2 -> server:deleteCar(Pid), {stop,{move_to_comp2,E1,E3,E4,C,From,To,Con,Nav}};
    pc_3 -> server:deleteCar(Pid), {stop,{move_to_comp3,E1,E3,E4,C,From,To,Con,Nav}};
    pc_4 -> server:deleteCar(Pid), {stop,{move_to_comp4,E1,E3,E4,C,From,To,Con,Nav}}
  end;



idle(cast,{kill,Pid},_) -> % kill car
  K1 = get(sensor1), K2 = get(sensor2), K3 = get(sensor3), K4 = get(sensor4),K5 = get(sensor5),
  exit(K1,kill),exit(K2,kill),exit(K3,kill),exit(K4,kill),exit(K5,kill), % kill sensors
  ets:delete(sensors,K1), ets:delete(sensors,K2), ets:delete(sensors,K3), ets:delete(sensors,K4),ets:delete(sensors,K5), % delete sensors from ets
  E1 =get(name),
  E2 = get(carMon),
  E3 = get(start),
  E4  = get(speed),
  server:deleteCar(Pid),
  {stop,{outOfRange,E1,E2,E3,E4}};

idle(cast,{ctc,_,_},State = #cars_state{}) -> % if close to car was received in idle, ignore it
  NextStateName = idle, ets:update_element(cars,self(),[{6,{idle}}]) ,
  {next_state, NextStateName, State,get(speed)};

idle(cast,Else,State = #cars_state{}) -> % remove unwanted messages
  io:format("error in idle: ~p~n",[Else]),
  NextStateName = idle, ets:update_element(cars,self(),[{6,{idle}}]) ,
  {next_state, NextStateName, State,get(speed)}.

turning(timeout,10,State = #cars_state{}) -> % turn according to direction and update car ets
  [{P,[{X,Y},D,R,Type,_],_,_,_,_,_,_}] = ets:lookup(cars,self()), C =State#cars_state.turnCounter,
  Dir = State#cars_state.nextTurnDir,
  Road = State#cars_state.nextTurnRoad,
  if
    D == up, Dir == left, C =< 120 -> ets:update_element(cars,P,[{2,[{X,Y -1 },D,R,Type,st]}]), % update location and keep turning
      NextStateName = turning,ets:update_element(cars,self(),[{6,{turning,State#cars_state.turnCounter,State#cars_state.nextTurnDir,State#cars_state.nextTurnRoad}}]),
      {next_state, NextStateName, #cars_state{turnCounter = C + 1,nextTurnDir = Dir , nextTurnRoad = Road },10};
    D == up, Dir == right, C =< 75 ->ets:update_element(cars,P,[{2,[{X,Y -1 },D,R,Type,st]}]), % update location and keep turning
      NextStateName = turning,ets:update_element(cars,self(),[{6,{turning,State#cars_state.turnCounter,State#cars_state.nextTurnDir,State#cars_state.nextTurnRoad}}]),
      {next_state, NextStateName, #cars_state{turnCounter = C + 1,nextTurnDir = Dir , nextTurnRoad = Road },10};

    D == down, Dir == left, C =< 75 ->ets:update_element(cars,P,[{2,[{X,Y +1 },D,R,Type,st]}]),
      NextStateName = turning,ets:update_element(cars,self(),[{6,{turning,State#cars_state.turnCounter,State#cars_state.nextTurnDir,State#cars_state.nextTurnRoad}}]),
      {next_state, NextStateName, #cars_state{turnCounter = C + 1,nextTurnDir = Dir , nextTurnRoad = Road },10};
    D == down, Dir == right, C =< 120 -> ets:update_element(cars,P,[{2,[{X,Y +1 },D,R,Type,st]}]),
      NextStateName = turning,ets:update_element(cars,self(),[{6,{turning,State#cars_state.turnCounter,State#cars_state.nextTurnDir,State#cars_state.nextTurnRoad}}]),
      {next_state, NextStateName, #cars_state{turnCounter = C + 1,nextTurnDir = Dir , nextTurnRoad = Road },10};

    D == right, Dir == up, C =< 120 ->ets:update_element(cars,P,[{2,[{X + 1,Y },D,R,Type,st]}]),
      NextStateName = turning,ets:update_element(cars,self(),[{6,{turning,State#cars_state.turnCounter,State#cars_state.nextTurnDir,State#cars_state.nextTurnRoad}}]),
      {next_state, NextStateName, #cars_state{turnCounter = C + 1,nextTurnDir = Dir , nextTurnRoad = Road },10};
    D == right, Dir == down, C =< 75 -> ets:update_element(cars,P,[{2,[{X + 1,Y },D,R,Type,st]}]),
      NextStateName = turning,ets:update_element(cars,self(),[{6,{turning,State#cars_state.turnCounter,State#cars_state.nextTurnDir,State#cars_state.nextTurnRoad}}]),
      {next_state, NextStateName, #cars_state{turnCounter = C + 1,nextTurnDir = Dir , nextTurnRoad = Road },10};

    D == left, Dir == up, C =< 75 -> ets:update_element(cars,P,[{2,[{X - 1,Y },D,R,Type,st]}]) ,
      NextStateName = turning,ets:update_element(cars,self(),[{6,{turning,State#cars_state.turnCounter,State#cars_state.nextTurnDir,State#cars_state.nextTurnRoad}}]),
      {next_state, NextStateName, #cars_state{turnCounter = C + 1,nextTurnDir = Dir , nextTurnRoad = Road },10};
    D == left, Dir == down, C =< 120 -> ets:update_element(cars,P,[{2,[{X - 1,Y },D,R,Type,st]}]),
      NextStateName = turning,ets:update_element(cars,self(),[{6,{turning,State#cars_state.turnCounter,State#cars_state.nextTurnDir,State#cars_state.nextTurnRoad}}]),
      {next_state, NextStateName, #cars_state{turnCounter = C + 1,nextTurnDir = Dir , nextTurnRoad = Road },10};

    Dir == undefined ->
      Ans = server:error_in_turn(self()),

      if
        Ans == true  -> NextStateName = drive_straight,
          {next_state, NextStateName,State,10};
        true ->
          K1 = get(sensor1), K2 = get(sensor2), K3 = get(sensor3), K4 = get(sensor4),K5 = get(sensor5),
          exit(K1,kill),exit(K2,kill),exit(K3,kill),exit(K4,kill),exit(K5,kill), % kill sensors
          ets:delete(sensors,K1), ets:delete(sensors,K2), ets:delete(sensors,K3), ets:delete(sensors,K4),ets:delete(sensors,K5), % delete sensors from ets
          E1 =get(name),
          E2 = get(carMon),
          E3 = get(start),
          E4  = get(speed),

          server:deleteCar(self()),
          {stop,{outOfRange,E1,E2,E3,E4}}
      end;


    true ->   ets:update_element(cars,P,[{2,[{X ,Y },Dir,Road,Type,st]}]), % if the car finished turning, go to drive straight
      NextStateName = drive_straight, ets:update_element(cars,self(),[{6,{drive_straight}}]),
      {next_state, NextStateName, #cars_state{turnCounter = C + 1,nextTurnDir = Dir , nextTurnRoad = Road },10}

  end;

turning(timeout,20,State = #cars_state{}) ->
  [{P,[{X,Y},D,R,Type,_],_,_,_,_,_,_}] = ets:lookup(cars,self()),
  C =State#cars_state.turnCounter,
  Dir = State#cars_state.nextTurnDir,
  Road = State#cars_state.nextTurnRoad,


  if
    D == up, Dir == left, C =< 120 -> ets:update_element(cars,P,[{2,[{X,Y -1 },D,R,Type,st]}]),% update location and keep turning
      NextStateName = turning,ets:update_element(cars,self(),[{6,{turning,State#cars_state.turnCounter,State#cars_state.nextTurnDir,State#cars_state.nextTurnRoad}}]),
      {next_state, NextStateName, #cars_state{turnCounter = C + 1,nextTurnDir = Dir , nextTurnRoad = Road },20};
    D == up, Dir == right, C =< 75 ->ets:update_element(cars,P,[{2,[{X,Y -1 },D,R,Type,st]}]),% update location and keep turning
      NextStateName = turning,ets:update_element(cars,self(),[{6,{turning,State#cars_state.turnCounter,State#cars_state.nextTurnDir,State#cars_state.nextTurnRoad}}]),
      {next_state, NextStateName, #cars_state{turnCounter = C + 1,nextTurnDir = Dir , nextTurnRoad = Road },20};

    D == down, Dir == left, C =< 75 ->ets:update_element(cars,P,[{2,[{X,Y +1 },D,R,Type,st]}]),
      NextStateName = turning,ets:update_element(cars,self(),[{6,{turning,State#cars_state.turnCounter,State#cars_state.nextTurnDir,State#cars_state.nextTurnRoad}}]),
      {next_state, NextStateName, #cars_state{turnCounter = C + 1,nextTurnDir = Dir , nextTurnRoad = Road },20};
    D == down, Dir == right, C =< 120 -> ets:update_element(cars,P,[{2,[{X,Y +1 },D,R,Type,st]}]),
      NextStateName = turning,ets:update_element(cars,self(),[{6,{turning,State#cars_state.turnCounter,State#cars_state.nextTurnDir,State#cars_state.nextTurnRoad}}]),
      {next_state, NextStateName, #cars_state{turnCounter = C + 1,nextTurnDir = Dir , nextTurnRoad = Road },20};

    D == right, Dir == up, C =< 120 ->ets:update_element(cars,P,[{2,[{X + 1,Y },D,R,Type,st]}]) ,
      NextStateName = turning,ets:update_element(cars,self(),[{6,{turning,State#cars_state.turnCounter,State#cars_state.nextTurnDir,State#cars_state.nextTurnRoad}}]),
      {next_state, NextStateName, #cars_state{turnCounter = C + 1,nextTurnDir = Dir , nextTurnRoad = Road },20};
    D == right, Dir == down, C =< 75 -> ets:update_element(cars,P,[{2,[{X + 1,Y },D,R,Type,st]}]),
      NextStateName = turning,ets:update_element(cars,self(),[{6,{turning,State#cars_state.turnCounter,State#cars_state.nextTurnDir,State#cars_state.nextTurnRoad}}]),
      {next_state, NextStateName, #cars_state{turnCounter = C + 1,nextTurnDir = Dir , nextTurnRoad = Road },20};

    D == left, Dir == up, C =< 75 -> ets:update_element(cars,P,[{2,[{X - 1,Y },D,R,Type,st]}]),
      NextStateName = turning,ets:update_element(cars,self(),[{6,{turning,State#cars_state.turnCounter,State#cars_state.nextTurnDir,State#cars_state.nextTurnRoad}}]),
      {next_state, NextStateName, #cars_state{turnCounter = C + 1,nextTurnDir = Dir , nextTurnRoad = Road },20};
    D == left, Dir == down, C =< 120 -> ets:update_element(cars,P,[{2,[{X - 1,Y },D,R,Type,st]}]),
      NextStateName = turning,ets:update_element(cars,self(),[{6,{turning,State#cars_state.turnCounter,State#cars_state.nextTurnDir,State#cars_state.nextTurnRoad}}]),
      {next_state, NextStateName, #cars_state{turnCounter = C + 1,nextTurnDir = Dir , nextTurnRoad = Road },20};

    Dir == undefined ->
      Ans = server:error_in_turn(self()),
      if
        Ans == true  -> NextStateName = drive_straight,
          {next_state, NextStateName,State,20};
        true ->
          K1 = get(sensor1), K2 = get(sensor2), K3 = get(sensor3), K4 = get(sensor4),K5 = get(sensor5),
          exit(K1,kill),exit(K2,kill),exit(K3,kill),exit(K4,kill),exit(K5,kill), % kill sensors
          ets:delete(sensors,K1), ets:delete(sensors,K2), ets:delete(sensors,K3), ets:delete(sensors,K4),ets:delete(sensors,K5), % delete sensors from ets
          E1 =get(name),
          E2 = get(carMon),
          E3 = get(start),
          E4  = get(speed),

          server:deleteCar(self()),
          {stop,{outOfRange,E1,E2,E3,E4}}
      end;


    true -> ets:update_element(cars,P,[{2,[{X ,Y },Dir,Road,Type,st]}]),
      NextStateName = drive_straight, ets:update_element(cars,self(),[{6,{drive_straight}}]),
      {next_state, NextStateName, #cars_state{turnCounter = C + 1,nextTurnDir = Dir , nextTurnRoad = Road },20}
  end;

turning(cast,{ctj,_,_,_,_},State = #cars_state{}) -> % car is close to junction, keep turning
  Dir = State#cars_state.nextTurnDir, Road = State#cars_state.nextTurnRoad, C = State#cars_state.turnCounter,
  NextStateName = turning,ets:update_element(cars,self(),[{6,{turning,State#cars_state.turnCounter,State#cars_state.nextTurnDir,State#cars_state.nextTurnRoad}}]),
  {next_state, NextStateName, #cars_state{turnCounter = C ,nextTurnDir = Dir , nextTurnRoad = Road },get(speed)};


turning(cast,{ctc,_,_},State = #cars_state{}) -> % if the car is close to another car, keep turning
  Dir = State#cars_state.nextTurnDir, Road = State#cars_state.nextTurnRoad, C = State#cars_state.turnCounter,
  NextStateName = turning,ets:update_element(cars,self(),[{6,{turning,State#cars_state.turnCounter,State#cars_state.nextTurnDir,State#cars_state.nextTurnRoad}}]),
  {next_state, NextStateName, #cars_state{turnCounter = C ,nextTurnDir = Dir , nextTurnRoad = Road },get(speed)};

turning(cast,{send,Who,From,Msg},State = #cars_state{})-> % send message to another car or to comm tower
  C =State#cars_state.turnCounter,
  Dir = State#cars_state.nextTurnDir,
  Road = State#cars_state.nextTurnRoad,
  {Bool1,To} = check_comms_d(Who,ets:first(comms)),% if there is a close comm tower, send the message to it, else, send to a close car
  case Bool1 of
    true -> communication_tower:receive_message(To,From,Msg);
    _-> {Bool2,To2} = check_close_car(Who,ets:first(cars),From),
      case Bool2 of
        true -> cars:send_msg(To2,{From,Msg});
        _->  [{_,[To3]}] = ets:lookup(comms,ets:first(comms)),
          communication_tower:receive_message(To3,From,Msg)
      end
  end,


  NextStateName = turning,ets:update_element(cars,self(),[{6,{turning,State#cars_state.turnCounter,State#cars_state.nextTurnDir,State#cars_state.nextTurnRoad}}])  ,
  {next_state, NextStateName,#cars_state{turnCounter = C ,nextTurnDir = Dir , nextTurnRoad = Road },get(speed)};

turning(cast,{acc,Pid,_},_) -> % car got in an accident
  K1 = get(sensor1), K2 = get(sensor2), K3 = get(sensor3), K4 = get(sensor4),K5 = get(sensor5),
  exit(K1,kill),exit(K2,kill),exit(K3,kill),exit(K4,kill),exit(K5,kill), % kill sensors
  ets:delete(sensors,K1), ets:delete(sensors,K2), ets:delete(sensors,K3), ets:delete(sensors,K4),ets:delete(sensors,K5), % delete sensors from ets
  E1 = get(name),
  E2 = get(carMon),
  E3 = get(start),
  E4  = get(speed),
  timer:sleep(2500),
  server:deleteCar(Pid),
  server:deletesmoke(Pid),
  {stop,{accident,E1,E2,E3,E4}};% send car data to monitor when dying

turning(cast,{add_sensor,_,Sensor,Type},State = #cars_state{}) -> % revive fallen sensor
  C =State#cars_state.turnCounter,
  Dir = State#cars_state.nextTurnDir,
  Road = State#cars_state.nextTurnRoad,
  case Type of
    close_to_car -> erase(sensor1), put(sensor1,Sensor);
    car_accident -> erase(sensor4), put(sensor4,Sensor)
  end,
  NextStateName = turning,ets:update_element(cars,self(),[{6,{turning,State#cars_state.turnCounter,State#cars_state.nextTurnDir,State#cars_state.nextTurnRoad}}])  ,
  {next_state, NextStateName, State = #cars_state{turnCounter = C,nextTurnRoad = Road,nextTurnDir = Dir},get(speed)};

turning(cast,{switch,Pid,From,To},State = #cars_state{}) -> % switch PCs
  K1 = get(sensor1), K2 = get(sensor2), K3 = get(sensor3), K4 = get(sensor4),K5 = get(sensor5),
  exit(K1,kill),exit(K2,kill),exit(K3,kill),exit(K4,kill),exit(K5,kill), % kill sensors
  ets:delete(sensors,K1), ets:delete(sensors,K2), ets:delete(sensors,K3), ets:delete(sensors,K4),ets:delete(sensors,K5),  % delete sensors from ets
  Con =  {turning,State#cars_state.turnCounter,State#cars_state.nextTurnDir,State#cars_state.nextTurnRoad},
  E1 =get(name),
  E3 = get(start),
  E4  = get(speed),
  [{_,C,_,_,_,_,_,Nav}] = ets:lookup(cars,Pid),

  case To of
    pc_1 -> server:deleteCar(Pid), {stop,{move_to_comp1,E1,E3,E4,C,From,To,Con,Nav}};
    pc_2 -> server:deleteCar(Pid), {stop,{move_to_comp2,E1,E3,E4,C,From,To,Con,Nav}};
    pc_3 -> server:deleteCar(Pid), {stop,{move_to_comp3,E1,E3,E4,C,From,To,Con,Nav}};
    pc_4 -> server:deleteCar(Pid), {stop,{move_to_comp4,E1,E3,E4,C,From,To,Con,Nav}}
  end;

turning(cast,{kill,Pid},_) -> % kill car
  K1 = get(sensor1), K2 = get(sensor2), K3 = get(sensor3), K4 = get(sensor4),K5 = get(sensor5),
  exit(K1,kill),exit(K2,kill),exit(K3,kill),exit(K4,kill),exit(K5,kill), % kill sensors
  ets:delete(sensors,K1), ets:delete(sensors,K2), ets:delete(sensors,K3), ets:delete(sensors,K4),ets:delete(sensors,K5),  % delete sensors from ets
  E1 =get(name),
  E2 = get(carMon),
  E3 = get(start),
  E4  = get(speed),

  server:deleteCar(Pid),
  {stop,{outOfRange,E1,E2,E3,E4}};

turning(cast,Else,State = #cars_state{}) -> % remove unwanted messages
  C =State#cars_state.turnCounter,
  Dir = State#cars_state.nextTurnDir,
  Road = State#cars_state.nextTurnRoad,
  io:format("error in turning: ~p~n",[Else]),
  NextStateName = turning,ets:update_element(cars,self(),[{6,{turning,State#cars_state.turnCounter,State#cars_state.nextTurnDir,State#cars_state.nextTurnRoad}}])  ,
  {next_state, NextStateName,#cars_state{turnCounter = C ,nextTurnDir = Dir , nextTurnRoad = Road },get(speed)}.

stopping(cast,{turn,_,{Dir, Road}},State = #cars_state{}) -> % receive turn message, check light state and go to timeout event
  LP = State#cars_state.lightPid,
  NextStateName = stopping, ets:update_element(cars,self(),[{6,{drive_straight}}]) ,
  {next_state, NextStateName, #cars_state{nextTurnDir = Dir,nextTurnRoad = Road,lightPid = LP},get(speed)};

stopping(timeout,20,State = #cars_state{}) -> % checks light state, if green keep straight or turn, else go back to timeout event
  LP = State#cars_state.lightPid, Dir = State#cars_state.nextTurnDir, Road = State#cars_state.nextTurnRoad,
  case sys:get_state(LP) of
    {green,_} ->   [{_,[{_,_},D,_,_,_],_,_,_,_,_,_}] = ets:lookup(cars,self()), % if light is green check the direction and drive straight or turn
      case D == Dir of
        true ->NextStateName1 = drive_straight,
          {next_state, NextStateName1, State,get(speed)};
        _ ->  NextStateName = turning,ets:update_element(cars,self(),[{6,{turning,State#cars_state.turnCounter,Dir,Road}}])  ,
          {next_state, NextStateName, #cars_state{nextTurnDir = Dir,nextTurnRoad = Road},get(speed)}
      end;
    _ -> NextStateName = stopping, ets:update_element(cars,self(),[{6,{drive_straight}}]) , % if light isn't green, stay in stopping
      {next_state, NextStateName, #cars_state{nextTurnDir = Dir,nextTurnRoad = Road, lightPid = LP},20}
  end ;

stopping(timeout,10,State = #cars_state{}) ->% checks light state, if green keep straight or turn, else go back to timeout event
  LP = State#cars_state.lightPid, Dir = State#cars_state.nextTurnDir, Road = State#cars_state.nextTurnRoad,
  case sys:get_state(LP) of
    {green,_} ->   [{_,[{_,_},D,_,_,_],_,_,_,_,_,_}] = ets:lookup(cars,self()),% if light is green check the direction and drive straight or turn
      case D == Dir of
        true ->NextStateName1 = drive_straight,
          {next_state, NextStateName1, State,get(speed)};
        _ ->  NextStateName = turning,ets:update_element(cars,self(),[{6,{turning,State#cars_state.turnCounter,Dir,Road}}])  ,
          {next_state, NextStateName, #cars_state{nextTurnDir = Dir,nextTurnRoad = Road},get(speed)}

      end;
    _ -> NextStateName = stopping, ets:update_element(cars,self(),[{6,{drive_straight}}]) , % if light isn't green, stay in stopping
      {next_state, NextStateName, #cars_state{nextTurnDir = Dir,nextTurnRoad = Road, lightPid = LP},10}
  end ;

stopping(cast,{ctj,_,_,_,LP},State = #cars_state{}) -> % car is close to junction while stopping, check light and go to timeout event
  Dir = State#cars_state.nextTurnDir, Road = State#cars_state.nextTurnRoad,
  NextStateName = stopping, ets:update_element(cars,self(),[{6,{drive_straight}}]) ,
  {next_state, NextStateName, #cars_state{nextTurnDir = Dir,nextTurnRoad = Road, lightPid = LP},get(speed)};

stopping(cast,{far,_},State = #cars_state{}) -> % car got far from car event, go straight
  NextStateName = drive_straight, ets:update_element(cars,self(),[{6,{drive_straight}}]) ,
  {next_state, NextStateName, State,get(speed)};

stopping(cast,{acc,Pid,_},_) -> % car got in an accident
  K1 = get(sensor1), K2 = get(sensor2), K3 = get(sensor3), K4 = get(sensor4),K5 = get(sensor5),
  exit(K1,kill),exit(K2,kill),exit(K3,kill),exit(K4,kill),exit(K5,kill), % kill sensors
  ets:delete(sensors,K1), ets:delete(sensors,K2), ets:delete(sensors,K3), ets:delete(sensors,K4),ets:delete(sensors,K5),  % delete sensors from ets
  E1 = get(name),
  E2 = get(carMon),
  E3 = get(start),
  E4  = get(speed),
  timer:sleep(2500),
  server:deleteCar(Pid),
  server:deletesmoke(Pid),
  {stop,{accident,E1,E2,E3,E4}};% send car data to monitor when dying

stopping(cast,{send,Who,From,Msg},State = #cars_state{})-> % send message to another car or to comm tower
  LP = State#cars_state.lightPid, Dir = State#cars_state.nextTurnDir, Road = State#cars_state.nextTurnRoad,
  {Bool1,To} = check_comms_d(Who,ets:first(comms)),% if there is a close comm tower, send the message to it, else, send to a close car
  case Bool1 of
    true -> communication_tower:receive_message(To,From,Msg);
    _-> {Bool2,To2} = check_close_car(Who,ets:first(cars),From),
      case Bool2 of
        true -> cars:send_msg(To2,{From,Msg});
        _->  [{_,[To3]}] = ets:lookup(comms,ets:first(comms)),
          communication_tower:receive_message(To3,From,Msg)
      end
  end,
  case Dir of
    undefined -> NextStateName = stopping, ets:update_element(cars,self(),[{6,{drive_straight}}]) ,
      {next_state, NextStateName, State};
    _->  NextStateName = stopping, ets:update_element(cars,self(),[{6,{drive_straight}}]) ,
      {next_state, NextStateName, #cars_state{lightPid = LP, nextTurnDir = Dir,nextTurnRoad = Road },get(speed)}
  end;

stopping(cast,{add_sensor,_,Sensor,Type},State = #cars_state{}) -> % revive fallen sensor
  LP = State#cars_state.lightPid, Dir = State#cars_state.nextTurnDir, Road = State#cars_state.nextTurnRoad,
  case Type of
    close_to_car -> erase(sensor1), put(sensor1,Sensor);
    car_accident -> erase(sensor4), put(sensor4,Sensor)
  end,
  case Dir of
    undefined -> NextStateName = stopping, ets:update_element(cars,self(),[{6,{drive_straight}}]) ,
      {next_state, NextStateName, State};
    _->  NextStateName = stopping, ets:update_element(cars,self(),[{6,{drive_straight}}]) ,
      {next_state, NextStateName, #cars_state{lightPid = LP, nextTurnDir = Dir,nextTurnRoad = Road },get(speed)}
  end;

stopping(cast,{kill,Pid},_) -> % kill car
  K1 = get(sensor1), K2 = get(sensor2), K3 = get(sensor3), K4 = get(sensor4),K5 = get(sensor5),
  exit(K1,kill),exit(K2,kill),exit(K3,kill),exit(K4,kill),exit(K5,kill), % kill sensors
  ets:delete(sensors,K1), ets:delete(sensors,K2), ets:delete(sensors,K3), ets:delete(sensors,K4),ets:delete(sensors,K5),  % delete sensors from ets
  E1 =get(name),
  E2 = get(carMon),
  E3 = get(start),
  E4  = get(speed),

  server:deleteCar(Pid),
  {stop,{outOfRange,E1,E2,E3,E4}};

stopping(cast,_,State = #cars_state{}) -> % remove unwanted messages
  LP = State#cars_state.lightPid, Dir = State#cars_state.nextTurnDir, Road = State#cars_state.nextTurnRoad,
  NextStateName = stopping, ets:update_element(cars,self(),[{6,{drive_straight}}]) ,
  {next_state, NextStateName,#cars_state{lightPid = LP, nextTurnDir = Dir,nextTurnRoad = Road }}.

bypassing(cast,{ctc,_,_},_) -> % car is close to another car while bypassing, return to right lane
  NextStateName = bypassing, ets:update_element(cars,self(),[{6,{bypassing,280}}]) ,
  {next_state, NextStateName, #cars_state{bypassCounter = 280},get(speed)};

bypassing(cast,{ctj,_,T,{_,_},LP},State = #cars_state{}) -> % car is close to junction while bypassing, check light and keep bypassing if green or stop
  C =State#cars_state.bypassCounter,
  case T of
    {green,_} -> NextStateName = bypassing, ets:update_element(cars,self(),[{6,{bypassing,State#cars_state.bypassCounter}}]) ,
      {next_state, NextStateName, #cars_state{bypassCounter = C},get(speed)};

    _ -> NextStateName = bypassing, ets:update_element(cars,self(),[{6,{bypassing,State#cars_state.bypassCounter}}]) ,
      {next_state, NextStateName, #cars_state{lightPid = LP,bypassCounter = C},8}

  end;
bypassing(timeout,8,State = #cars_state{}) -> % check light and keep bypassing if green
  LP = State#cars_state.lightPid, C = State#cars_state.bypassCounter,
  case LP of
    nal ->  NextStateName = bypassing, ets:update_element(cars,self(),[{6,{bypassing,State#cars_state.bypassCounter}}]) , % if the junction doesn't have a traffic light, keep bypassing
      {next_state, NextStateName, #cars_state{bypassCounter = C},get(speed)};
    _ -> case sys:get_state(LP) of
           {green,_} -> NextStateName = bypassing, ets:update_element(cars,self(),[{6,{bypassing,State#cars_state.bypassCounter}}]) , % if the light is green, keep bypassing
             {next_state, NextStateName, #cars_state{bypassCounter = 100},10};
           _ -> NextStateName = bypassing, ets:update_element(cars,self(),[{6,{bypassing,State#cars_state.bypassCounter}}]) , % if light is not green, activate this event again
             {next_state, NextStateName, #cars_state{lightPid = LP},8}
         end
  end;


bypassing(cast,{acc,Pid,_},_) -> % car got in an accident
  K1 = get(sensor1), K2 = get(sensor2), K3 = get(sensor3), K4 = get(sensor4),K5 = get(sensor5),
  exit(K1,kill),exit(K2,kill),exit(K3,kill),exit(K4,kill),exit(K5,kill), % kill sensors
  ets:delete(sensors,K1), ets:delete(sensors,K2), ets:delete(sensors,K3), ets:delete(sensors,K4),ets:delete(sensors,K5), % delete sensors from ets
  E1 = get(name),
  E2 = get(carMon),
  E3 = get(start),
  E4  = get(speed),
  timer:sleep(2500),
  server:deleteCar(Pid),
  server:deletesmoke(Pid),
  {stop,{accident,E1,E2,E3,E4}};% send car data to monitor when dying

bypassing(timeout,20,State = #cars_state{}) -> % bypass a car - move to left lane, go straight and return to right lane
  [{P,[{X,Y},D,R,Type,Turn],_,_,_,_,_,_}] = ets:lookup(cars,self()), C =State#cars_state.bypassCounter,
  if
    D == up, C =< 26 -> ets:update_element(cars,P,[{2,[{X - 1,Y -1 },D,R,Type,Turn]}]), % if counter is lower than 26, keep moving to left lane
      NextStateName = bypassing, ets:update_element(cars,self(),[{6,{bypassing,State#cars_state.bypassCounter}}]);
    D == down, C =< 26 ->ets:update_element(cars,P,[{2,[{X + 1,Y +1 },D,R,Type,Turn]}]),
      NextStateName = bypassing, ets:update_element(cars,self(),[{6,{bypassing,State#cars_state.bypassCounter}}]);
    D == right, C =< 26 ->ets:update_element(cars,P,[{2,[{X + 1,Y - 1 },D,R,Type,Turn]}]),
      NextStateName = bypassing, ets:update_element(cars,self(),[{6,{bypassing,State#cars_state.bypassCounter}}]);
    D == left, C =< 26 ->ets:update_element(cars,P,[{2,[{X - 1,Y + 1},D,R,Type,Turn]}]),
      NextStateName = bypassing, ets:update_element(cars,self(),[{6,{bypassing,State#cars_state.bypassCounter}}]);


    D == up, C > 26, C =< 300 -> ets:update_element(cars,P,[{2,[{X ,Y -1 },D,R,Type,Turn]}]), % if counter is between 26 and 300, go straight in left lane
      NextStateName = bypassing, ets:update_element(cars,self(),[{6,{bypassing,State#cars_state.bypassCounter}}]);
    D == down, C > 26,C =< 300 ->ets:update_element(cars,P,[{2,[{X,Y +1 },D,R,Type,Turn]}]),
      NextStateName = bypassing, ets:update_element(cars,self(),[{6,{bypassing,State#cars_state.bypassCounter}}]);
    D == right, C > 26,C =< 300 ->ets:update_element(cars,P,[{2,[{X + 1,Y},D,R,Type,Turn]}]),
      NextStateName = bypassing, ets:update_element(cars,self(),[{6,{bypassing,State#cars_state.bypassCounter}}]);
    D == left, C > 26,C =< 300 ->ets:update_element(cars,P,[{2,[{X - 1,Y},D,R,Type,Turn]}]),
      NextStateName = bypassing, ets:update_element(cars,self(),[{6,{bypassing,State#cars_state.bypassCounter}}]);

    D == up, C > 300  ,C =< 326 -> ets:update_element(cars,P,[{2,[{X + 1 ,Y -1 },D,R,Type,Turn]}]), % if counter is between 300 and 326, go back to right lane
      NextStateName = bypassing, ets:update_element(cars,self(),[{6,{bypassing,State#cars_state.bypassCounter}}]);
    D == down, C > 300,C =< 326 ->ets:update_element(cars,P,[{2,[{X - 1,Y +1 },D,R,Type,Turn]}]),
      NextStateName = bypassing, ets:update_element(cars,self(),[{6,{bypassing,State#cars_state.bypassCounter}}]);
    D == right, C > 300,C =< 326 ->ets:update_element(cars,P,[{2,[{X + 1,Y + 1},D,R,Type,Turn]}]),
      NextStateName = bypassing, ets:update_element(cars,self(),[{6,{bypassing,State#cars_state.bypassCounter}}]);
    D == left, C > 300,C =< 326 ->ets:update_element(cars,P,[{2,[{X - 1,Y - 1},D,R,Type,Turn]}]),
      NextStateName = bypassing, ets:update_element(cars,self(),[{6,{bypassing,State#cars_state.bypassCounter}}]);


    true ->  NextStateName = drive_straight, ets:update_element(cars,self(),[{6,{drive_straight}}]) % if car finished bypassing, go to drive straight

  end,
  {next_state, NextStateName, #cars_state{bypassCounter = C + 1},20};

bypassing(timeout,10,State = #cars_state{}) ->
  [{P,[{X,Y},D,R,Type,Turn],_,_,_,_,_,_}] = ets:lookup(cars,self()), C =State#cars_state.bypassCounter,
  if
    D == up, C =< 26 -> ets:update_element(cars,P,[{2,[{X - 1,Y -1 },D,R,Type,Turn]}]),% if counter is lower than 26, keep moving to left lane
      NextStateName = bypassing, ets:update_element(cars,self(),[{6,{bypassing,State#cars_state.bypassCounter}}]);
    D == down, C =< 26 ->ets:update_element(cars,P,[{2,[{X + 1,Y +1 },D,R,Type,Turn]}]),
      NextStateName = bypassing, ets:update_element(cars,self(),[{6,{bypassing,State#cars_state.bypassCounter}}]);
    D == right, C =< 26 ->ets:update_element(cars,P,[{2,[{X + 1,Y - 1 },D,R,Type,Turn]}]),
      NextStateName = bypassing, ets:update_element(cars,self(),[{6,{bypassing,State#cars_state.bypassCounter}}]);
    D == left, C =< 26 ->ets:update_element(cars,P,[{2,[{X - 1,Y + 1},D,R,Type,Turn]}]),
      NextStateName = bypassing, ets:update_element(cars,self(),[{6,{bypassing,State#cars_state.bypassCounter}}]);


    D == up, C > 26, C =< 300 -> ets:update_element(cars,P,[{2,[{X ,Y -1 },D,R,Type,Turn]}]),% if counter is between 26 and 300, go straight in left lane
      NextStateName = bypassing, ets:update_element(cars,self(),[{6,{bypassing,State#cars_state.bypassCounter}}]);
    D == down, C > 26,C =< 300 ->ets:update_element(cars,P,[{2,[{X,Y +1 },D,R,Type,Turn]}]),
      NextStateName = bypassing, ets:update_element(cars,self(),[{6,{bypassing,State#cars_state.bypassCounter}}]);
    D == right, C > 26,C =< 300 ->ets:update_element(cars,P,[{2,[{X + 1,Y},D,R,Type,Turn]}]),
      NextStateName = bypassing, ets:update_element(cars,self(),[{6,{bypassing,State#cars_state.bypassCounter}}]);
    D == left, C > 26,C =< 300 ->ets:update_element(cars,P,[{2,[{X - 1,Y},D,R,Type,Turn]}]),
      NextStateName = bypassing, ets:update_element(cars,self(),[{6,{bypassing,State#cars_state.bypassCounter}}]);

    D == up, C > 300  ,C =< 326 -> ets:update_element(cars,P,[{2,[{X + 1 ,Y -1 },D,R,Type,Turn]}]),% if counter is between 300 and 326, go back to right lane
      NextStateName = bypassing, ets:update_element(cars,self(),[{6,{bypassing,State#cars_state.bypassCounter}}]);
    D == down, C > 300,C =< 326 ->ets:update_element(cars,P,[{2,[{X - 1,Y +1 },D,R,Type,Turn]}]),
      NextStateName = bypassing, ets:update_element(cars,self(),[{6,{bypassing,State#cars_state.bypassCounter}}]);
    D == right, C > 300,C =< 326 ->ets:update_element(cars,P,[{2,[{X + 1,Y + 1},D,R,Type,Turn]}]),
      NextStateName = bypassing, ets:update_element(cars,self(),[{6,{bypassing,State#cars_state.bypassCounter}}]);
    D == left, C > 300,C =< 326 ->ets:update_element(cars,P,[{2,[{X - 1,Y - 1},D,R,Type,Turn]}]),
      NextStateName = bypassing, ets:update_element(cars,self(),[{6,{bypassing,State#cars_state.bypassCounter}}]);


    true -> NextStateName = drive_straight, ets:update_element(cars,self(),[{6,{drive_straight}}])
  end,
  {next_state, NextStateName, #cars_state{bypassCounter = C + 1},10};

bypassing(cast,{send,Who,From,Msg},State = #cars_state{})-> % send message to another car or to comm tower
  C =State#cars_state.bypassCounter,
  {Bool1,To} = check_comms_d(Who,ets:first(comms)),% if there is a close comm tower, send the message to it, else, send to a close car
  case Bool1 of
    true -> communication_tower:receive_message(To,From,Msg);
    _-> {Bool2,To2} = check_close_car(Who,ets:first(cars),From),
      case Bool2 of
        true -> cars:send_msg(To2,{From,Msg});
        _->  [{_,[To3]}] = ets:lookup(comms,ets:first(comms)),
          communication_tower:receive_message(To3,From,Msg)
      end
  end,
  NextStateName = bypassing, ets:update_element(cars,self(),[{6,{bypassing,State#cars_state.bypassCounter}}]) ,
  {next_state, NextStateName, #cars_state{bypassCounter =  C},get(speed)};

bypassing(cast,{add_sensor,_,Sensor,Type},State = #cars_state{}) -> % revive fallen sensor
  case Type of
    close_to_car -> erase(sensor1), put(sensor1,Sensor);
    car_accident -> erase(sensor4), put(sensor4,Sensor)
  end,
  C =State#cars_state.bypassCounter,
  NextStateName = bypassing, ets:update_element(cars,self(),[{6,{bypassing,State#cars_state.bypassCounter}}]) ,
  {next_state, NextStateName, #cars_state{bypassCounter =  C},get(speed)};

bypassing(cast,{switch,Pid,From,To},State = #cars_state{}) -> % switch PC
  K1 = get(sensor1), K2 = get(sensor2), K3 = get(sensor3), K4 = get(sensor4),K5 = get(sensor5),
  exit(K1,kill),exit(K2,kill),exit(K3,kill),exit(K4,kill),exit(K5,kill), % kill sensors
  ets:delete(sensors,K1), ets:delete(sensors,K2), ets:delete(sensors,K3), ets:delete(sensors,K4),ets:delete(sensors,K5),  % delete sensors from ets

  Con =  {bypassing,State#cars_state.bypassCounter},
  E1 =get(name),
  E3 = get(start),
  E4  = get(speed),
  [{_,C,_,_,_,_,_,Nav}] = ets:lookup(cars,Pid),

  case To of
    pc_1 -> server:deleteCar(Pid), {stop,{move_to_comp1,E1,E3,E4,C,From,To,Con,Nav}};
    pc_2 -> server:deleteCar(Pid), {stop,{move_to_comp2,E1,E3,E4,C,From,To,Con,Nav}};
    pc_3 -> server:deleteCar(Pid), {stop,{move_to_comp3,E1,E3,E4,C,From,To,Con,Nav}};
    pc_4 -> server:deleteCar(Pid), {stop,{move_to_comp4,E1,E3,E4,C,From,To,Con,Nav}}
  end;

bypassing(cast,{kill,Pid},_) -> % kill car
  K1 = get(sensor1), K2 = get(sensor2), K3 = get(sensor3), K4 = get(sensor4),K5 = get(sensor5),
  exit(K1,kill),exit(K2,kill),exit(K3,kill),exit(K4,kill),exit(K5,kill), % kill sensors
  ets:delete(sensors,K1), ets:delete(sensors,K2), ets:delete(sensors,K3), ets:delete(sensors,K4),ets:delete(sensors,K5),  % delete sensors from ets
  E1 =get(name),
  E2 = get(carMon),
  E3 = get(start),
  E4  = get(speed),

  server:deleteCar(Pid),
  {stop,{outOfRange,E1,E2,E3,E4}};

bypassing(cast,Else,State = #cars_state{}) -> % remove unwanted messages
  io:format("error in bypassing: ~p~n",[Else]),
  C =State#cars_state.bypassCounter,
  NextStateName = bypassing, ets:update_element(cars,self(),[{6,{bypassing,State#cars_state.bypassCounter}}]) ,
  {next_state, NextStateName, #cars_state{bypassCounter =  C},get(speed)}.




%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2, cast/2, or as a normal
%% process message, this function is called.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Actions} |
%%                   {stop, Reason, NewState} |
%%    				 stop |
%%                   {stop, Reason :: term()} |
%%                   {stop, Reason :: term(), NewData :: data()} |
%%                   {stop_and_reply, Reason, Replies} |
%%                   {stop_and_reply, Reason, Replies, NewState} |
%%                   {keep_state, NewData :: data()} |
%%                   {keep_state, NewState, Actions} |
%%                   keep_state_and_data |
%%                   {keep_state_and_data, Actions}
%% @end
%%--------------------------------------------------------------------
handle_event(_EventType, _EventContent, _StateName, State) ->
  NextStateName = the_next_state_name,
  {next_state, NextStateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


% this function checks if there is a close communication tower and returns it if there is
check_comms_d(_,'$end_of_table') -> {false,nal}; % if no tower is close, return false
check_comms_d(Pid,Key)->
  [{_,[{X,Y},_,_,_,_],_,_,_,_,_,_}] = ets:lookup(cars,Pid),
  {X2,Y2} = Key,
  [{_,[To]}] = ets:lookup(comms,Key),

  D = math:sqrt(math:pow(X-X2,2) + math:pow(Y-Y2,2)),
  if
    D =< 150 -> {true,To}; % if the distance is low enough, return true and the tower pid
    true -> check_comms_d(Pid,ets:next(comms,Key)) % else, check next tower
  end.

% this function checks if there is a close car, if there is return it
check_close_car(_,'$end_of_table',_) -> {false,nal}; % if there is no close car, return false
check_close_car(Pid,Key,From)->
  [{_,[{X,Y},_,_,_,_],_,_,_,_,_,_}] = ets:lookup(cars,Pid),
  [{_,[{X2,Y2},_,_,_,_],_,_,_,_,_,_}] = ets:lookup(cars,Key),

  D = math:sqrt(math:pow(X-X2,2) + math:pow(Y-Y2,2)),
  if
    D =< 130, Pid /= Key, Key /= From -> {true,Key}; % if distance is low enough, return true
    true -> check_close_car(Pid,ets:next(cars,Key),From) % else, check next car
  end.


check_response(Pid,Msg) ->
  timer:sleep(3500),
  [{_,[To]}] = ets:lookup(comms,ets:first(comms)),
  Ans = ets:member(cars,Pid),
  if
    Ans == true -> case sys:get_state(Pid) of
                     {stopping,_} -> communication_tower:receive_message(To,Pid,Msg);
                     _-> ok
                   end;
    true -> ok
  end.


