%%%-------------------------------------------------------------------
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Jul 2023 
%%%-------------------------------------------------------------------
-module(server).


-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

% gen_server events
-export([s_close_to_car/3,s_light/3,start/0,start/5,
  deleteCar/1,deletePid/1,update_car_location/0,start_car/4,moved_car/7,update_monitor/1,smoke/4,deletesmoke/1,print_light/2,
  search_close_junc/2,update_car_nav/2,server_search_close_junc/2,light/4,checkBypass/3,checkBypass2/2,
  error_in_turn/1]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
start() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
start(PC1,PC2,PC3,PC4,Home) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [PC1,PC2,PC3,PC4,Home], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).

init([PC1,PC2,PC3,PC4,Home]) ->
  put(pc1,PC1), put(pc2,PC2), put(pc3,PC3), put(pc4,PC4), put(home,Home), % put PCs address in process dictionary
  ets:new(cars,[set,public,named_table]), % initialize new ets for cars

  net_kernel:connect_node(PC1), % connect PCs
  net_kernel:connect_node(PC2),
  net_kernel:connect_node(PC3),
  net_kernel:connect_node(PC4),

  ets:new(junction,[set,public,named_table]), % initialize ets for junctions and add all traffic lights

  traffic_light:start(r1a,{{r1,a},[{1137,120}]}),
  traffic_light:start(r1b,{{r1,b},[{938,120}]}),
  ets:insert(junction,{{r1,t},[{799,120},nal]}),
  traffic_light:start(r1c,{{r1,c},[{638,120}]}),
  ets:insert(junction,{{r1,s},[{420,120},nal]}),
  traffic_light:start(r1d,{{r1,d},[{302,120}]}),
  traffic_light:start(r1e,{{r1,e},[{164,120}]}),
  traffic_light:start(r2e,{{r2,e},[{128,75}]}),
  traffic_light:start(r2f,{{r2,f},[{128,355}]}),
  traffic_light:start(r2o,{{r2,o},[{128,590}]}),
  traffic_light:start(r3f,{{r3,f},[{81,418}]}),
  ets:insert(junction,{{r3,r},[{204,418},nal]}),
  traffic_light:start(r3g,{{r3,g},[{372,418}]}),
  traffic_light:start(r3h,{{r3,h},[{560,418}]}),
  traffic_light:start(r3i,{{r3,i},[{728,418}]}),
  ets:insert(junction,{{r3,u},[{860,418},nal]}),
  traffic_light:start(r3j,{{r3,j},[{1055,418}]}),
  traffic_light:start(r4l,{{r4,l},[{625,820}]}),
  traffic_light:start(r4m,{{r4,m},[{625,689}]}),
  traffic_light:start(r4h,{{r4,h},[{590,433}]}),
  traffic_light:start(r4c,{{r4,c},[{625,154}]}),
  traffic_light:start(r5k,{{r5,k},[{1058,640}]}),
  traffic_light:start(r6k,{{r6,k},[{1122,671}]}),
  traffic_light:start(r6j,{{r6,j},[{1122,434}]}),
  traffic_light:start(r6a,{{r6,a},[{1122,154}]}),
  traffic_light:start(r7l,{{r7,l},[{640,787}]}),
  traffic_light:start(r8d,{{r8,d},[{266,154}]}),
  traffic_light:start(r9o,{{r9,o},[{80,655}]}),
  traffic_light:start(r9n,{{r9,n},[{342,655}]}),
  traffic_light:start(r9m,{{r9,m},[{560,655}]}),
  traffic_light:start(r10i,{{r10,i},[{763,355}]}),
  ets:insert(junction,{{r12,p},[{902,590},nal]}),
  ets:insert(junction,{{r12,q},[{902,745},nal]}),
  traffic_light:start(r14n,{{r14,n},[{407,670}]}),
  traffic_light:start(r14g,{{r14,g},[{407,433}]}),
  traffic_light:start(r18b,{{r18,b},[{902,75}]}),

  FirstKey = ets:first(junction),
  KeyList = keys(junction, FirstKey, [FirstKey]),
  spawn(sensors,traffic_light_sensor,[KeyList,ets:first(junction)]), % spawn traffic light sensor

  ets:new(comms,[set,public,named_table]), % initialize ets for communication towers and insert all of them
  communication_tower:start(com1_1,{1121,111}),
  communication_tower:start(com1_2,{850,105}),
  communication_tower:start(com1_3,{838,377}),
  communication_tower:start(com1_4,{1124,341}),
  communication_tower:start(com2_1,{550,123}),
  communication_tower:start(com2_2,{219,120}),
  communication_tower:start(com2_3,{197,374}),
  communication_tower:start(com2_4,{480,395}),
  communication_tower:start(com3_1,{589,557}),
  communication_tower:start(com3_2,{392,623}),
  communication_tower:start(com3_3,{157,632}),
  communication_tower:start(com3_4,{561,784}),
  communication_tower:start(com4_1,{1025,519}),
  communication_tower:start(com4_2,{868,717}),
  communication_tower:start(com4_3,{1121,707}),

  CarMonitor = spawn(sensors,car_monitor,[PC1,PC2,PC3,PC4]), % spawn car monitor
  put(car_monitor,CarMonitor),

  ets:new(sensors,[set,public,named_table]), % initialize sensors ets
  roadGraph(), % make graph for roads
  {ok, #state{}}.



%% Events
s_light(Comm,Who,{R,J}) -> gen_server:cast(?MODULE,{light,Comm,Who,{R,J}}). % car is close to junction
s_close_to_car(Comm,Who,OtherCar) -> gen_server:cast(?MODULE,{ctc,Comm,Who,OtherCar}). % car is close to another car
deleteCar(Pid)-> gen_server:cast(?MODULE,{del,Pid}). % delete car from ets
deletePid(Pid)-> gen_server:cast(?MODULE,{delP,Pid}). % kill pid
update_car_location() -> gen_server:call(?MODULE,update_car). % main requests car ets
start_car(Name,Type,Start,PC)-> gen_server:cast(?MODULE,{start_car,Name,Type,Start,PC}). % initialize car process
moved_car(Name,Type,Start,Location,Con,PC,Nav) -> gen_server:cast(?MODULE,{movedCar,Name,Type,Start,Location,Con,PC,Nav}). % car moved from one PC to another
update_monitor(PC) -> gen_server:cast(?MODULE,{nodedown,PC}). % update the car monitor that a PC is down
smoke(Car1,L1,Car2,L2)-> gen_server:cast(?MODULE,{smoke,Car1,L1,Car2,L2}). % activate smoke 
deletesmoke(Pid) -> gen_server:cast(?MODULE,{delsmoke,Pid}). % delete smoke
print_light(X,Y) -> printTrafficLight(ets:first(junction),X,Y). % print traffic light state
update_car_nav(Pid,Dest) -> ets:update_element(cars,Pid,[{8,Dest}]). % update ets element with destination
server_search_close_junc(X,Y) -> gen_server:call(?MODULE,{server_search_close_junc,X,Y}). % search close junction
error_in_turn(Pid) -> gen_server:call(?MODULE,{error_in_turn,Pid}). % correct errors in turn



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_call(update_car, _, State) -> % return list of car ets to main
  Car = ets:tab2list(cars),
  {reply, {ok,Car}, State};

handle_call({server_search_close_junc,X,Y},_,State) -> % return a close enough junction
  Res = search_close_junc(ets:first(junction),{X,Y}),
  {reply, Res, State};

handle_call({error_in_turn,Pid},_,State) -> % return if car can go straight
  Res = checkTurn(Pid,ets:first(junction)),
  {reply, Res, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).



handle_cast({smoke,Car1,L1,Car2,L2},State) -> % send message to car monitor about a nodedown
  rpc:call(get(home),main,start_smoke,[Car1,L1,Car2,L2]),
  {noreply, State};

handle_cast({delsmoke,Pid},State) -> % send message to car monitor about a nodedown
  rpc:call(get(home),main,del_smoke,[Pid]),
  {noreply, State};


handle_cast({nodedown,PC},State) -> % send message to car monitor about a nodedown
  Pid = get(car_monitor),
  Pid ! {nodedown,PC},
  {noreply, State};

handle_cast({del,Pid},State) -> % call main to delete car from ets and delete from local ets
  timer:sleep(320),
  rpc:call(get(home),main,delete_car,[Pid]),
  ets:delete(cars,Pid),
  {noreply, State};

handle_cast({delP,Pid},State) -> % kill process with pid PID
  exit(Pid,kill),
  {noreply, State};

handle_cast({movedCar,Name,Type,Start,Location,Con,PC,Nav},State) -> % start a car in local PC when it moved into range
  cars:start(Name,get(car_monitor),Type,Start,Location,Con,PC,Nav),
  {noreply, State};

handle_cast({light,Comm,Who,{_,J}}, State) -> % decide whether the car turns left, right or straight
  spawn(server,light,[get(graph),Comm,Who,J]),
  {noreply, State};


handle_cast({ctc,Comm,Who,OtherCar}, State) -> % decide whether the car bypasses the other car or stops

  Ans = ets:member(cars,Who),
  Ans2 =ets:member(cars,OtherCar),
  if
    Ans == true, Ans2 == true ->

      [{_,_,_,_,_,_,_,Nav}] = ets:lookup(cars,Who),
      Bool1 = checkBypass(Who,OtherCar,ets:first(cars)), % check if the car can bypass
      Bool2 = checkBypass2(Who,ets:first(junction)),

      if
        Nav == null; Nav == in_process  -> Bool3 = true ;
        true -> Bool3 = false
      end,
      case {Bool1,Bool2,Bool3} of
        {true,true,true} -> % if it can, bypass
          case Comm of
            null -> cars:bypass(Who);
            _-> communication_tower:receive_message(Comm,Who,{bypass})
          end;

        _ ->     case Comm of % if it can't, stop
                   null -> cars:stop(Who,OtherCar);
                   _-> communication_tower:receive_message(Comm,Who,{stop,OtherCar})
                 end
      end,
      {noreply, State};
    true -> {noreply, State}
  end;

handle_cast({start_car,Name,Type,Start,PC},State) -> % starts car in local PC
  cars:start(Name,get(car_monitor),Type,Start,PC),
  {noreply, State};

handle_cast(Else,State) -> % starts car in local PC
  io:format("error in server: ~p~n",[Else]),
  {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

% this function checks if there is another car in front of the car he wants to bypass, and if there isn't, return true
checkBypass(_,_,'$end_of_table') -> true; % return true if no car is too close
checkBypass(Who,OtherCar,FirstKey) -> [{_,[{X,Y},Dir1,R,_,_],_,_,_,_,_,_}] =  ets:lookup(cars,Who),
  [{P2,[{X2,Y2},_,R2,_,_],_,_,_,_,_,_}] = ets:lookup(cars,FirstKey),
  if
    R == R2, P2 /= Who, P2 /= OtherCar ->
      case Dir1 of % checks all cars that are going the same direction and same road
        left -> D = X-X2, if
                            D =< 200 , D >= 0 -> false; % if the distance between cars is short enough, return false
                            true -> checkBypass(Who,OtherCar,ets:next(cars,P2)) % else, check next car
                          end;

        right ->  D = X2-X, if
                              D =< 200 , D >= 0 -> false;
                              true -> checkBypass(Who,OtherCar,ets:next(cars,P2))
                            end;
        up ->  D = Y-Y2, if
                           D =< 200 , D >= 0 -> false;
                           true -> checkBypass(Who,OtherCar,ets:next(cars,P2))
                         end;
        down -> D = Y2-Y, if
                            D =< 200 , D >= 0 -> false;
                            true -> checkBypass(Who,OtherCar,ets:next(cars,P2))
                          end
      end;

    true -> checkBypass(Who,OtherCar,ets:next(cars,FirstKey)) % if the car isn't going the same direction, check next car
  end.

% this function checks if there is a close junction when a car wants to bypass and if it can go straight
checkBypass2(_,'$end_of_table') -> true; 
checkBypass2(Who,Key) ->
  [{_,[{X,Y},Dir1,R,_,_],_,_,_,_,_,_}] =  ets:lookup(cars,Who),
  [{{R2,J},[{X2,Y2},_]}] = ets:lookup(junction,Key),
  case R == R2 of
    false -> checkBypass2(Who,ets:next(junction,Key));
    _ -> case Dir1 of % checks distance from junctions on the same road
           left -> D = X-X2, if

                               D >= 400  -> checkBypass2(Who,ets:next(junction,Key)); % if it's far, check the next junction
                               D >= 50 -> List =  digraph:out_neighbours(get(graph),J), % if it's close, check if it can go straight
                                 L = [getEdgeLabel(get(graph),digraph:out_edges(get(graph),J),E)||E <- List],
                                 L2 = [{Dir,Road}|| {Dir,Road} <- L, R==Road],
                                 case L2 of % if it can go straight, check next junction and if it can't, don't bypass
                                   [] -> false;
                                   _ -> checkBypass2(Who,ets:next(junction,Key))
                                 end;
                               D =< 0 -> checkBypass2(Who,ets:next(junction,Key));
                               true -> false % if junction is too close, don't bypass
                             end;

           right ->  D = X2-X, if
                                 D >= 400  -> checkBypass2(Who,ets:next(junction,Key));
                                 D >= 50 -> List =  digraph:out_neighbours(get(graph),J),
                                   L = [getEdgeLabel(get(graph),digraph:out_edges(get(graph),J),E)||E <- List],
                                   L2 = [{Dir,Road}|| {Dir,Road} <- L, R==Road],
                                   case L2 of
                                     [] -> false;
                                     _ -> checkBypass2(Who,ets:next(junction,Key))
                                   end;
                                 D =< 0 -> checkBypass2(Who,ets:next(junction,Key));
                                 true -> false
                               end;
           up ->  D = Y-Y2, if
                              D >= 400  -> checkBypass2(Who,ets:next(junction,Key));
                              D >= 50 -> List =  digraph:out_neighbours(get(graph),J),
                                L = [getEdgeLabel(get(graph),digraph:out_edges(get(graph),J),E)||E <- List],
                                L2 = [{Dir,Road}|| {Dir,Road} <- L, R==Road],
                                case L2 of
                                  [] -> false;
                                  _ -> checkBypass2(Who,ets:next(junction,Key))
                                end;
                              D =< 0 -> checkBypass2(Who,ets:next(junction,Key));
                              true ->false
                            end;
           down -> D = Y2-Y, if
                               D >= 400  -> checkBypass2(Who,ets:next(junction,Key));
                               D >= 50 -> List =  digraph:out_neighbours(get(graph),J),
                                 L = [getEdgeLabel(get(graph),digraph:out_edges(get(graph),J),E)||E <- List],
                                 L2 = [{Dir,Road}|| {Dir,Road} <- L, R==Road],
                                 case L2 of
                                   [] -> false;
                                   _ -> checkBypass2(Who,ets:next(junction,Key))
                                 end;
                               D =< 0 -> checkBypass2(Who,ets:next(junction,Key));
                               true ->false
                             end

         end
  end.

% this function checks if there is a close junction and if it can go straight 
checkTurn(_,'$end_of_table') -> true;
checkTurn(Who,Key) ->
  [{_,[{X,Y},Dir1,R,_,_],_,_,_,_,_,_}] =  ets:lookup(cars,Who),
  [{{R2,J},[{X2,Y2},_]}] = ets:lookup(junction,Key),
  case R == R2 of
    false -> checkTurn(Who,ets:next(junction,Key));
    _ -> case Dir1 of % checks distance from junctions on the same road
           left -> D = X-X2, if

                               D >= 400  -> checkTurn(Who,ets:next(junction,Key)); % if it's far, check the next junction
                               D =< 0 -> checkTurn(Who,ets:next(junction,Key));
                               true -> List =  digraph:out_neighbours(get(graph),J), % if it's close, check if it can go straight
                                 L = [getEdgeLabel(get(graph),digraph:out_edges(get(graph),J),E)||E <- List],
                                 L2 = [{Dir,Road}|| {Dir,Road} <- L, R==Road],
                                 case L2 of % if it can go straight, check next junction and if it can't, return false
                                   [] -> false;
                                   _ -> checkTurn(Who,ets:next(junction,Key))
                                 end
                             end;

           right ->  D = X2-X, if

                                 D >= 400  -> checkTurn(Who,ets:next(junction,Key)); % if it's far, check the next junction
                                 D =< 0 -> checkTurn(Who,ets:next(junction,Key));
                                 true -> List =  digraph:out_neighbours(get(graph),J), % if it's close, check if it can go straight
                                   L = [getEdgeLabel(get(graph),digraph:out_edges(get(graph),J),E)||E <- List],
                                   L2 = [{Dir,Road}|| {Dir,Road} <- L, R==Road],
                                   case L2 of % if it can go straight, check next junction and if it can't, return false
                                     [] -> false;
                                     _ -> checkTurn(Who,ets:next(junction,Key))
                                   end
                               end;
           up ->  D = Y-Y2, if

                              D >= 400  -> checkTurn(Who,ets:next(junction,Key)); % if it's far, check the next junction
                              D =< 0 -> checkTurn(Who,ets:next(junction,Key));
                              true -> List =  digraph:out_neighbours(get(graph),J), % if it's close, check if it can go straight
                                L = [getEdgeLabel(get(graph),digraph:out_edges(get(graph),J),E)||E <- List],
                                L2 = [{Dir,Road}|| {Dir,Road} <- L, R==Road],
                                case L2 of % if it can go straight, check next junction and if it can't, return false
                                  [] -> false;
                                  _ -> checkTurn(Who,ets:next(junction,Key))
                                end
                            end;
           down -> D = Y2-Y, if

                               D >= 400  -> checkTurn(Who,ets:next(junction,Key)); % if it's far, check the next junction
                               D =< 0 -> checkTurn(Who,ets:next(junction,Key));
                               true -> List =  digraph:out_neighbours(get(graph),J), % if it's close, check if it can go straight
                                 L = [getEdgeLabel(get(graph),digraph:out_edges(get(graph),J),E)||E <- List],
                                 L2 = [{Dir,Road}|| {Dir,Road} <- L, R==Road],
                                 case L2 of % if it can go straight, check next junction and if it can't, return false
                                   [] -> false;
                                   _ -> checkTurn(Who,ets:next(junction,Key))
                                 end
                             end

         end
  end.





% this function creates a graph with junctions as nodes and roads as edges
roadGraph()->
  G =  digraph:new(),
  digraph:add_vertex(G,a),
  digraph:add_vertex(G,b),
  digraph:add_vertex(G,c),
  digraph:add_vertex(G,d),
  digraph:add_vertex(G,e),
  digraph:add_vertex(G,f),
  digraph:add_vertex(G,g),
  digraph:add_vertex(G,h),
  digraph:add_vertex(G,i),
  digraph:add_vertex(G,j),
  digraph:add_vertex(G,k),
  digraph:add_vertex(G,l),
  digraph:add_vertex(G,m),
  digraph:add_vertex(G,n),
  digraph:add_vertex(G,o),
  digraph:add_vertex(G,p),
  digraph:add_vertex(G,q),
  digraph:add_vertex(G,r),
  digraph:add_vertex(G,s),
  digraph:add_vertex(G,t),
  digraph:add_vertex(G,u),
  digraph:add_vertex(G,"out1"),
  digraph:add_vertex(G,"out4"),
  digraph:add_vertex(G,"out6"),
  digraph:add_vertex(G,"out3"),
  digraph:add_vertex(G,"out5"),
  digraph:add_vertex(G,"out12"),
  digraph:add_vertex(G,"out2"),
  digraph:add_vertex(G,"out16"),
  digraph:add_vertex(G,"in2"),
  digraph:add_vertex(G,"in6"),
  digraph:add_vertex(G,"in9"),
  digraph:add_vertex(G,"in14"),
  digraph:add_vertex(G,"in4"),
  digraph:add_vertex(G,"in6"),
  digraph:add_vertex(G,"in1"),
  digraph:add_vertex(G,"in18"),
  digraph:add_edge(G,a,b,{left,r1}),
  digraph:add_edge(G,a,"out6",{up,r6}),
  digraph:add_edge(G,b,t,{left,r1}),
  digraph:add_edge(G,t,i,{down,r10}),
  digraph:add_edge(G,t,c,{left,r1}),
  digraph:add_edge(G,c,"out4",{up,r4}),
  digraph:add_edge(G,c,s,{left,r1}),
  digraph:add_edge(G,s,"out16",{up,r16}),
  digraph:add_edge(G,s,d,{left,r1}),
  digraph:add_edge(G,d,e,{left,r1}),
  digraph:add_edge(G,e,"out1",{left,r1}),
  digraph:add_edge(G,e,f,{down,r2}),
  digraph:add_edge(G,f,r,{right,r3}),
  digraph:add_edge(G,f,o,{down,r2}),
  digraph:add_edge(G,r,g,{right,r3}),
  digraph:add_edge(G,r,d,{up,r8}),
  digraph:add_edge(G,g,h,{right,r3}),
  digraph:add_edge(G,h,i,{right,r3}),
  digraph:add_edge(G,h,c,{up,r4}),
  digraph:add_edge(G,i,u,{right,r3}),
  digraph:add_edge(G,u,j,{right,r3}),
  digraph:add_edge(G,u,p,{down,r12}),
  digraph:add_edge(G,j,"out3",{right,r3}),
  digraph:add_edge(G,j,a,{up,r6}),
  digraph:add_edge(G,k,j,{up,r6}),
  digraph:add_edge(G,k,"out5",{right,r5}),
  digraph:add_edge(G,p,k,{right,r5}),
  digraph:add_edge(G,p,q,{down,r12}),
  digraph:add_edge(G,q,l,{left,r7}),
  digraph:add_edge(G,q,"out12",{down,r12}),
  digraph:add_edge(G,l,m,{up,r4}),
  digraph:add_edge(G,m,h,{up,r4}),
  digraph:add_edge(G,n,m,{right,r9}),
  digraph:add_edge(G,n,g,{up,r14}),
  digraph:add_edge(G,o,n,{right,r9}),
  digraph:add_edge(G,o,"out2",{down,r2}),

  put(graph,G).

% this function gets the label from and edge which includes the direction and road
getEdgeLabel(_,[],_) -> io:format("error");
getEdgeLabel(G,[H|T],V) ->
  {_,_,V2,Label} = digraph:edge(G,H),
  case V == V2 of
    true -> Label;
    _ -> getEdgeLabel(G,T,V)
  end.

% this function gets all keys of ets
keys(_TableName, '$end_of_table', ['$end_of_table'|Acc]) ->
  Acc;
keys(TableName, CurrentKey, Acc) ->
  NextKey = ets:next(TableName, CurrentKey),
  keys(TableName, NextKey, [NextKey|Acc]).

% this function search a close traffic light, in case there is a close traffic light the function print the traffic light color
printTrafficLight('$end_of_table',_,_) ->io:format("there is no close traffic light ~n"),
  ok;
printTrafficLight(Key,X,Y) -> 
  [{_,[{XP,YP},LightPid]}] =  ets:lookup(junction,Key),
  case LightPid of
    nal-> printTrafficLight(ets:next(junction,Key),X,Y); % if there is no traffic light in this junction, check next junction
    _->  D = math:sqrt(math:pow(X-XP ,2) + math:pow(Y-YP ,2)), 
      if 
        D =< 80 -> {C,_} =sys:get_state(LightPid), % if the distance between the traffic light and the click is short enough, print the color of the light
          io:format("the color of the traffic light is: ~p~n",[C]);
        true -> printTrafficLight(ets:next(junction,Key),X,Y) % else, check next junction
      end
  end.


% this function search a close junction, in case there is a close junction the function return his name
search_close_junc('$end_of_table',_)  -> null;
search_close_junc(Key,{X,Y}) -> [{{_,J},[{XP,YP},_]}] =  ets:lookup(junction,Key),
  D = math:sqrt(math:pow(X-(XP-10),2) + math:pow(Y-(YP-10),2)),
  if
    D =< 100 -> J; 
    true -> search_close_junc(ets:next(junction,Key),{X,Y})
  end.

% this function decides in which direction the car will continue and sends the response to the car
light(Graph,Comm,Who,J) ->   List =  digraph:out_neighbours(Graph,J), % get all possible directions the car can continue towards using digraph
  [{_,[_,_,_,_,_],_,_,_,_,_,Nav}] = ets:lookup(cars,Who),% get the cars navigation status
  case Nav of
    null   -> E = lists:nth(rand:uniform(length(List)),List),% in case the navigation status is null or in_process, pick a random direction
      {Dir, Road} = getEdgeLabel(Graph,digraph:out_edges(Graph,J),E),
      case Comm of
        null -> cars:turn(Who, {Dir, Road});
        _-> communication_tower:receive_message(Comm,Who,{turn,{Dir, Road}})
      end;

    in_process -> E = lists:nth(rand:uniform(length(List)),List), 
      {Dir, Road} = getEdgeLabel(Graph,digraph:out_edges(Graph,J),E),
      case Comm of
        null -> cars:turn(Who, {Dir, Road});
        _-> communication_tower:receive_message(Comm,Who,{turn,{Dir, Road}})
      end;

    Dest   -> if % in case the navigation status is a destination Junction and the car hasn't reached its destination yet, find a trail
                Dest /= J -> Trail = digraph:get_short_path(Graph,J,Dest),io:format("~p Trail : ~p~n",[Who,Trail]),
                  case Trail of
                    false -> io:format("The trail doesn't exist, pick a new junction"),ets:update_element(cars,Who,[{8,null}]), % in case the trail doesn't exist, pick a random direction
                      E = lists:nth(rand:uniform(length(List)),List),
                      {Dir, Road} = getEdgeLabel(Graph,digraph:out_edges(Graph,J),E),
                      case Comm of
                        null -> cars:turn(Who, {Dir, Road});
                        _-> communication_tower:receive_message(Comm,Who,{turn,{Dir, Road}})
                      end;
                    _->  Next = hd(tl(Trail)),% in case the trail does exist, get the next junction
                      {Dir, Road} = getEdgeLabel(Graph,digraph:out_edges(Graph,J),Next),
                      case Comm of
                        null -> cars:turn(Who, {Dir, Road});
                        _-> communication_tower:receive_message(Comm,Who,{turn,{Dir, Road}})
                      end
                  end;
                true -> ets:update_element(cars,Who,[{8,null}]),io:format("~p has reached its destination~n",[Who]),% in case the car reached its destination, pick a random direction
                  E = lists:nth(rand:uniform(length(List)),List),
                  {Dir, Road} = getEdgeLabel(Graph,digraph:out_edges(Graph,J),E),
                  case Comm of
                    null -> cars:turn(Who, {Dir, Road});
                    _-> communication_tower:receive_message(Comm,Who,{turn,{Dir, Road}})
                  end
              end
  end.


