%%%-------------------------------------------------------------------
%%% 
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Aug 2023 
%%%-------------------------------------------------------------------
-module(communication_tower).


-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3,start/2,receive_message/3]).

-define(SERVER, ?MODULE).

-record(communication_tower_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
start(Name,{X,Y})->
  gen_server:start_link({local, Name}, ?MODULE, [{X,Y}], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #communication_tower_state{}} | {ok, State :: #communication_tower_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).

init([]) ->
  {ok, #communication_tower_state{}};

init([{X,Y}]) -> % initialize comm and insert to ets
  ets:insert(comms,{{X,Y},[self()]}),
  {ok, #communication_tower_state{}}.

%% Events
receive_message(Who,Car,MSG) -> % message was received
  gen_server:cast(Who,{Who,Car,MSG}).





%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #communication_tower_state{}) ->
  {reply, Reply :: term(), NewState :: #communication_tower_state{}} |
  {reply, Reply :: term(), NewState :: #communication_tower_state{}, timeout() | hibernate} |
  {noreply, NewState :: #communication_tower_state{}} |
  {noreply, NewState :: #communication_tower_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #communication_tower_state{}} |
  {stop, Reason :: term(), NewState :: #communication_tower_state{}}).
handle_call(_Request, _From, State = #communication_tower_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #communication_tower_state{}) ->
  {noreply, NewState :: #communication_tower_state{}} |
  {noreply, NewState :: #communication_tower_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #communication_tower_state{}}).


handle_cast({Comm,Car,MSG},State)-> % checks what the message was and forward it to car or to server
  case MSG of
    {s_light,M} -> server:s_light(Comm,Car,M);
    {s_close_to_car,M} -> server:s_close_to_car(Comm,Car,M);
    {deleteCar} -> server:deleteCar(Car);
    {turn,M} -> cars:turn(Car,M);
    {bypass} -> cars:bypass(Car);
    {stop,M} -> cars:stop(Car,M);
    Error -> io:format("error in comms: ~p~n",[Error])

  end,
  {noreply, State};

handle_cast(Else,State) -> % starts car in local PC
  io:format("error in comms: ~p~n",[Else]),
  {noreply, State}.


%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #communication_tower_state{}) ->
  {noreply, NewState :: #communication_tower_state{}} |
  {noreply, NewState :: #communication_tower_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #communication_tower_state{}}).
handle_info(_Info, State = #communication_tower_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #communication_tower_state{}) -> term()).
terminate(_Reason, _State = #communication_tower_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #communication_tower_state{},
    Extra :: term()) ->
  {ok, NewState :: #communication_tower_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #communication_tower_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

