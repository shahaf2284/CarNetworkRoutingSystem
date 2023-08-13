%%%-------------------------------------------------------------------
%%% @author Maayan Belzer, Nir Tapiero
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. יוני 2020 17:58
%%%-------------------------------------------------------------------
-module(traffic_light).
-author("Maayan Belzer, Nir Tapiero").

-behaviour(gen_statem).

%% API
-export([start_link/0,start/2]).

%% gen_statem callbacks
-export([init/1, format_status/2, state_name/3, handle_event/4, terminate/3,
  code_change/4, callback_mode/0]).

%Events
-export([timeout/0,sensor_msg/2]).

%States
-export([red/3,green/3,yellow/3]).

-define(SERVER, ?MODULE).

-record(traffic_light_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
start_link() ->
  gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).
start(Name,{{R,J},[{X,Y}]}) ->
  gen_statem:start({local,Name}, ?MODULE, {{R,J},[{X,Y}]}, []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
%init([]) ->




init({{R,J},[{X,Y}]}) ->
  ets:insert(junction,{{R,J},[{X,Y},self()]}), % insert junction to ets and go to red
  {ok, red, #traffic_light_state{},2000}.


%% @private
%% @doc This function is called by a gen_statem when it needs to find out
%% the callback mode of the callback module.
callback_mode() ->
  state_functions.

%%Events
timeout() -> gen_statem:cast(?MODULE,{time}). % timeout event
sensor_msg(Pid,Color) -> gen_statem:cast(Pid,{msg,Color}). % receive message


%% @private
%% @doc Called (1) whenever sys:get_status/1,2 is called by gen_statem or
%% (2) when gen_statem terminates abnormally.
%% This callback is optional.
format_status(_Opt, [_PDict, _StateName, _State]) ->
  Status = some_term,
  Status.

%% @private
%% @doc There should be one instance of this function for each possible
%% state name.  If callback_mode is state_functions, one of these
%% functions is called when gen_statem receives and event from
%% call/2, cast/2, or as a normal process message.
state_name(_EventType, _EventContent, State = #traffic_light_state{}) ->
  NextStateName = next_state,
  {next_state, NextStateName, State}.


red(timeout,4000,State = #traffic_light_state{}) -> % after 4 seconds, turn green
  NextStateName = green,
  {next_state, NextStateName, State,3000};

red(timeout,2000,State = #traffic_light_state{}) -> % after 2 seconds turn green
  NextStateName = green,
  {next_state, NextStateName, State,3000};

red(cast,{msg,_},State = #traffic_light_state{}) -> % stay red for 3 seconds if message was received
  NextStateName = red,
  {next_state, NextStateName, State,4000}.

yellow(timeout,1000,State = #traffic_light_state{}) -> % after 1 second, turn red for 2 seconds
  NextStateName = red,
  {next_state, NextStateName, State,2000};

yellow(cast,{msg,_},State = #traffic_light_state{}) -> % stay yellow for 1 second if message was received

  NextStateName = red,
  {next_state, NextStateName, State,4000}.

green(timeout,3000,State = #traffic_light_state{}) -> % after 3 seconds turn yellow for 1 second
  NextStateName = yellow,
  {next_state, NextStateName, State,1000};

green(cast,{msg,Color},State = #traffic_light_state{}) -> % if message received is red go to red for 4 seconds, if it's green stay green for 3 seconds
  case Color of
    red ->
      NextStateName = red,
      {next_state, NextStateName, State,4000};

    _ ->   NextStateName = green,
      {next_state, NextStateName, State,3000}
  end.

%% @private
%% @doc If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2, cast/2, or as a normal
%% process message, this function is called.
handle_event(_EventType, _EventContent, _StateName, State = #traffic_light_state{}) ->
  NextStateName = the_next_state_name,
  {next_state, NextStateName, State}.

%% @private
%% @doc This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
terminate(_Reason, _StateName, _State = #traffic_light_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, StateName, State = #traffic_light_state{}, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

