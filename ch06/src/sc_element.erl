% For every key there is a separate process (a simple cache element = sc_element) which stores the value!
% Implemented as gen_server. Is used as a dynamic child for the supervisor.

-module(sc_element).

-behavior(gen_server).

-export([start_link/2, create/2, create/1, fetch/1, replace/2, delete/1]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(DEFAULT_LEASE_TIME, 60).

-record(state, {value, lease_time, start_time}).

%%%%%%%
% API %
%%%%%%%

start_link(Value, LeaseTime) ->
    gen_server:start_link(?MODULE, [Value, LeaseTime], []).

create(Value, LeaseTime) ->
    sc_sup:start_child(Value, LeaseTime).

create(Value) ->
    create(Value, ?DEFAULT_LEASE_TIME).

fetch(Pid) ->
    gen_server:call(Pid, fetch).

replace(Pid, Value) ->
    gen_server:cast(Pid, {replace, Value}).

delete(Pid) ->
    gen_server:cast(Pid, delete).

%%%%%%%%%%%%%
% Callbacks %
%%%%%%%%%%%%%

% Actually not a good idea to use time-outs.
% Will bog down the server when you have many processes.

init([Value, LeaseTime]) ->
    Now = calendar:local_time(),
    StartTime = calendar:datetime_to_gregorian_seconds(Now),
    {ok, #state{value = Value, lease_time = LeaseTime, start_time = StartTime},
        time_left(StartTime, LeaseTime)}.

handle_call(fetch, _From, State) ->
    #state{value = Value, lease_time = LeaseTime, start_time = StartTime} = State,
    TimeLeft = time_left(StartTime, LeaseTime),
    {reply, {ok, Value}, State, TimeLeft}.

handle_cast({replace, Value}, State) ->
    #state{lease_time = LeaseTime, start_time = StartTime} = State,
    TimeLeft = time_left(StartTime, LeaseTime),
    {noreply, State#state{value = Value}, TimeLeft};
handle_cast(delete, State) ->
    {stop, normal, State}.

handle_info(timeout, State) ->
    {stop, normal, State}.

terminate(_Reason, _State) ->
    sc_store:delete(self()),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%
% Internal Functions %
%%%%%%%%%%%%%%%%%%%%%%

% Setting server timeouts:
% If you forget to return a new timeout value in one of the call-back functions,
% the timeout will revert to infinity. When you're using server timeouts,
% it's important to remember to set them in EVERY clause of EVERY call-back function.

time_left(_StartTime, infinity) ->
    infinity;
time_left(StartTime, LeaseTime) ->
    Now = calendar:local_time(),
    CurrentTime = calendar:datetime_to_gregorian_seconds(Now),
    TimeElapsed = CurrentTime - StartTime,
    case LeaseTime - TimeElapsed of
        Time when Time =< 0 ->
            0;
        Time ->
            Time * 1000
    end.
