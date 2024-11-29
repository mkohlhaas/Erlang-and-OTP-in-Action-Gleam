-module(sc_event_logger).

-behavior(gen_event).

-export([add_handler/0, delete_handler/0]).
-export([
    init/1,
    handle_event/2,
    handle_call/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

-record(state, {}).

%%%%%%%
% API %
%%%%%%%

% just convenient wrappers around sc_event calls

add_handler() ->
    sc_event:add_handler(?MODULE, []).

delete_handler() ->
    sc_event:delete_handler(?MODULE, []).

%%%%%%%%%%%%%
% Callbacks %
%%%%%%%%%%%%%

init([]) ->
    {ok, #state{}}.

% We are just logging events. We could also use OpenTelemetry!

handle_event({create, {Key, Value}}, State) ->
    error_logger:info_msg("create(Key:~w, Value:~w)~n", [Key, Value]),
    {ok, State};
handle_event({lookup, Key}, State) ->
    error_logger:info_msg("lookup(Key:~w)~n", [Key]),
    {ok, State};
handle_event({delete, Key}, State) ->
    error_logger:info_msg("delete(Key: ~w)~n", [Key]),
    {ok, State};
handle_event({replace, {Key, Value}}, State) ->
    error_logger:info_msg("replace(Key:~w, Value:~w)~n", [Key, Value]),
    {ok, State};
handle_event({timeout, Value}, State) ->
    error_logger:info_msg("timeout(Value: ~w)~n", [Value]),
    {ok, State}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
