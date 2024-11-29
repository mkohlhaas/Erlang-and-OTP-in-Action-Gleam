-module(sc_event).

-export([start_link/0, add_handler/2, delete_handler/2]).
-export([lookup/1, create/2, replace/2, delete/1, timeout/1]).

-define(SERVER, ?MODULE).

%%%%%%%%%%%%%%%%%%%
% gen_event Calls %
%%%%%%%%%%%%%%%%%%%

% Starts a new gen_event container - the event manager comparable to the error_logger/logger container - under the module name.
% Normally we don't need a start_link function but would invoke the event manager from a supervisor, e.g.:
%
% {sc_event,
% {gen_event, start_link, [{local, sc_event}]}, permanent, 1000, worker, [gen_event]}

start_link() ->
    gen_event:start_link({local, ?SERVER}).

add_handler(Handler, Args) ->
    gen_event:add_handler(?SERVER, Handler, Args).

delete_handler(Handler, Args) ->
    gen_event:delete_handler(?SERVER, Handler, Args).

%%%%%%%
% API %
%%%%%%%

% used in src/simple_cache.erl

% create event
create(Key, Value) ->
    gen_event:notify(?SERVER, {create, {Key, Value}}).

% lookup event
lookup(Key) ->
    gen_event:notify(?SERVER, {lookup, Key}).

% replace event
replace(Key, Value) ->
    gen_event:notify(?SERVER, {replace, {Key, Value}}).

% delete event
delete(Key) ->
    gen_event:notify(?SERVER, {delete, Key}).

% timeout event
timeout(Key) ->
    gen_event:notify(?SERVER, {timeout, Key}).
