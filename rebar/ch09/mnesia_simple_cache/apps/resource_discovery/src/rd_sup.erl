-module(rd_sup).

-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

%%%%%%%
% API %
%%%%%%%

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%%%%%%%%%%%
% Callbacks %
%%%%%%%%%%%%%

init([]) ->
    Server = {rd_server, {rd_server, start_link, []}, permanent, 2000, worker, [rd_server]},
    Children = [Server],
    RestartStrategy = {one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.
