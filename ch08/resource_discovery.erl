-module(resource_discovery).

-behavior(gen_server).

-export([start_link/0, add_target_resource_type/1, add_local_resource/2,
         fetch_resources/1, trade_resources/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

% target_resource_types ∷ List Resource_Type, e.g. [simple_cache, logger],            aka "I want"-list.
% local_resource_dict ∷ Dict (Resource_Type ⇒ [Resource]), e.g. simple_cache ⇒ [Pid], aka "I have"-list.
% found_resource_dict ∷ Dict (Resource_Type ⇒ [Resource]), e.g. simple_cache ⇒ [Pid], all resources of the cluster matching "I want"-list. Answers: Where is all the stuff I want?
-record(state, {target_resource_types, local_resource_dict, found_resource_dict}).

%%%%%%%
% API %
%%%%%%%

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

% add resource type to "I want"-list
add_target_resource_type(Type) ->
  gen_server:cast(?SERVER, {add_target_resource_type, Type}).

% add resource to the local "I have"-list
add_local_resource(Type, Resource) ->
  gen_server:cast(?SERVER, {add_local_resource, {Type, Resource}}).

% get a list of all available resources in the cluster for a specific resource type
fetch_resources(Type) ->
  gen_server:call(?SERVER, {fetch_resources, Type}).

% Trade resources means exchanging resource information among all nodes in the cluster.
% Send a message to all nodes asking for their resources and publish own resources to all other nodes.
trade_resources() ->
  gen_server:cast(?SERVER, trade_resources).

%%%%%%%%%%%%%
% Callbacks %
%%%%%%%%%%%%%

init([]) ->
  {ok,
   #state{target_resource_types = [],
          local_resource_dict = dict:new(),
          found_resource_dict = dict:new()}}.

handle_call({fetch_resources, Type}, _From, State) ->
  {reply, dict:find(Type, State#state.found_resource_dict), State}.

handle_cast({add_target_resource_type, Type}, State) ->
  TargetTypes = State#state.target_resource_types,
  NewTargetTypes = [Type | lists:delete(Type, TargetTypes)],
  {noreply, State#state{target_resource_types = NewTargetTypes}};
handle_cast({add_local_resource, {Type, Resource}}, State) ->
  ResourceDict = State#state.local_resource_dict,
  NewResourceDict = add_resource(Type, Resource, ResourceDict),
  {noreply, State#state{local_resource_dict = NewResourceDict}};
handle_cast(trade_resources, State) ->
  ResourceDict = State#state.local_resource_dict,
  AllNodes = [node() | nodes()],
  % send available local resources to all nodes
  lists:foreach(fun(Node) ->
                   gen_server:cast({?SERVER, Node}, {trade_resources, {node(), ResourceDict}})
                end,
                AllNodes),
  {noreply, State};
% reply from all nodes with their available resources
handle_cast({trade_resources, {ReplyTo, Remotes}},
            #state{target_resource_types = TargetTypes,
                   local_resource_dict = Locals,
                   found_resource_dict = OldFound} =
              State) ->
  % Add resources from calling node, but only the ones in our "I want"-list.
  FilteredRemotes = resources_for_types(TargetTypes, Remotes),
  NewFound = add_resources(FilteredRemotes, OldFound),
  case ReplyTo of
    noreply ->
      ok;
    _ ->
      % Send the remote node who called us our local resource dictionary.
      gen_server:cast({?SERVER, ReplyTo}, {trade_resources, {noreply, Locals}})
  end,
  {noreply, State#state{found_resource_dict = NewFound}}.

handle_info(ok = _Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%
% Internal Functions %
%%%%%%%%%%%%%%%%%%%%%%

% Local PIDs are translated/displayed automagically on remote nodes!!! See README for an example.
% See the PIDs in the dictionary on the different nodes.

% Returns updated dictionary.
% Could be easier done with a fold.
add_resources([{Type, Resource} | T], Dict) ->
  add_resources(T, add_resource(Type, Resource, Dict));
add_resources([], Dict) ->
  Dict.

% Returns updated dictionary.
% Add resource to dictionary.
add_resource(Type, Resource, Dict) ->
  case dict:find(Type, Dict) of
    {ok, ResourceList} ->
      NewList = [Resource | lists:delete(Resource, ResourceList)],
      dict:store(Type, NewList, Dict);
    error ->
      dict:store(Type, [Resource], Dict)
  end.

% Returns [{Type, Resource}] (a list of all resource types and their resources we are interested in).
% ResourceDict = available resources on the remote node.
resources_for_types(TypesWeWant, ResourceDict) ->
  Fun =
    fun(TypeWeWant, Acc) ->
       case dict:find(TypeWeWant, ResourceDict) of
         {ok, List} -> [{TypeWeWant, Resource} || Resource <- List] ++ Acc;
         error -> Acc
       end
    end,
  lists:foldl(Fun, [], TypesWeWant).
