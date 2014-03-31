-module(pjm_mongo_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_link(Config) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Config).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(Args) ->
    Children = [?CHILD(pjm_mongo_config, worker, Args)],
    {ok, { {one_for_one, 5, 10}, Children} }.
