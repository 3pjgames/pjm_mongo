-module(pjm_mongo_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, Args) ->
    pjm_mongo_sup:start_link(Args).

stop(_State) ->
    ok.
