-module(pjm_mongo_config).

-behaviour(gen_server).
-include("./pjm_mongo_config.hrl").

%% API
-export([start_link/0, start_link/1, start_link/2]).
-export([stop/0, stop/1]).
-export([sync_stop/0, sync_stop/1]).
-export([config/0, config/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {config = #config{}}).

%%% ==================================================================
%%% API
%%% ==================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

-spec start_link([proplists:property()]) -> {ok, pid()}.
start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

-spec start_link({local, atom()} | {global, term()} | {via, module(), term()}, [proplists:property()]) -> {ok, pid()}.
start_link(ServerName, Args) ->
    gen_server:start_link(ServerName, ?MODULE, Args, []).

-spec stop() -> ok.
stop() -> stop(?MODULE).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:cast(Pid, stop).

-spec sync_stop() -> ok.
sync_stop() ->
    sync_stop(?MODULE).

-spec sync_stop(pid()) -> ok.
sync_stop(Pid) ->
    gen_server:call(Pid, stop).

-spec config() -> #config{}.
config() -> get(?MODULE).

-spec config(pid()) -> #config{}.
config(Pid) ->
    gen_server:call(Pid, get).

%%% ==================================================================
%%% gen_server callbacks
%%% ==================================================================

init(Args) ->
    DefaultConfig = set_config(application:get_all_env(), #config{}),
    Config = set_config(Args, DefaultConfig),
    {ok, _} = start_pool(Config),
    {ok, #state{config = Config}}.

handle_call(get, _From, State) ->
    {reply, State#state.config, State};
handle_call(stop, _From, State) ->
    stop_pool(State#state.config),
    {stop, normal, ok, State}.

handle_cast({set, Args}, #state{config = Config} = State) ->
    NewConfig = set_config(Args, Config),
    case (NewConfig#config.host =/= Config#config.host)
        orelse (NewConfig#config.port =/= Config#config.port)
        orelse (NewConfig#config.pool_name =/= Config#config.pool_name)
        orelse (NewConfig#config.pool_size =/= Config#config.pool_size) of
        true ->
            stop_pool(Config),
            start_pool(NewConfig)
    end,
    {noreply, State#state{config = NewConfig}};
handle_cast(stop, State) ->
    stop_pool(State#state.config),
    {stop, normal, State}.

handle_info(_Request, State) ->
    {noreply, State}.

terminate(_Reson, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% ==================================================================
%%% Internal functions
%%% ==================================================================

set_config({host, V}, Config) ->
    Config#config{ host = V };
set_config({port, V}, Config) ->
    Config#config{ port = V };
set_config({pool_name, V}, Config) ->
    Config#config{ pool_name = V };
set_config({pool_size, V}, Config) ->
    Config#config{ pool_size = V };
set_config({database, V}, Config) ->
    Config#config{ database = V };
set_config({read_mode, V}, Config) ->
    Config#config{ read_mode = V };
set_config({write_mode, V}, Config) ->
    Config#config{ write_mode = V };
set_config(_, Config) ->
    Config.

start_pool(#config{pool_name = Name, pool_size = Size, host = Host, port = Port}) ->
    mongo_sup:start_pool(Name, Size, {Host, Port}).

stop_pool(#config{pool_name = Name}) ->
    mongo_sup:stop_pool(Name).
