-module(pjm_mongo).
-include("./pjm_mongo_config.hrl").

-export([new/0, new/1]).
-export([do/2, do/4]).

new() ->
    {?MODULE, pjm_mongo_config:config()}.

new(ConfigPid) ->
    {?MODULE, pjm_mongo_config:config(ConfigPid)}.

-spec do(fun(() -> A), #config{}) -> A.
do(Action, { read_mode = R, write_mode = W, database = Db } = Config) ->
    Conn = connect(Config),
    mongo:do(W, R, Conn, Db, Action).

do(W, R, Action, { database = Db } = Config) ->
    Conn = connect(Config),
    mongo:do(W, R, Conn, Db, Action).

connect(#config{pool_name = Name}) ->
    mongo_pool:get(Name).
