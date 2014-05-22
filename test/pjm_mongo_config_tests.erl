-module(pjm_mongo_config_tests).
-include("./test_helpers.hrl").
-include("../src/pjm_mongo_config.hrl").

start() ->
    application:start(bson),
    application:start(mongodb),
    catch pjm_mongo_config:sync_stop(?MODULE),
    {ok, Pid} = ?start_pjm_mongo_config(),
    Pid.

stop(Pid) ->
    pjm_mongo_config:sync_stop(Pid),
    application:stop(mongodb),
    application:start(bson).

get_config_test_() ->
    F = fun(Pid) ->
                Config = pjm_mongo_config:get_config(Pid),
                ?_assertEqual(#config{host = "localhost",
                                      port = 27017,
                                      pool_name = pjm_mongo_pool,
                                      pool_size = 5,
                                      database = pjm_mongo_test,
                                      read_mode = master,
                                      write_mode = safe},
                              Config)
        end,
    ?setup(F).
