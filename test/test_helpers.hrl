-include_lib("eunit/include/eunit.hrl").
-define(setup(F), {setup, fun start/0, fun stop/1, F}).

-define(start_pjm_mongo_config(), pjm_mongo_config:start_link({local, ?MODULE}, [{database, pjm_mongo_test}])).
