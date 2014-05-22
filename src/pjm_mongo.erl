-module(pjm_mongo).
-include("./pjm_mongo_config.hrl").

-export([new/0, new/1]).
-export([do/1, do/2, do/4]).

-export([insert/1, update/1, update/2, update/3, update/4, save/1]).
-export([do_insert/2, do_update/2, do_update/3, do_update/4, do_update/5, do_save/2]).
-export([delete/1, do_delete/2]).
-export([find_one/2, find_one/3, find_one/4]).
-export([do_find_one/3, do_find_one/4, do_find_one/5]).
-export([find_one_or_insert/2, find_one_or_insert/3]).
-export([do_find_one_or_insert/3, do_find_one_or_insert/4]).
-export([find/2, find/3, find/4, find/5]).
-export([do_find/3, do_find/4, do_find/5, do_find/6]).
-export([command/1, do_command/2]).
-export([list_collections/0, do_list_collections/1]).
-export([get_collection/1]).

-type mongo() :: {?MODULE, #config{}}.
-export_type([mongo/0]).

-define(WRAP_DO(Name, F), Name(Config) -> do(fun() -> F() end, Config)).
-define(WRAP_DO(Name, F, A1), Name(A1, Config) -> do(fun() -> F(A1) end, Config)).
-define(WRAP_DO(Name, F, A1, A2), Name(A1, A2, Config) -> do(fun() -> F(A1, A2) end, Config)).
-define(WRAP_DO(Name, F, A1, A2, A3), Name(A1, A2, A3, Config) -> do(fun() -> F(A1, A2, A3) end, Config)).
-define(WRAP_DO(Name, F, A1, A2, A3, A4), Name(A1, A2, A3, A4, Config) -> do(fun() -> F(A1, A2, A3, A4) end, Config)).
-define(WRAP_DO(Name, F, A1, A2, A3, A4, A5), Name(A1, A2, A3, A4, A5, Config) -> do(fun() -> F(A1, A2, A3, A4, A5) end, Config)).

-define(setup_do(F), {setup, fun start/0, fun stop/1, fun({_, M}) -> M:do(fun() -> F(M) end) end}).

new() ->
    {?MODULE, pjm_mongo_config:get_config()}.

new(ConfigPid) ->
    {?MODULE, pjm_mongo_config:get_config(ConfigPid)}.

-spec do(fun(() -> A)) -> A.
do(Action) ->
    do(Action, new()).

-spec do(fun(() -> A), {?MODULE, #config{}}) -> A.
do(Action, {?MODULE, #config{ read_mode = R, write_mode = W, database = Db } = Config}) ->
    Conn = connect(Config),
    mongo:do(W, R, Conn, Db, Action).

do(W, R, Action, {?MODULE, #config{ database = Db } = Config}) ->
    Conn = connect(Config),
    mongo:do(W, R, Conn, Db, Action).

% Insert model or models. When inserting models, the first model collection is
% used.
-spec insert(pjm:model()) -> pjm:model();
            ([pjm:model()]) -> [pjm:model()].
insert(Model) when is_tuple(Model) ->
    hd(insert([Model]));
insert([Model|_] = Models) ->
    Coll = get_collection(Model),
    Docs = lists:map(fun pjm_bson:to_bson/1, Models),
    Docs1 = mongo:insert(Coll, Docs),
    lists:zipwith(
      fun(M, Doc) ->
              case bson:lookup('_id', Doc) of
                  {Id} -> M:set('_id', Id);
                  _ -> M
              end
      end,
      Models,
      Docs1
     ).

?WRAP_DO(do_insert, insert, Model).

update(Model) ->
    save(Model).

update(Model, Selector) ->
    update(Model, Selector, false, false).

update(Model, Selector, Upsert) ->
    update(Model, Selector, Upsert, false).

update(Model, Selector, Upsert, MultiUpdate) ->
    Coll = get_collection(Model),
    Doc = pjm_bson:to_bson(Model),
    mongo:update(Coll, Selector, Doc, Upsert, MultiUpdate).

save(Model) ->
    case objectid(Model) of
        {} -> insert(Model);
        {Id} -> update(Model, {'_id', Id})
    end.

?WRAP_DO(do_update, update, Model).
?WRAP_DO(do_update, update, Selector, Model).
?WRAP_DO(do_update, update, Selector, Model, Upsert).
?WRAP_DO(do_update, update, Selector, Model, Upsert, MultiUpdate).
?WRAP_DO(do_save, save, Model).

delete(Model) ->
    case objectid(Model) of
        {} -> ok;
        {Id} ->
            Coll = get_collection(Model),
            mongo:delete(Coll, {'_id', Id})
    end.

?WRAP_DO(do_delete, delete, Model).

find_one(Model, Selector) ->
    find_one(Model, Selector, [], 0).

find_one(Model, Selector, Projector) ->
    find_one(Model, Selector, Projector, 0).

find_one(Model, Selector, Projector, Skip) when is_tuple(Model) ->
    Coll = get_collection(Model),
    Result = mongo:find_one(Coll, Selector, Projector, Skip),
    case Result of
        {} -> {};
        {Doc} -> {pjm_bson:from_bson(Doc, Model)}
    end;
find_one(Module, Selector, Projector, Skip) ->
    find_one(Module:new(), Selector, Projector, Skip).

?WRAP_DO(do_find_one, find_one, Model, Selector).
?WRAP_DO(do_find_one, find_one, Model, Selector, Projector).
?WRAP_DO(do_find_one, find_one, Model, Selector, Projector, Skip).

-spec find_one_or_insert(pjm:model(), bson:document()) -> pjm:model().
find_one_or_insert(Model, Selector) ->
    find_one_or_insert(Model, Selector, []).

-spec find_one_or_insert(pjm:model(), bson:document(), mongo:projector()) -> pjm:model().
find_one_or_insert(Model, Selector, Projector) ->
    case find_one(Model, Selector, Projector, 0) of
        {} -> insert(Model);
        {Result} -> Result
    end.

?WRAP_DO(do_find_one_or_insert, find_one_or_insert, Model, Selector).
?WRAP_DO(do_find_one_or_insert, find_one_or_insert, Model, Selector, Projector).

find(Module, Selector) ->
    find(Module, Selector, [], 0, 0).

find(Module, Selector, Projector) ->
    find(Module, Selector, Projector, 0, 0).

find(Module, Selector, Projector, Skip) ->
    find(Module, Selector, Projector, Skip, 0).

find(Module, Selector, Projector, Skip, BatchSize) ->
    Coll = get_collection(Module),
    Cursor = mongo:find(Coll, Selector, Projector, Skip, BatchSize),
    pjm_mongo_cursor:new(Module, Cursor).

?WRAP_DO(do_find, find, Model, Selector).
?WRAP_DO(do_find, find, Model, Selector, Projector).
?WRAP_DO(do_find, find, Model, Selector, Projector, Skip).
?WRAP_DO(do_find, find, Model, Selector, Projector, Skip, BatchSize).

%% @see mongo:command/1
command(Command) ->
    mongo:command(Command).

?WRAP_DO(do_command, command, Command).

list_collections() ->
    Db = get_database(),
    io:format("Db=~p~n", [Db]),
    DbBinary = atom_to_binary(Db, utf8),
    PrefixSize = size(DbBinary) + 1,
    GetName = fun({name, Name}, List) ->
                      case {binary:match(Name, <<"$">>), binary:match(Name, <<".system.">>)} of
                          {nomatch, nomatch} -> [binary:part(Name, {PrefixSize, size(Name) - PrefixSize})|List];
                          _ -> List
                      end;
                 (_, List) -> List
              end,
    Cursor = mongo:find('system.namespaces', {}),
    lists:foldl(GetName, [], mongo_cursor:rest(Cursor)).

?WRAP_DO(do_list_collections, list_collections).

connect(#config{pool_name = Name}) ->
    mongo_pool:get(Name).

-spec objectid(pjm:model()) -> {binary()}.
objectid(Model) ->
    case Model:get('_id') of
        undefined -> {};
        Id -> {pjm_bson:coerce(objectid, Id)}
    end.

-spec get_collection(pjm:model()) -> atom().
get_collection({pjm, Module, _} = Model) when is_tuple(Model) ->
    case pjm:info(stores_in, Model) of
        undefined -> Module;
        Result -> Result
    end;
get_collection(Module) ->
    case pjm:info(stores_in, Module) of
        undefined -> Module;
        Result -> Result
    end.

%% Get database set in current context.
%%
%% 5 is magic number to get database from the internal record `context' in
%% mongo.erl
get_database() ->
    element(5, erlang:get(mongo_action_context)).
