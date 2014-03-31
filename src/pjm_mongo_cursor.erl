-module(pjm_mongo_cursor).
-export([new/2, next/1, rest/1, take/2, foldl/4, map/3, close/1]).

new(ModelModule, MongoCursor) ->
    {?MODULE, {ModelModule, MongoCursor}}.

next({?MODULE, {M, C}}) ->
    case mongo_cursor:next(C) of
        {} -> {};
        {Result} -> pjm_bson:from_bson(Result, M)
    end.

rest({?MODULE, {M, C}}) ->
    lists:map(
      fun(Bson) -> pjm_bson:from_bson(Bson, M) end,
      mongo_cursor:rest(C)
     ).

take(Limit, {?MODULE, {M, C}}) ->
    lists:map(
      fun(Bson) -> pjm_bson:from_bson(Bson, M) end,
      mongo_cursor:take(C, Limit)
     ).

foldl(Fun, Acc, Max, {?MODULE, {M, C}}) ->
    F = fun(Bson, AccIn) ->
                Fun(pjm_bson:from_bson(Bson, M), AccIn)
        end,
    mongo_cursor:foldl(F, Acc, C, Max).

map(Fun, Max, {?MODULE, {M, C}}) ->
    F = fun(Bson) ->
                Fun(pjm_bson:from_bson(Bson, M))
        end,
    mongo_cursor:map(F, C, Max).

close({?MODULE, {_M, C}}) ->
    mongo_cursor:close(C).
