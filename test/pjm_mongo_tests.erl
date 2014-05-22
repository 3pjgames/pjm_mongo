-module(pjm_mongo_tests).
-include("./test_helpers.hrl").

-compile([{parse_transform, pjm_parse_trans}]).

-pjm({fields, [
               {login, binary},
               {password, binary},
               {age, integer}
              ]}).

start() ->
    application:start(bson),
    application:start(mongodb),
    catch pjm_mongo_config:sync_stop(?MODULE),
    {ok, Pid} = ?start_pjm_mongo_config(),
    Mongo = pjm_mongo:new(Pid),
    Mongo:do(fun() ->
                     [
                      try mongo:command({drop, C})
                      catch
                          error:{bad_command, _} -> ok
                      end || C <- pjm_mongo:list_collections() ]
             end),
    {Pid, Mongo}.

stop({ConfigPid, _}) ->
    pjm_mongo_config:sync_stop(ConfigPid),
    application:stop(mongodb),
    application:start(bson).

list_collections_test_() ->
    F = fun({_, M}) ->
                M:do(fun() ->
                             mongo:insert(foo, {bar, 1}),
                             mongo:insert(bar, {foo, 1})
                     end),
                ?_assertEqual([<<"bar">>, <<"foo">>], M:do_list_collections())
        end,
    ?setup(F).

insert_test_() ->
    ?setup(
       fun({_, M}) ->
               Doc = new([{login, <<"ian">>}, {password, <<"secret">>}]),
               M:do_insert(Doc),
               {Doc1} = M:do_find_one(?MODULE, {login, <<"ian">>}),
               [
                ?_assertEqual(<<"ian">>, Doc1:get(login)),
                ?_assertEqual(<<"secret">>, Doc1:get(password))
               ]
       end
      ).

update_test_() ->
    ?setup(
       fun({_, M}) ->
               Doc = new([{login, <<"ian">>}, {password, <<"secret">>}]),
               M:do_insert(Doc),
               Update = Doc:set([{password, <<"x">>}]),
               M:do_update(Update, {login, <<"ian">>}),
               {Doc1} = M:do_find_one(?MODULE, {login, <<"ian">>}),
               [
                ?_assertEqual(<<"x">>, Doc1:get(password))
               ]
       end
      ).

find_test_() ->
    ?setup(
       fun({_, M}) ->
               M:do(fun() ->
                            pjm_mongo:insert(new([{login, <<"ian">>}, {password, <<"secret">>}])),
                            pjm_mongo:insert(new([{login, <<"ian">>}, {password, <<"x">>}]))
                    end),
               Cursor = M:do_find(?MODULE, {login, <<"ian">>}),
               Docs = Cursor:rest(),
               Passwords = lists:sort(lists:map(fun(Doc) -> Doc:get(password) end, Docs)),
               [
                ?_assertEqual([<<"secret">>, <<"x">>], Passwords)
               ]
       end
      ).
