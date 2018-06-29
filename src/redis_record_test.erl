-module(redis_record_test).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-record (user, {id = 0,
                name = "",
                height = 0.0,
                private = <<>>,
                aa = ""
                }).

-export([test/0]).

test() ->
    redis_record:start(),
    redis_record:add_default_record(#user{}, [{id, #user.id}], [#user.aa]),
    redis_record:add_cache(#user{id = 2, name = "test2", private = <<2:32>>, height = 99.99}),
    CacheRecord = redis_record:get_cache(#user{id = 2}),
    io:format("CacheRecord: ~p~n", [CacheRecord]).

-ifdef(EUNIT).
start_test() ->
    % ?assert(redis_record:start() == ok).
    ?assertEqual({user, "2,integer;3,list;4,float;5,binary;"}, redis_record_handler:parse_field(#user{})).
-endif.