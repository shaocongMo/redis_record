-module(redis_record).

%% ====================================================================
%% Include files  
%% ====================================================================


%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0, start/1, start/3, start/4, start/2, stop/0]).
-export([add_default_record/3,
         add_cache/1,
         get_cache/1]).

start() ->    
    case eredis:start_link() of
        {ok, RedisClient} ->
            start(RedisClient, []);
        _ ->
            fail_to_connect_redis
    end.

start(Args) ->
    case eredis:start_link() of
        {ok, RedisClient} ->
            start(RedisClient, Args);
        _ ->
            fail_to_connect_redis
    end.

start(RedisHost, RedisPort, RedisDatabase) ->
    case eredis:start_link(RedisHost, RedisPort, RedisDatabase) of
        {ok, RedisClient} ->
            start(RedisClient, []);
        _ ->
            fail_to_connect_redis
    end.

start(RedisHost, RedisPort, RedisDatabase, Args) ->
    case eredis:start_link(RedisHost, RedisPort, RedisDatabase) of
        {ok, RedisClient} ->
            start(RedisClient, Args);
        _ ->
            fail_to_connect_redis
    end.

start(RedisClient, Args) ->
    application:start(redis_record),
    redis_record_client:start([{redis_client, RedisClient} | Args]),
    ok.

stop() ->
    redis_record_client:stop().

add_default_record(DefaultRecord, PrimaryFields, IgnoreFields) ->
    Fields = redis_record_handler:parse_field(DefaultRecord),
    redis_record_client:add_default_record(Fields, PrimaryFields, IgnoreFields).

add_cache(Cache) ->
    redis_record_client:add_cache(Cache).

get_cache(CacheKey) ->
    redis_record_client:get_cache(CacheKey).
%% ====================================================================
%% Internal functions
%% ====================================================================


