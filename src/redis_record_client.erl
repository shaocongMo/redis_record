-module(redis_record_client).

-behaviour(gen_server).

%% ====================================================================
%% Include files  
%% ====================================================================
-define(DEFAULT_RECORDS, default_records).

%% ====================================================================
%% API functions
%% ====================================================================
%% gen_server callbacks 
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start/1, start_link/1, stop/0]).

-export([add_default_record/3,
         add_cache/1,
         get_cache/1]).

-record(state, {redis_client = undefined}).

start(Args) ->
    redis_record_sup:start_child(?MODULE, Args).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

stop() ->
    gen_server:call(?MODULE, stop).

get_redis_client() ->
    gen_server:call(?MODULE, {get_redis_client}).

add_default_record({RecordName, Fields}, PrimaryFields, IgnoreFields) ->
    DefaultRecord = {RecordName, Fields, PrimaryFields, IgnoreFields},
    case ets:lookup(?DEFAULT_RECORDS, RecordName) of
        [_OldDefaultRecord] ->
            ets:delete(?DEFAULT_RECORDS, RecordName),
            ets:insert(?DEFAULT_RECORDS, DefaultRecord);
        [] ->
            ets:insert(?DEFAULT_RECORDS, DefaultRecord);
        _ ->
            skip
    end.

add_cache(Cache) ->
    [RecordName | _] = erlang:tuple_to_list(Cache),
    case ets:lookup(?DEFAULT_RECORDS, RecordName) of
        [{RecordName, _Fields, PrimaryFields, IgnoreFields}] ->
            Key = redis_record_handler:parse_key(Cache, PrimaryFields),
            Values = redis_record_handler:parse_value(Cache, IgnoreFields),
            RedisClient = get_redis_client(),
            eredis:q_async(RedisClient, ["HMSET", Key | Values]);
        _ ->
            add_cache_error_without_default_record
    end.

get_cache(CacheKey) ->
    [RecordName | _ValueList] = tuple_to_list(CacheKey),
    case ets:lookup(?DEFAULT_RECORDS, RecordName) of
        [{RecordName, Fields, PrimaryFields, _IgnoreFields}] ->
            Key = redis_record_handler:parse_key(CacheKey, PrimaryFields),
            RedisClient = get_redis_client(),
            case eredis:q(RedisClient, ["HGETALL", Key]) of
                {ok, ReturnValue} ->
                    redis_record_handler:construct_record(ReturnValue, Fields, CacheKey);
                _ ->
                    without_cache
            end;
        _ ->
            get_cache_error_without_default_record
    end.

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
    Result :: {ok, State}
            | {ok, State, Timeout}
            | {ok, State, hibernate}
            | {stop, Reason :: term()}
            | ignore,
    State :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init(Args) ->
    case proplists:get_value(redis_client, Args, []) of
        [] ->
            {stop, normal};
        RedisClient ->
            ets:new(?DEFAULT_RECORDS, [named_table, public, set]),
            {ok, #state{redis_client = RedisClient}}
    end.

%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
    Result :: {reply, Reply, NewState}
            | {reply, Reply, NewState, Timeout}
            | {reply, Reply, NewState, hibernate}
            | {noreply, NewState}
            | {noreply, NewState, Timeout}
            | {noreply, NewState, hibernate}
            | {stop, Reason, Reply, NewState}
            | {stop, Reason, NewState},
    Reply :: term(),
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity,
    Reason :: term().
%% ====================================================================
handle_call({get_redis_client}, _From, State) ->
    {reply, State#state.redis_client, State};

handle_call(stop, _From, State) ->
    {stop, normale, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
    Result :: {noreply, NewState}
            | {noreply, NewState, Timeout}
            | {noreply, NewState, hibernate}
            | {stop, Reason :: term(), NewState},
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================

handle_cast(_Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
    Result :: {noreply, NewState}
            | {noreply, NewState, Timeout}
            | {noreply, NewState, hibernate}
            | {stop, Reason :: term(), NewState},
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info(_Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
    Reason :: normal
            | shutdown
            | {shutdown, term()}
            | term().
%% ====================================================================
terminate(_Reason, _State) ->
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
    Result :: {ok, NewState :: term()} | {error, Reason :: term()},
    OldVsn :: Vsn | {down, Vsn},
    Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================
