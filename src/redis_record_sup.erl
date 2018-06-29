%%%-------------------------------------------------------------------
%% @doc redis_record top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(redis_record_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Mod, Args) ->
    supervisor:start_child(?SERVER, child(Mod, Args)).
%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {one_for_all, 0, 1}, []} }.

%%====================================================================
%% Internal functions
%%====================================================================

child(Mod, Args) ->
    {Mod, {Mod, start_link, [Args]}, permanent, brutal_kill, worker, [Mod]}.