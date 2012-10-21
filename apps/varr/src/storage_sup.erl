-module(storage_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%spec for storage process
storage_spec() ->
    {storage_server,
        {storage, start_link, []},
        transient,
        10,
        worker,
        [storage]}.

init([]) ->
    {ok, { {one_for_one, 5, 10}, [storage_spec()]} }.
