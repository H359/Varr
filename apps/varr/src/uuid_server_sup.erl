-module(uuid_server_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link(?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%spec for storage process
uuid_spec() ->
    {uuid_server,
        {uuid_server, start_link, []},
        transient,
        10,
        worker,
        [uuid_server]}.

init([]) ->
    {ok, { {one_for_one, 5, 10}, [uuid_spec()]} }.
