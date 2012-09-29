-module(var_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Dispatch = [
        {'_', [
            {[], toppage_handler, []}
        ]}
    ],
    Port = var:get_config_value(http_port, 8080),
    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
        {dispatch, Dispatch}
    ]),
    storage_sup:start_link(),
    var_sup:start_link().

stop(_State) ->
    ok.
