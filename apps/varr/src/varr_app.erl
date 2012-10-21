-module(varr_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Dispatch = [
        {'_', [
            {'_', toppage_handler, []},
            {[<<"socket.io">>, <<"1">>], handshake_handler, []},
            {[<<"socket.io">>, <<"1">>, <<"websocket">>, '...'], websocket_handler, []}
        ]}
    ],
    Port = varr:get_config_value(http_port, 8080),
    {ok, _} = cowboy:start_http(http, 100, [{port, Port}], [
        {dispatch, Dispatch}
    ]),
    ParserPoolSize = varr:get_config_value(parser_pool_size, 10),
    hottub:start_link(parser, ParserPoolSize, parser, start_link, []),
    storage_sup:start_link(),
    parser_sup:start_link(),
    uuid_server:start(),
    endpoint_server:start(),
    session_server:start(),
    varr_sup:start_link().

stop(_State) ->
    ok.
