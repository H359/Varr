-module(varr_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    SockJSState = sockjs_handler:init_state(<<"/store">>, fun toppage_handler:process_ws/3, state, []),
    Dispatch = [
        {'_', [
            {[<<"store">>, '...'], sockjs_cowboy_handler, SockJSState},
            {'_', toppage_handler, []}
        ]}
    ],
    Port = varr:get_env(http_port, 8080),
    {ok, _} = cowboy:start_http(http, 100, [{port, Port}], [
        {dispatch, Dispatch}
    ]),
    % ParserPoolSize = varr:get_env(parser_pool_size, 10),
    % hottub:start_link(parser, ParserPoolSize, parser, start_link, []),
    TcpPoolSize = varr:get_env(tcp_storage_pool_size, 10),
    hottub:start_link(tcp_storage, TcpPoolSize, tcp_storage, start_link, []),
    % storage_sup:start_link(),
    uuid_server_sup:start(),
    % session_server:start(),
    varr_sup:start_link().

stop(_State) ->
    ok.
