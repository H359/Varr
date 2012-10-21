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
            {[<<"socket.io">>, <<"1">>], handshake_handler, []},
            {[<<"socket.io">>, <<"1">>, <<"websocket">>, '...'], websocket_handler, []},
            %% just server socket.io's static files, eg: socket.io.js, WebSocketMain.swf, WebSocketMainInsecure.swf
            {[<<"socket.io">>, <<"static">>, '...'], cowboy_static, [
                {directory, {priv_dir, ?MODULE, [<<"static">>]}},
                {mimetypes, [
                    {<<".js">>, [<<"application/x-javascript">>]},
                    {<<".swf">>, [<<"application/x-shockwave-flash">>]}
                ]}
            ]},
            {[<<"top">>], toppage_handler, []},
            {['...'], cowboy_static, [
                {directory, {priv_dir, varr, [<<"www">>]}},
                {mimetypes, [
                    {<<".htm">>, [<<"text/html">>]},
                    {<<".html">>, [<<"text/html">>]},
                    {<<".css">>, [<<"text/css">>]},
                    {<<".js">>, [<<"application/x-javascript">>]},
                    {<<".jpeg">>, [<<"image/jpeg">>]},
                    {<<".jpg">>, [<<"image/jpeg">>]},
                    {<<".ico">>, [<<"image/x-icon">>]},
                    {<<".gif">>, [<<"image/gif">>]},
                    {<<".png">>, [<<"image/png">>]},
                    {<<".swf">>, [<<"application/x-shockwave-flash">>]}
                ]}
            ]}
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
