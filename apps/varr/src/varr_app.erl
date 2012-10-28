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
    Port = varr:get_env(http_port, 8080),
    {ok, _} = cowboy:start_http(http, 100, [{port, Port}], [
        {dispatch, Dispatch}
    ]),
    ParserPoolSize = varr:get_env(parser_pool_size, 10),
    hottub:start_link(parser, ParserPoolSize, parser, start_link, []),
    storage_sup:start_link(),
    parser_sup:start_link(),
    uuid_server:start(),
    session_server:start(),
    varr_sup:start_link().

stop(_State) ->
    ok.
