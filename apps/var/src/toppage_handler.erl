-module(toppage_handler).

%ordinary HTTP handling
-export([init/3, handle/2, terminate/2]).

%websockets handling
-export([websocket_init/3, websocket_handle/3, websocket_info/3, websocket_terminate/3]).

% ordinary HTTP section

init(_Transport, Req, []) ->
    % We check for a specific websocket header here
    % RFC 6455
    % Upgrade: websocket
    case cowboy_req:header(<<"Upgrade">>, Req) of
        {<<"websocket">>, _Req2} -> {upgrade, protocol, cowboy_websocket};
        _ -> {ok, Req, undefined}
    end.

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req), %access the method
    {ok, Data, Req3} = cowboy_req:body(Req2), %access the body of the request
    {ok, Req4} = store(Method, Data, Req3), %now we store stuff
    {ok, Req4, State}.

terminate(_Req, _State) ->
    ok.
    
% websocket handling

websocket_init(_Transport, Req, _Opts) ->
    {ok, Req, undefined_state}.

websocket_handle({text, Msg}, Req, State) ->
    store(<<"WEBSOCKET">>, Msg, Req),
    {reply, {text, <<"OK">>}, Req, State}; %don't like the magic "WEBSOCKET" here; TODO: find a suitable approach

websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

% inner API

store(<<"POST">>, Data, Req) -> 
    % _Json = parser:process_json(Data),
    _Json = hottub:call(parser, {process_json, Data}),
    cowboy_req:reply(200,
        [{<<"Content-Encoding">>, <<"utf-8">>}], <<"OK">>, Req); %TODO: form and send reply

store(<<"WEBSOCKET">>, Data, _Req) ->
    _Json = hottub:call(parser, {process_json, Data});

store(_, _, Req) ->
    %% Method not allowed.
    cowboy_req:reply(405, Req).
