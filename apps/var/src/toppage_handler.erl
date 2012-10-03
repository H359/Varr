-module(toppage_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/2]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req), %access the method
    {ok, Data, Req3} = cowboy_req:body(Req2), %access the body of the request
    {ok, Req4} = store(Method, Data, Req3), %now we store stuff
    {ok, Req4, State}.

store(<<"POST">>, Data, Req) -> 
    % _Json = parser:process_json(Data),
    _Json = hottub:call(parser, {process_json, Data}),
    cowboy_req:reply(200,
        [{<<"Content-Encoding">>, <<"utf-8">>}], <<"OK">>, Req); %TODO: form and send reply

store(_, _, Req) ->
    %% Method not allowed.
    cowboy_req:reply(405, Req).

terminate(_Req, _State) ->
    ok.