-module(toppage_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/2]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    io:format("~w~n", [State]),
    {Method, Req2} = cowboy_req:method(Req), %access the method
    {ok, Data, Req3} = cowboy_req:body(Req2), %access the body of the request
    {ok, Parsed} = parse_json(Data), %parse the data we've found
    {ok, Token} = token(Parsed), %we get token out of JSON parsed data, we'll need it later on
    {ok, Req4} = store(Method, Parsed, Req3), %now we store stuff
    {ok, Req4, State}.

store(<<"POST">>, Data, Req) -> 
    storage:save_value(none, Data), %TODO: rewrite save_value since we don't provide keys here, we generate them
    cowboy_req:reply(200,
        [{<<"Content-Encoding">>, <<"utf-8">>}], <<"OK">>, Req); %TODO: form and send reply

store(_, _, Req) ->
    %% Method not allowed.
    cowboy_req:reply(405, Req).

terminate(_Req, _State) ->
    ok.

%% Additional functions (json body parsing)

parse_json(Body) ->
    json:decode(Body). %TODO: does it need to be this way? shouldn't we just use "json:decode" in-place?

lookup(Key, Json) ->
    case Json of
        {[{Key, Value} | _]} -> {ok, Value};
        {[_ | Rest]} -> lookup(Key, {Rest});
        _ when not(is_tuple(Json)) -> {error, badarg}; %not a JSON object
        _ -> {not_found}
    end.

token(Json) ->
    case lookup(<<"token">>, Json) of
        {ok, Token} -> {ok, Token};
        _ -> {ok, undefined} %token is undefined - we'll provide a new one
    end.