-module(toppage_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/2]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req), %access the method
    {ok, Data, Req3} = cowboy_req:body(Req2), %access the body of the request
    {ok, Parsed} = json:decode(Data), %parse the data we've found
    {ok, Token} = token(Parsed), %we get token out of JSON parsed data, we'll need it later on
    Parsed2 = update_token_info(Token, Parsed),
    {ok, Req4} = store(Method, Parsed2, Req3), %now we store stuff
    {ok, Req4, State}.

store(<<"POST">>, Data, Req) -> 
    storage:save_value(generate_token(), json:encode({Data})), %TODO: rewrite save_value since we don't provide keys here, we generate them
    cowboy_req:reply(200,
        [{<<"Content-Encoding">>, <<"utf-8">>}], <<"OK">>, Req); %TODO: form and send reply

store(_, _, Req) ->
    %% Method not allowed.
    cowboy_req:reply(405, Req).

terminate(_Req, _State) ->
    ok.

%% Additional functions (json body parsing)

token({Json}) -> 
    Result = proplists:get_value(<<"token">>, Json),
    case Result of
        undefined -> {ok, generate_token()};
        _ -> {ok, Result}
    end.

update_token_info(Token, {Parsed}) ->
    orddict:update(<<"token">>, fun(_) -> Token end, Token, Parsed).

generate_token() ->
    lists:flatten(io_lib:format("~w~w", [self(), crypto:rand_bytes(16)])).