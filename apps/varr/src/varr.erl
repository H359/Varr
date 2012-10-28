-module(varr).
-export([start/0]).
-export([get_env/1, get_env/2]).


start() ->
    ok = application:start(ranch),
    ok = application:start(cowboy),
    ok = application:start(eredis),
    ok = application:start(hottub),
    ok = application:start(sockjs),
    ok = application:start(varr_app).

get_env(Key, Default) ->
    case get_env(Key) of
        undefined -> Default;
        Value -> Value
    end.

get_env(Key) ->
    case application:get_env(varr, Key) of
        {ok, Value} -> Value;
        _ -> undefined
    end.