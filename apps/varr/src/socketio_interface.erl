-module(socketio_interface).
-export([decode/1]).

-define(PATTERN, "(\\d):(\\d+\\+)?:(/[^:]*)?:?(.*)?").

decode(Msg) ->
    case re_decode(Msg) of
        [] -> {[]};
        [Type, MsgID, Endpoint, Data] -> {[Type, MsgID, Endpoint, Data]};
        _ -> {[]}
    end.

re_decode(Msg) ->
    Result = re:run(Msg, ?PATTERN, [{capture, all_but_first, list}]),
    case Result of
        {match, Match} -> Match;
        nomatch -> []
    end.