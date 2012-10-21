-module(varr_impl).

-behaviour(socketio_impl).

-export([on_init/1, on_connect/2, on_disconnect/2, on_message/2, on_destroy/1]).

on_init(_Name) ->
    ok.

on_destroy(_Name) ->
    void.

on_connect({_SessionId, _MessageId, _Endpoint, _OriMessage}, _SendFn) ->
    ok.

on_disconnect({_SessionId, _Endpoint, _SubMsgData}, _SendFn) ->
    ok.

on_message({_SessionId, _Type, MessageId, Endpoint, Message}, SendFn) ->
    case string:len(MessageId) > 0  of
        true ->
            Ack = MessageId ++ "[false]",
            SendFn(Ack, ack);
        false -> ok         
    end,
    _Json = hot_tub:call(parser, {process_json, Message}).