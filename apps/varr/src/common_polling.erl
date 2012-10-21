-module(common_polling).
-export([timeout_call/1, set_timeout/1, set_timeout/2, do_post_msg/1]).
-define(HEARBEAT_INTERVAL, varr:get_env(heartbeat_interval)*1000).
-define(HEARBEAT_TIMEOUT, varr:get_env(heartbeat_timeout)*1000).

do_post_msg({SessionId, Msg}) ->
    {[Type, MessageId, Endpoint, SubMsgData]} = socketio_interface:decode(Msg),
    case session_server:check(SessionId) of
        false ->
            string:join(["7:", Endpoint, "[\"Request Invalid\"]+[\"Please do not do that!\"]"], ":");
        true ->
            do_handle_post_msg({Type, MessageId, Endpoint, SubMsgData}, {SessionId, Msg}),
            "1"
    end.

timeout_call({SessionId}) ->
    session_server:unregister(SessionId);

timeout_call({SessionId, Endpoint, Type}) ->
    Implement = endpoint_server:lookup(Endpoint),
    Implement:on_disconnect({SessionId, Endpoint, timeout}, fun(SendMsg, Others) ->
                send_call({SessionId, Type, Endpoint}, SendMsg, Others)
    end),
    session_server:unregister(SessionId).

set_timeout(SessionId) ->
    set_timeout(SessionId, ?HEARBEAT_TIMEOUT).

set_timeout(SessionId, Timeout) ->
    Endpoint = session_server:call({SessionId, getEndpoint}),
    Args = case Endpoint of
        undefined ->
            {SessionId};
        _ ->
            {SessionId, Endpoint, "5"}
    end,
    TimeRef = case timer:apply_after(Timeout, ?MODULE, timeout_call, [Args]) of
        {ok, TRef} ->
            TRef;
        {error, _Reason} ->
            undefined
    end,
    session_server:cast({SessionId, timeout, TimeRef}).

do_handle_post_msg({Type, MessageId, Endpoint, SubMsgData}, {SessionId, Msg}) ->
    Implement = endpoint_server:lookup(Endpoint),
    case Type of
        "0" ->
            Implement:on_disconnect({SessionId, Endpoint, SubMsgData}, fun(SendMsg, Others) ->
                send_call({SessionId, Type, Endpoint}, SendMsg, Others)
            end),
            session_server:unregister(SessionId);
        "1" ->
            session_server:cast({SessionId, endpoint, Endpoint}),
            session_server:cast({SessionId, self(), post, Msg}),
            Implement:on_connect({SessionId, MessageId, Endpoint, SubMsgData}, fun(SendMsg, Others) ->
                send_call({SessionId, Type, Endpoint}, SendMsg, Others)
            end);
        "2" ->
            set_timeout(SessionId, ?HEARBEAT_TIMEOUT),
            timer:apply_after(?HEARBEAT_INTERVAL, session_server, cast, [{SessionId, self(), post, "2::"}]);
        "5" ->
            Implement:on_message({SessionId, Type, MessageId, Endpoint, SubMsgData}, fun(SendMsg, Others) ->
                send_call({SessionId, Type, Endpoint}, SendMsg, Others)
            end)
    end.

send_call({SessionId, _, Endpoint}, SendMsg, ack) ->
    Message = {SessionId, self(), post, string:join(["6", "", Endpoint, SendMsg], ":")},
    send_message(Message);

send_call({SessionId, Type, Endpoint}, SendMsg, self) ->
    Message = {SessionId, self(), post, string:join([Type, "", Endpoint, SendMsg], ":")},
    send_message(Message);

send_call(_, _, []) ->
    void;

send_call({_, Type, Endpoint}, SendMsg, TargetSessionIdes = [_|_]) ->
    lists:foreach(fun(TargetSessionId) -> 
            Message = {TargetSessionId, self(), post, string:join([Type, "", Endpoint, SendMsg], ":")},
            send_message(Message)
        end, TargetSessionIdes);

send_call(_, _, {[], _}) ->
    void;

send_call({_, _, Endpoint}, SendMsg, {TargetSessionIds=[_|_], MessageType}) ->
    lists:foreach(fun(TargetSessionId) ->
                        Message = {TargetSessionId, self(), post, string:join([MessageType, "", Endpoint, SendMsg], ":")},
                        send_message(Message)
                  end, TargetSessionIds);

send_call({_, _, Endpoint}, SendMsg, {TargetSessionId, MessageType}) ->
    Message = {TargetSessionId, self(), post, string:join([MessageType, "", Endpoint, SendMsg], ":")},
    send_message(Message);

send_call({_, Type, Endpoint}, SendMsg, TargetSessionId) ->
    Message = {TargetSessionId, self(), post, string:join([Type, "", Endpoint, SendMsg], ":")},
    send_message(Message).

send_message(Message) ->
    session_server:cast(Message).