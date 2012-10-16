-module(session_server).

-behaviour(gen_server).

-export([start/0]).
-export([call/1, cast/1]).
-export([register/1, unregister/1, check/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(session, {subscribed = false, messages = [], defined, timeRef, endpoint, transport}).
-define(SESSION_TAB, session_tab).
-record(state, {}).

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

register(SessionId) ->
    gen_server:cast(?MODULE, {register, SessionId}).

unregister(SessionId) ->
    gen_server:cast(?MODULE, {unregister, SessionId}).

check(SessionId) ->
    gen_server:call(?MODULE, {lookup, SessionId}).

call(Request) ->
    gen_server:call(?MODULE, Request).

cast(Request) ->
    gen_server:cast(?MODULE, Request).

init([]) ->
    ets:new(?SESSION_TAB, [set, public, named_table, {write_concurrency, true}, {read_concurrency, true}]),
    {ok, #state{}}.

%% calls

handle_call({SessionId, getEndpoint}, _From, State) ->
    Session = get_session(SessionId),
    Reply = Session#session.endpoint,
    {reply, Reply, State};

handle_call({lookup, SessionId}, _From, State) ->
    Reply = case ets:lookup(?SESSION_TAB, SessionId) of
        [] -> false;
        [_] -> true
    end,
    {reply, Reply, State};

handle_call(_Msg, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% casts

handle_cast({register, SessionId}, State) ->
    ets:insert(?SESSION_TAB, {SessionId, #session{}}),
    {noreply, State};

handle_cast({unregister, SessionId}, State) ->
    Session = get_session(SessionId),
    case Session#session.timeRef of
        undefined -> ok;
        _ -> timer:cancel(Session#session.timeRef)
    end,
    ets:delete(?SESSION_TAB, SessionId),
    {noreply, State};

handle_cast({SessionId, From, subscribe, Transport}, State) ->
    Session = get_session(SessionId),
    case Session#session.messages of
        [] ->
            NewDefined = From,
            NewMessages = [],
            
            NewSubscribed = if
                Session#session.subscribed == false ->
                    From ! {reply, first},
                    true;
                true ->
                    true
            end;
        [H|T] ->
            From ! {reply, H},
            NewDefined = undefined,
            NewMessages = T,
            NewSubscribed = true
    end,
    ets:insert(?SESSION_TAB, {SessionId, Session#session{subscribed = NewSubscribed, messages = NewMessages, defined = NewDefined, transport = Transport}}),
    {noreply, State};

handle_cast({SessionId, timeout, NewTimeRef}, State) ->
    Session = get_session(SessionId),
    case Session#session.timeRef of
        undefined -> ok;
        _ -> timer:cancel(Session#session.timeRef)
    end,
    ets:insert(?SESSION_TAB, {SessionId, Session#session{timeRef = NewTimeRef}}),
    {noreply, State};

handle_cast({SessionId, end_connect}, State) ->
    Session = get_session(SessionId),   
    ets:insert(?SESSION_TAB, {SessionId, Session#session{defined = undefined}}),
    {noreply, State};

handle_cast({SessionId, endpoint, NewEndpoint}, State) ->
    Session = get_session(SessionId),   
    ets:insert(?SESSION_TAB, {SessionId, Session#session{endpoint = NewEndpoint}}), 
    {noreply, State};

handle_cast({SessionId, From, post, Message}, State) ->
    Session = get_session(SessionId),
    {NewMessages, NewDefined} = handle_post_msg({From, Message}, Session, Session#session.transport),
    ets:insert(?SESSION_TAB, {SessionId, Session#session{messages = NewMessages, defined = NewDefined}}),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_post_msg({_, Message}, Session, websocket) ->
    NewMessages = case Session#session.defined of
        undefined ->
            %lager:debug("undefined~n", []),
            lists:merge(Session#session.messages, [Message]);
        Pid ->
            Pid ! {reply, Message},
            Session#session.messages
    end,
    {NewMessages, Session#session.defined};

handle_post_msg({_, Message}, Session, htmlfile) ->
    NewMessages = case Session#session.defined of
        undefined ->
            lists:merge(Session#session.messages, [Message]);
        Pid ->
            Pid ! {reply, Message},
            Session#session.messages
    end,
    {NewMessages, Session#session.defined};

handle_post_msg({_, Message}, Session, _) ->
    case Session#session.defined of
        undefined ->
            NewDefined = Session#session.defined,
            NewMessages = lists:merge([Message], Session#session.messages);
        Pid ->
            Pid ! {reply, Message},
            NewDefined = undefined,
            NewMessages = Session#session.messages
    end,
    {NewMessages, NewDefined}.

get_session(SessionId) ->
    Result = ets:lookup(?SESSION_TAB, SessionId),
    case Result of
        [{_Key, Reply}] ->
            Reply;
        [] ->
            #session{}
    end.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
