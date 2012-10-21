-module(endpoint_server).

-behaviour(gen_server).

-export([start/0]).
-export([register/2, unregister/1, lookup/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {session, endpoint}).

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

register(Endpoint, Implement) ->
    gen_server:call(?MODULE, {register, Endpoint, Implement}).

unregister(Endpoint) ->
    gen_server:call(?MODULE, {unregister, Endpoint}).

lookup(Endpoint) ->
    gen_server:call(?MODULE, {lookup, Endpoint}).

init([]) ->
    {ok, #state{
        session = ets:new(session, [set]),
        endpoint = ets:new(endpoint, [set])
    }}.

handle_call({register, Endpoint, Implement}, _From, State) ->
    Reply = ets:insert(State#state.endpoint, {Endpoint, Implement}),
    Implement:on_init(Endpoint),
    {reply, Reply, State};

handle_call({lookup, Endpoint}, _From, State) ->
    Reply = case ets:lookup(State#state.endpoint, Endpoint) of
        [{_Key, Value}] ->
            Value;
        [] ->
             none
    end,
    {reply, Reply, State};

handle_call({unregister, Endpoint}, _From, State) ->
    Reply = ets:delete(State#state.endpoint, Endpoint),
    {reply, Reply, State};

handle_call(_, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
