-module(uuid_server).

-behaviour(gen_server).

-export([start/0, gen/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start() ->
    gen_server:start(?MODULE, [], []).

gen() ->
    gen_server:call(?MODULE, {gen}).

init([]) ->
    {ok, {}}.

handle_call({gen}, From, State) ->
    Reply = uuid:to_string(uuid:v4()),
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
