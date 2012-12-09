-module(tcp_storage).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3, stop/1]).
-export([save_value/1]).

% public api

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  Host = varr:get_env(redis_host, "127.0.0.1"),
  Port = varr:get_env(tcp_storage_port, 9123),
  io:format("attempting connection~n", []),
  Result = gen_tcp:connect(Host, Port, [binary, {packet, 0}]),
  case Result of
    {ok, Sender} -> 
      io:format("Connected OK: ~p~n", [Result]),
      Result;
    {error, Error} -> 
      io:format("Error occured while connecting~n", []),
      Result
  end.

stop(_Pid) ->
  stop().

stop() ->
  gen_server:cast(?MODULE, stop).

% client api

save_value(Value) ->
  gen_server:cast(?MODULE, {save_value, Value}).

% gen_server api

handle_call(_Message, _From, Sender) ->
  {reply, error, Sender}.

handle_cast({save_value, {ok, Value}}, Sender) ->
  ok = gen_tcp:send(Sender, Value),
  {noreply, Sender};

handle_cast(_Message, Sender) ->
  {noreply, Sender}.

% gen_server handles

terminate(_Reason, Sender) ->
  gen_tcp:close(Sender),
  ok.

handle_info(_Message, Sender) -> {noreply, Sender}.
code_change(_OldVersion, Sender, _Extra) -> {ok, Sender}.