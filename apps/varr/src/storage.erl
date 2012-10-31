-module(storage).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3, stop/1]).
-export([save_value/2]).

% public api

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  Host = varr:get_env(redis_host, "127.0.0.1"),
  Port = varr:get_env(redis_port, 6379),
  Db = varr:get_env(redis_db, 3),
  {ok, Redis} = eredis:start_link(Host, Port, Db),
  {ok, Redis}.

stop(_Pid) ->
  stop().

stop() ->
    gen_server:cast(?MODULE, stop).

%% public client api

save_value(Key, Value) ->
  gen_server:cast(?MODULE, {save_value, Key, Value}).

%% genserver handles

% handle_call({get_value, Api, Method}, _From, Redis) ->
%   Response = eredis:q(Redis, ["GET", get_key(Api, Method)]),
%   {reply, Response, Redis};

handle_call({save_value, Key, Value}, _From, Redis) ->
  Response = eredis:q(Redis, ["SET", Key, Value]),
  {reply, Response, Redis};

handle_call(_Message, _From, Redis) ->
  {reply, error, Redis}.

handle_cast({save_value, Key, Value}, Redis) ->
  _Response = eredis:q(Redis, ["SET", Key, Value]),
  {noreply, Redis};

handle_cast(_Meesage, Redis) ->
  {noreply, Redis}.

handle_info(_Message, Redis) -> {noreply, Redis}.
terminate(_Reason, _Redis) -> ok.
code_change(_OldVersion, Redis, _Extra) -> {ok, Redis}.