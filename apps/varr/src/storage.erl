-module(storage).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3, stop/1]).
-export([save_value/2, save_value_set/1]).

% public api

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  Host = varr:get_env(redis_host, "127.0.0.1"),
  Port = varr:get_env(redis_port, 6379),
  Db = varr:get_env(redis_db, 3),
  Password = varr:get_env(redis_pass, none),
  case Password of
    none -> {ok, Redis} = eredis:start_link(Host, Port, Db);
    Pass -> {ok, Redis} = eredis:start_link(Host, Port, Db, Password)
  end,  
  {ok, Redis}.

stop(_Pid) ->
  stop().

stop() ->
    gen_server:cast(?MODULE, stop).

%% public client api

save_value(Key, Value) ->
  gen_server:cast(?MODULE, {save_value, Key, Value}).

save_value_set(Value) ->
  gen_server:cast(?MODULE, {save_value_set, Value}).

%% genserver handles

% handle_call({get_value, Api, Method}, _From, Redis) ->
%   Response = eredis:q(Redis, ["GET", get_key(Api, Method)]),
%   {reply, Response, Redis};

handle_call({save_value, Key, {ok, Value}}, _From, Redis) ->
  Response = eredis:q(Redis, ["SET", Key, Value]),
  {reply, Response, Redis};

handle_call({save_value_set, {ok, Value}}, _From, Redis) ->
  Response = eredis:q(Redis, ["SADD", "VarrStats", Value]),
  {reply, Response, Redis};

handle_call(_Message, _From, Redis) ->
  {reply, error, Redis}.

handle_cast({save_value, Key, {ok, Value}}, Redis) ->
  _Response = eredis:q(Redis, ["SET", Key, Value]),
  {noreply, Redis};

handle_cast({save_value_set, {ok, Value}}, Redis) ->
  _Response = eredis:q(Redis, ["SADD", "VarrStats", Value]),
  {noreply, Redis};

handle_cast(_Meesage, Redis) ->
  {noreply, Redis}.

handle_info(_Message, Redis) -> {noreply, Redis}.
terminate(_Reason, _Redis) -> ok.
code_change(_OldVersion, Redis, _Extra) -> {ok, Redis}.