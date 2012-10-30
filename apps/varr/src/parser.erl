-module(parser).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3, stop/1]).

-export([process_json/1]).

start_link() ->
  gen_server:start_link(?MODULE, [], []).

init([]) ->
  {ok, undefined}.

stop(_Pid) ->
  stop().

stop() ->
    gen_server:cast(?MODULE, stop).

% public client api

process_json(Body) ->
    gen_server:call(?MODULE, {process_json, Body}).

% gen_server calls

handle_call({process_json, Body}, _From, State) ->
    {ok, Json} = json:decode(Body),
    {ok, Token} = token(Json),
    Json2 = update_token_info(Token, Json),
    Json3 = update_time_info(Json2),
    storage:save_value(generate_token(), json:encode(Json3)),
    {reply, Token, State};

handle_call(_Message, _From, State) ->
  {reply, error, State}.
    
%% Additional functions (json body parsing)

token({Json}) -> 
    Result = proplists:get_value(<<"token">>, Json),
    case Result of
        undefined -> {ok, generate_token()};
        _ -> {ok, Result}
    end.

update_token_info(Token, {Json}) ->
    {orddict:update(<<"token">>, fun(_) -> Token end, Token, Json)}.

generate_token() ->
    uuid_server:gen().

update_time_info({Json}) ->
    {{Yr, Mt, Dy},{H, M, S}} = {date(), time()},
    NewValue = [Yr, Mt, Dy, H, M, S],
    {orddict:update(<<"server_time">>, fun(_) -> NewValue end, NewValue, Json)}.

% additional functions

handle_cast(_Message, State) -> {noreply, State}.
handle_info(_Message, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.