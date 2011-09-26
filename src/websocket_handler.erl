%% Feel free to use, reuse and abuse the code in this file.

-module(websocket_handler).
-behaviour(cowboy_http_handler).
-behaviour(cowboy_http_websocket_handler).
-export([init/3, handle/2, terminate/2]).
-export([websocket_init/3, websocket_handle/3, websocket_info/3, websocket_terminate/3]).

init({_Any, http}, Req, []) ->
	case cowboy_http_req:header('Upgrade', Req) of
		{undefined, Req2} -> {ok, Req2, undefined};
		{<<"websocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket};
		{<<"WebSocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket}
	end.

handle(Req, State) ->
	{ok, Req2} = cowboy_http_req:reply(404, [{'Content-Type', <<"text/html">>}], <<"Not found">>, Req),
	{ok, Req2, State}.

terminate(_Req, _State) ->
	ok.

websocket_init(_Any, Req, []) ->
	Req2 = cowboy_http_req:compact(Req),
	{ok, State} = game_session:init(),
	{ok, Req2, State, hibernate}.

websocket_handle({text, Msg}, Req, State) ->
  io:format("Hi! ~p~n", [Msg]),
  case game_session:handle_info(Msg, State) of
    {reply, Reply, State1} ->
      {reply, {text, Reply}, Req, State1, hibernate};
    {stop, _Reason, State1} ->
      {stop, _Reason, State1}
  end;  
	
websocket_handle(_Any, Req, State) ->
	{ok, Req, State}.

websocket_info(tick, Req, State) ->
	{reply, {text, <<"Tick">>}, Req, State, hibernate};
	
websocket_info(Info, Req, State) ->
  case game_session:handle_info(Info, State) of
    {noreply, State1} ->
	    {ok, Req, State1, hibernate};
	  {reply, Reply, State1} ->
	    {reply, {text, Reply}, Req, State1, hibernate};
	  {stop, _Reason, State1} ->
	    {stop, _Reason, State1}
	end.

websocket_terminate(_Reason, _Req, _State) ->
	ok.
