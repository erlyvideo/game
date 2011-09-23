%% Feel free to use, reuse and abuse the code in this file.

-module(game).
-behaviour(application).
-export([start/0, start/2, stop/1]).

start() ->
	application:start(crypto),
	application:start(public_key),
	application:start(ssl),
	application:start(cowboy),
	sync:go(),
	application:start(game).

start(_Type, _Args) ->
	Dispatch = [
		{'_', [
			{[<<"websocket">>], websocket_handler, []},
			{'_', http_file_handler, [{root, "priv/wwwroot"}]}
		]}
	],
	cowboy:start_listener(http, 100,
		cowboy_tcp_transport, [{port, 8000}],
		cowboy_http_protocol, [{dispatch, Dispatch}]
	),
	game_sup:start_link().

stop(_State) ->
	ok.
