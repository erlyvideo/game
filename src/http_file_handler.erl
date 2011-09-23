%% Feel free to use, reuse and abuse the code in this file.

-module(http_file_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

-record(file, {
  root
}).

init({_Any, http}, Req, Options) ->
	{ok, Req, #file{root = proplists:get_value(root, Options)}}.

handle(Req, #file{root = Root} = State) ->
  {RequestPath, _} = cowboy_http_req:path(Req),
  case lookup_file([Root | RequestPath]) of
    {ok, Bin} ->
	    {ok, Req2} = cowboy_http_req:reply(200, [{'Content-Length', list_to_binary(integer_to_list(size(Bin)))}], Bin, Req),
	    {ok, Req2, State};
	  {error, _Else} ->
	    {ok, Req2} = cowboy_http_req:reply(404, [], <<"Not found">>, Req),
	    {ok, Req2, State}
	end.    

lookup_file(Path) ->
  case file:read_file(filename:join(Path)) of
    {ok, Bin1} -> {ok, Bin1};
    {error, _} ->
      case file:read_file(filename:join(Path++["index.html"])) of
        {ok, Bin2} -> {ok, Bin2};
        Else -> Else
      end
  end.

terminate(_Req, _State) ->
	ok.
