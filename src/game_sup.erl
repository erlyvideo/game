%% Feel free to use, reuse and abuse the code in this file.

-module(game_sup).
-behaviour(supervisor).

-export([start_link/0]). %% API.
-export([init/1]). %% supervisor.

-define(SUPERVISOR, ?MODULE).

%% API.

-spec start_link() -> {ok, Pid::pid()}.
start_link() ->
	supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

%% supervisor.

init([]) ->
  
  Supervisors = 
  [{   blackjack_sup,
      {blackjack,start_link,[]},
      permanent,            
      brutal_kill,          
      worker,               
      []          
  }],
  
  
	{ok, {{one_for_one, 10, 10}, Supervisors}}.
