-module(game_session).
-include("blackjack.hrl").

-record(session, {
  cards = [],
  game
}).

init() ->
  {ok, Game} = blackjack:join(),
  erlang:monitor(process, Game),
  {ok, #session{
    game = Game
  }}.

handle_info({'DOWN',_,_,_Game,_Reason}, _State) ->
  timer:sleep(100),
  {ok, State1} = init(),
  {noreply, State1};

handle_info(<<"next">>, #session{game = Game} = State) ->
  blackjack:next_player(Game, total(State)),
  {reply, <<"true">>, State};

handle_info(<<"more">>, #session{game = Game, cards = Cards} = State) ->
  {ok, Card} = blackjack:take_more(Game),
  #card{value = Value, suit = Suit} = Card,
  State1 = State#session{cards = [Card|Cards]},
  Total = total(State1),
  
  Json = iolist_to_binary(io_lib:format("{\"card\" : {\"value\" : ~p, \"suit\" : ~p}, \"total\" : ~p}", [Value,Suit,Total])),
  {reply, Json, State1};

handle_info({blackjack, {Message,Total}}, State) ->
  Json = iolist_to_binary(io_lib:format("{\"game_result\" : \"~s\", \"total\" : ~p}", [Message,Total])),
  io:format("message: ~p~n", [Json]),
  {reply, Json, State};

handle_info(Msg, #session{} = State) when is_binary(Msg) ->
  
  {reply, <<"{\"status\" : \"ok\"}">>, State}.

total(#session{cards = Cards}) ->
  card:total(Cards).

