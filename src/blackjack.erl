-module(blackjack).
-behaviour(gen_server).

-include("blackjack.hrl").
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([join/0, take_more/1]).

-compile(export_all).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-record(blackjack, {
  cards = [],
  players = [],
  state
}).


join() ->
  gen_server:call(?MODULE, {join, self()}).
  
take_more(Game) ->
  gen_server:call(Game, take_more).
  
next_player(Game, PlayerTotal) ->
  gen_server:call(Game, {next_player, self(), PlayerTotal}).
  

init([]) ->
  State = #blackjack{},
  State1 = init_cards(State),
  State2 = shuffle_cards(State1),
  {ok, State2}.

handle_call({next_player, _CurrentPlayer, PlayerTotal}, _From, State) ->
  {Total, State1} = ai_take_cards(State),
  io:format("Player: ~p, ai: ~p~n", [PlayerTotal, Total]),
  Message = get_winner(Total, PlayerTotal),
  notify_all_users(State, {Message, Total}),
  {reply, ok, State1};
  
handle_call({join, Player}, _From, #blackjack{players = Players} = State) ->
  erlang:monitor(process, Player),
  {reply, {ok, self()}, State#blackjack{players = [Player|Players]}};

handle_call(take_more, _From, #blackjack{cards = [Card|Cards]} = State) ->
  {reply, {ok, Card}, State#blackjack{cards = Cards}};

handle_call(_Call, _From, State) ->
  {stop, {unknown_call, _Call}, State}.

handle_cast(_Cast, State) ->
  {stop, {unknown_cast, _Cast}, State}.

handle_info({'DOWN',_,_,Player,_Reason}, #blackjack{players = Players} = State) ->
  {noreply, State#blackjack{players = lists:delete(Player, Players)}};

handle_info(Msg, State) ->
  {stop, {unknown_info, Msg}, State}.


code_change(_, State, _) -> {ok, State}.
terminate(_, _) -> ok.


init_cards(State) ->
  Cards = lists:flatten([init_pack() || _N <- lists:seq(1,?MAX_PACKS)]),
  State#blackjack{cards = Cards}.

init_pack() ->
  [#card{value = V, suit = S} || V <- lists:seq(1, ?MAX_VALUE), S <- lists:seq(1, ?MAX_SUIT)].
  
shuffle_cards(#blackjack{cards = Cards} = State) ->
  State#blackjack{cards = shuffle_cards(Cards, [])}.
  
shuffle_cards([Card], Acc) ->
  [Card|Acc];
  
shuffle_cards(Cards, Acc) ->
  Number = random:uniform(length(Cards)),
  {Card, Rest} = take_nth(Number, Cards),
  shuffle_cards(Rest, [Card|Acc]).
  
take_nth(Number, Cards) ->
  take_nth(Number, Cards, []).

take_nth(1, [Card|Cards], Acc) ->
  {Card, Cards ++ Acc};

take_nth(Number, [Card|Cards], Acc) ->
  take_nth(Number - 1, Cards, [Card|Acc]).


ai_take_cards(#blackjack{cards = Cards} = State) ->
  {Total, Rest} = ai_take_cards(Cards, []),
  {Total, State#blackjack{cards = Rest}}.

ai_take_cards([Card|Cards], Acc) ->
  case card:total(Acc) of
    Total when Total > 15 -> {Total, Cards};
    _ -> ai_take_cards(Cards, [Card|Acc])
  end.


get_winner(AITotal, PlayerTotal) when AITotal > 21 andalso PlayerTotal > 21 ->
  <<"nobody_win">>;
  
get_winner(AITotal, _PlayerTotal) when AITotal > 21 ->
  <<"player_win">>;

get_winner(_AITotal, PlayerTotal) when PlayerTotal > 21 ->
  <<"ai_win">>;

get_winner(AITotal, PlayerTotal) when AITotal > PlayerTotal ->
  <<"ai_win">>;

get_winner(AITotal, PlayerTotal) when PlayerTotal > AITotal ->
  <<"player_win">>;

get_winner(Total, Total) ->
  <<"nobody_win">>.
  
notify_all_users(#blackjack{players = Players}, Message) ->
  [Player ! {blackjack, Message} || Player <- Players].
  