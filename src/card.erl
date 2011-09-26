-module(card).
-include("blackjack.hrl").

-export([total/1]).

total(Cards) when is_list(Cards) ->
  lists:sum([card_value(Value) || #card{value = Value} <- Cards]).

card_value(Value) when Value >= 10 -> 10;
card_value(Value) -> Value.
  