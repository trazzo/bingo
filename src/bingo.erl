-module(bingo).

-export([generate_card/0, validate_line/2, validate_bingo/2, get_config/1]).


-type card()::dict(). %% A dictionary with keys 1, 2 and 3 for the rows and
                      %% a list of numbers per row
-type bnumber()::pos_integer(). %% Each one of the numbers used during the game
%% ===================================================================
%% API
%% ===================================================================

%% @doc Generates a new random number for the game.
%% The generated number hasn't been generated earlier in this game.
-spec generate_number([bnumber()]) -> bnumber().
%% @end
generate_number(Generated) ->
    unimplemented.

%% @doc Generates a random bingo card
-spec generate_card() -> card().
%% @end
generate_card() ->
    unimplemented.

%% @doc Validates a line claim
%% Checks whether the card is awarded with a line given the list of numbers
%% that have been generated in the game so far
-spec validate_line([bnumber()], card()) -> boolean().
%% @end
validate_line(Generated, Card) ->
    unimplemented.

%% @doc Validates a bingo claim
%% Checks whether the card is awarded with a line given the list of numbers
%% that have been generated in the game so far
-spec validate_bingo([bnumber()], card()) -> boolean().
%% @end
validate_bingo(Generated, Card) ->
    unimplemented.

%% @doc
-spec get_config(atom()) -> term().
%% @end
get_config(Key) ->
    case application:get_env(bingo, Key) of
        undefined -> undefined;
        {ok, Value} -> Value
    end.

