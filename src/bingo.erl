-module(bingo).

-export([generate_card/0, validate_line/2, validate_bingo/2, get_config/1, create_rows/2, 
         generate_number/2, split_list/1, intro_zeros/2, get_row/2, delete_zero/1, 
         card2json/1, check_card/1]).


-type card()::dict:dict(pos_integer(), [bnumber()]). %% A dictionary with keys 1, 2 and 3 for the rows and
                      %% a list of numbers per row
-type bnumber()::pos_integer(). %% Each one of the numbers used during the game

-export_type([card/0]).
%% ===================================================================
%% API
%% ===================================================================

%% @doc Generates a new random number for the game.
%% The generated number hasn't been generated earlier in this game.
-spec generate_number(pos_integer, [bnumber()]) -> bnumber().
%% @end
generate_number(Limit, Generated) ->
   N = random:uniform(Limit),
    case lists:member(N, Generated) of
         true -> 
             generate_number(Limit,Generated);
         false -> 
             N
    end.

%% @doc Generates a random bingo card
-spec generate_card() -> card().
%% @end
generate_card() ->
    {F1, F2, F3} = create_rows(15, 90),
    dict:from_list(lists:zip([1, 2, 3], erlang:tuple_to_list({F1, F2, F3}))).
                   
%% @doc Validates a line claim
%% Checks whether the card is awarded with a line given the list of numbers
%% that have been generated in the game so far
-spec validate_line(card(), [bnumber()]) -> boolean().
%% @end
validate_line(Card, Generated) ->
    Lines = check_card(Card),
    lists:any(fun(Line) -> all_in(Line, Generated) end, Lines).

%% @doc Validates a bingo claim
%% Checks whether the card is awarded with a line given the list of numbers
%% that have been generated in the game so far
-spec validate_bingo([bnumber()], card()) -> boolean().
%% @end
validate_bingo(Generated, Card) ->
    Lines = check_card(Card),
    Bingo_numbers = lists:append(Lines),
    all_in(Bingo_numbers, Generated).
    

%% @doc
-spec get_config(atom()) -> term().
%% @end
get_config(Key) ->
    case application:get_env(bingo, Key) of
        undefined -> default(Key);
        {ok, Value} -> Value
    end.
    
default(min_players) ->
    2;
default(countdown) ->
    3;
defaut(time_between_numbers) ->
    5;
default(game_over_duration) ->
    20.

-spec card2json(card()) -> iolist().
card2json(Card) ->
   {[
       {row1, get_row(1, Card)},
       {row2, get_row(2, Card)},
       {row3, get_row(3, Card)}
   ]}.

%%=====================================================================================
%%=============================INTERNAL FUNCTIONS======================================
%%=====================================================================================

create_rows(_Size, _Last) ->
    create_rows(15, 90, []).
 
create_rows(0, _Last, Randomized) ->
    SortedList = lists:sort(Randomized),
    {F1, F2, F3} = split_list(SortedList),
    File1 = intro_zeros(F1, 4),
    File2 = intro_zeros(F2, 4),
    File3 = intro_zeros(F3, 4),
    {File1, File2, File3};
                     
create_rows(Size, Last, Randomized) ->
    Number = [generate_number(Last, Randomized)],
    NewRandomized = Randomized ++ Number,
    create_rows(Size-1, Last, NewRandomized).      

get_row(Num_row,Card) when Num_row =< 3 ->
    dict:fetch(Num_row, Card).       
    
                    
split_list(List) ->
    {F1, T} = lists:split(5, List),
    {F2, F3} = lists:split(5, T),
    {F1, F2, F3}.
            
intro_zeros(L, Iterations) ->
    intro_zeros_aux(L, Iterations).
       
intro_zeros_aux(L, 0) ->
    L;

intro_zeros_aux(L, Iterations) ->
    Pos = random:uniform(5),
    {L1, L2} = lists:split(Pos,L),
    ZeroIn = L1 ++ [0],
    NewList = ZeroIn ++ L2,
    intro_zeros(NewList, Iterations-1).
      
check_card(Card) ->
    [delete_zero(Row) || {_, Row} <- dict:to_list(Card)].
    
delete_zero(List) ->
    lists:filter(fun (N) ->
                     case N of
                         0 -> false;
                         _ -> true
                     end
                 end, List).

all_in(L, Generated) ->
    lists:all(fun(N) -> lists:member(N, Generated) end, L).


    
                   



