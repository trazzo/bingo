-module(bingo_registry).

-behaviour(gen_server).

%% API
-export([start_link/0, add_observer/2, add_player/3, remove_player/1,
         broadcast/2, get_player_card/1, get_player_name/1, count_players/0,
         deal_new_cards/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {}).

-record(player, {
        connection::pid(),
        name::binary(),
        card::bingo:card() | undefined,
        browser::binary()
}).

-define(ETS, bingo_registry).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, #state{}}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec add_observer(pid(), binary()) -> ok.
add_observer(Conn, UA) ->
    true = ets:insert(?ETS, [#player{connection=Conn, browser=UA,
                                     card=undefined}]),
    ok.

-spec remove_player(pid()) -> non_neg_integer().
remove_player(Conn) ->
    true = ets:delete(?ETS, Conn),
    count_players().

-spec add_player(pid(), binary(), bingo:card()) -> non_neg_integer().
add_player(Conn, DisplayName, Card) ->
    true = ets:update_element(
            ?ETS, Conn, [{#player.card, Card}, {#player.name, DisplayName}]),
    count_players().

-spec deal_new_cards() -> non_neg_integer().
deal_new_cards() ->
    Players = ets:select(
            ?ETS, [{#player{card='$1', connection='$2', _='_'},
                    [{'/=', '$1', undefined}], ['$2']}]),
    ok = lists:foreach(fun (Conn) ->
                    Card = bingo:generate_card(),
                    ets:update_element(?ETS, Conn, [{#player.card, Card}]),
                    Conn ! {msg, {<<"card">>, bingo:card2json(Card)}}
        end, Players),
    count_players().

-spec get_player_card(pid()) -> bingo:card().
get_player_card(Conn) ->
    [InfoPlayer] = ets:lookup(?ETS,Conn),
    InfoPlayer#player.card.       
        
-spec get_player_name(pid()) -> binary() | undefined.
get_player_name(Conn) ->
    case ets:lookup(?ETS,Conn) of
        [InfoPlayer] -> InfoPlayer#player.name;
        [] -> undefined
    end.

-spec count_players() -> non_neg_integer().
count_players() ->
    %Result of number of true (true when is a player registered)
    length(ets:select(?ETS, [{#player{card='$1', _='_'},
                             [{'/=', '$1', undefined}], [true]}])).

-spec broadcast(Type::atom(), Value::atom()) -> ok.
broadcast(Type, Value) ->
    Pids = ets:select(?ETS,[{#player{connection='$1', _='_'}, [], ['$1']}]),
    [Pid ! {msg, {Type, Value}} || Pid <- Pids]. 


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ets:new(?ETS, [named_table, public, set, {read_concurrency,true},
    {write_concurrency, true}, {keypos, #player.connection}]),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
 


%%commit
