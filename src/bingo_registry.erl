-module(bingo_registry).

-behaviour(gen_server).

%% API
-export([start_link/0, add_observer/2, add_player/3, remove_player/1,
         broadcast/1, send/2, get_player_card/1]).

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
    ets:new(?ETS, [named_table, {read_concurrency,true},
                   {write_concurrency, true}, {keypos, 2}]),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec add_observer(pid(), binary()) -> ok.
add_observer(Conn, UA) ->
    true = ets:insert(?ETS, [#player{connection=Conn, browser=UA,
                                     card=undefined}]),
    ok.

-spec remove_player(pid()) -> ok.
remove_player(Conn) ->
    true = ets:delete(?ETS, Conn).

-spec add_player(pid(), binary(), bingo:card()) -> ok.
add_player(Conn, DisplayName, Card) ->
    true = ets:update_element(
            ?ETS, Conn, [{#player.card, Card}, {#player.name, DisplayName}]).

-spec get_player_card(pid()) -> bingo:card().
get_player_card(Conn) ->
    % TODO Read the card from the ets
    unimplemented.

-spec broadcast(iolist()) -> ok.
broadcast(Message) ->
    % TODO Send the message to every connection Pid in the ETS
    unimplemented.

-spec send(pid(), iolist()) -> ok.
send(Pid, Message) ->
    Pid ! Message.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
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
