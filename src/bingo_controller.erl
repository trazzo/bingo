-module(bingo_controller).

-behaviour(gen_fsm).

%% API
-export([start_link/0, register_player/2, unregister_player/1, claim/2]).

%% gen_fsm callbacks
-export([init/1,
         waiting_for_players/2,
         waiting_for_players/3,
         countdown/2,
         countdown/3,
         playing/2,
         playing/3,
         game_over/2,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

-record(state, {
        generated::[bingo:bnumber()], %% The numbers generated within a game
        min_players::pos_integer(),
        countdown::pos_integer(), %% In seconds
        time_between_numbers::pos_integer(), %In seconds
        game_over_duration::pos_integer(), %% Seconds between games
        line::boolean() %% Whether a line has been successfully claimed
}).

-type connection()::pid().
-type player_id()::iolist().

%%%===================================================================
%%% API
%%%===================================================================

%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
-spec start_link() -> {ok, pid()}.
%% @end
start_link() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Registers a new player in the bingo game
-spec register_player(connection(), player_id()) ->
    {ok, bingo:card()} | {error, wait_for_next_game}.
%% @end
register_player(Connection, PlayerId) ->
    gen_fsm:sync_send_event(?MODULE, {register, Connection, PlayerId}).

%% @doc Unregisters a player from bingo game
-spec unregister_player(connection()) ->
    ok.
%% @end
unregister_player(Connection) ->
    gen_fsm:send_all_state_event(?MODULE, {unregister, Connection}).

%% @doc Claim a line or a bingo when the game has started
-spec claim(line | bingo, connection()) -> true | false.
%% @end
claim(Claim, Connection) ->
    gen_fsm:sync_send_event(?MODULE, {Claim, Connection}).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

-spec init([]) -> {ok, waiting_for_players, #state{}}.
init([]) ->
    {A, B, C} = os:timestamp(),
    random:seed(A, B, C),
    MinPlayers = bingo:get_config(min_players),
    Countdown = bingo:get_config(countdown),
    TimeBetweenNumbers = bingo:get_config(time_between_numbers),
    GameOverDuration = bingo:get_config (game_over_duration),
    {ok, waiting_for_players,
     #state{min_players=MinPlayers, countdown=Countdown,
            time_between_numbers = TimeBetweenNumbers,
            game_over_duration = GameOverDuration, line=false,
            generated=[]}}.

%%% Handling async events

countdown(start_countdown, #state{countdown=Secs}=State) ->
    bingo_registry:broadcast(
        notification, list_to_binary(
            io_lib:format("Cuenta atras para la partida!", []))),
    gen_fsm:send_event(?MODULE, {count, Secs}),
    {next_state, countdown, State};
    
countdown({count, N}, State) when N>0->
    bingo_registry:broadcast(countdown, N),
    timer:apply_after(1000, gen_fsm, send_event, [?MODULE, {count, N-1}]),
    {next_state, countdown, State};
    
countdown({count, 0}, State) ->
    gen_fsm:send_event(?MODULE, generate_number),
    {next_state, playing, State};
    
countdown(_, State) ->
    {next_state, countdown, State}.

waiting_for_players(_, State) ->
    {next_state, waiting_for_players, State}.

playing(generate_number, #state{generated = Generated,
                                time_between_numbers = TimeBetweenNumbers,
                                game_over_duration = GameOverDuration}=State) ->
    case bingo:generate_number(99, Generated) of
        no_numbers_available ->
            timer:apply_after(GameOverDuration * 1000,
                              gen_fsm, send_event, [?MODULE, restart_game]),
            {next_state, game_over, State};
        Bnumber when is_number(Bnumber) ->
            NewState = State#state{generated=[Bnumber|Generated]},
            bingo_registry:broadcast(bnumber, Bnumber),
            timer:apply_after(TimeBetweenNumbers * 1000, gen_fsm, send_event, [?MODULE, generate_number]),
            {next_state, playing, NewState}
    end;
    
playing(_, State) ->
    {next_state, playing, State}.

game_over(restart_game, #state{min_players=MinPlayers} =State) ->
    bingo_registry:broadcast(
        notification, list_to_binary(
            io_lib:format("Fin de la partida!", []))),
    NewState = State#state{line=false, generated=[]},
    case bingo_registry:deal_new_cards() >= MinPlayers of
        true ->
            gen_fsm:send_event(?MODULE, start_countdown),
            {next_state, countdown, NewState};
        false ->
            bingo_registry:broadcast(
                notification, list_to_binary(
                    io_lib:format("Esperando a que se unan mas jugadores...", []))),
            {next_state, waiting_for_players, NewState}
    end;
game_over(_, State) ->
    {next_state, game_over, State}.

%%% Handling sync events

waiting_for_players({register, Conn, PlayerId}, _From, #state{min_players=Min}=State) ->
    Card = bingo:generate_card(),
    bingo_registry:broadcast(
        notification, list_to_binary(
            io_lib:format("'~s' se ha unido a la partida", [PlayerId]))),
    case bingo_registry:add_player(Conn, PlayerId, Card) >= Min of
        true ->
            gen_fsm:send_event(?MODULE, start_countdown),
            {reply, {ok, Card}, countdown, State};
        false ->
            {reply, {ok, Card}, waiting_for_players, State}
    end.

countdown({register, Conn, PlayerId}, _From, State) ->
    Card = bingo:generate_card(),
    bingo_registry:add_player(Conn, PlayerId, Card),
    bingo_registry:broadcast(
        notification, list_to_binary(
            io_lib:format("'~s' se ha unido a la partida", [PlayerId]))),
    {reply, {ok, Card}, countdown, State}.

playing({line, _Player}, _From, #state{line=true} = State) ->
{reply, false, playing, State};

playing({line, Player}, _From, #state{line=false, generated=NumGenerated} = State) ->
    Card = bingo_registry: get_player_card(Player),
    PlayerName = bingo_registry:get_player_name(Player),
    Result = bingo:validate_line(Card, NumGenerated),
    case Result of
        true ->
            bingo_registry:broadcast(
                notification, list_to_binary(
                    io_lib:format("'~s' ha cantado LINEA!", [PlayerName]))),
            {reply, Result, playing, State#state{line=true}};
        false ->
            bingo_registry:broadcast(
                notification, list_to_binary(
                    io_lib:format("'~s' ha cantado linea invalida", [PlayerName]))),
            {reply, Result, playing, State#state{line=false}}
     end;

playing({bingo, Player}, _From, #state{generated=NumGenerated,
                                       game_over_duration = GameOverDuration} = State) ->
    Card = bingo_registry:get_player_card(Player),
    PlayerName = bingo_registry:get_player_name(Player),
    case bingo:validate_bingo(NumGenerated, Card) of
        true ->
            bingo_registry:broadcast(
                notification, list_to_binary(
                    io_lib:format("'~s' ha cantado BINGO!!!", [PlayerName]))),
            timer:apply_after(GameOverDuration * 1000,
                              gen_fsm, send_event, [?MODULE, restart_game]),
            {reply, true, game_over, State};
        false ->
            bingo_registry:broadcast(
                notification, list_to_binary(
                    io_lib:format("'~s' ha cantado bingo invalido", [PlayerName]))),
            {reply, false, playing, State}
     end;

playing(_Msg, _From, State) ->
    {reply, {error, wait_for_next_game}, State}.

%%% Handling events in all states

handle_event({unregister, Conn}, StateName,
             #state{min_players = MinPlayers,
                    game_over_duration = GameOverDuration} = State) ->
    PlayerName = bingo_registry:get_player_name(Conn),
    NumPlayers = bingo_registry:remove_player(Conn),
    case PlayerName of
        undefined -> ok;
        _ -> 
            bingo_registry:broadcast(notification, list_to_binary(
                io_lib:format("~s ha abandonado la partida", [PlayerName])))
    end,
    if StateName==playing
        andalso NumPlayers < (MinPlayers div 2) ->
            bingo_registry:broadcast(notification, list_to_binary(
                io_lib:format("Numero de jugadores demasiado bajo.'~p'"
                                "Fin del juego.", [NumPlayers]))),
            timer:apply_after(GameOverDuration * 1000,
                              gen_fsm, send_event, [?MODULE, restart_game]),
            {next_state, game_over, State};
       StateName==countdown 
        andalso NumPlayers < MinPlayers ->
            bingo_registry:broadcast( 
                notification, list_to_binary(
                    io_lib:format("Numero de jugadores demasiado bajo.'~p'"
                                "Esperando a que se unan mas...", [NumPlayers]))),
                {next_state, waiting_for_players, State};
       true ->
           {next_state, StateName, State}
    end;

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

%%% Messages, termination and hot code updates

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

