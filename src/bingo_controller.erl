-module(bingo_controller).

-behaviour(gen_fsm).

%% API
-export([start_link/0, register_player/2, claim/2]).

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
        game_over_duration::pos_integer(), %% Seconds between games
        line::boolean() %% Turns on a successful line claim
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
-spec register_player(connection(), player_id()) -> ok | wait_for_next_game.
%% @end
register_player(Connection, PlayerId) ->
    gen_fsm:sync_send_event(?MODULE, {register, Connection, PlayerId}).

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
    MinPlayers = bingo:get_config(min_players),
    Countdown = bingo:get_config(countdown),
    {ok, waiting_for_players,
     #state{min_players=MinPlayers, countdown=Countdown, line=false,
            generated=[]}}.

%%% Handling async events

waiting_for_players(_Event, State) ->
    {next_state, state_name, State}.

countdown(tick, State) ->
    % TODO update the countdown in every connection
    % timer:send_after the next tick
    {next_state, countdown, State};
countdown(timeout, State) ->
    % TODO Broadcast the transition to playing
    {next_state, playing, State}.

playing(generate_number, State) ->
    % TODO get a new random_number from the bingo module
    % * broadcast the random number to all connected players
    % timer:send_after the next generate_number
    {next_state, playing, State};
playing(_Event, State) ->
    {next_state, playing, State}.

game_over(timeout, State) ->
    {next_state, waiting_for_players, State#state{line=false, generated=[]}};
game_over(_Event, State) ->
    {next_state, game_over, State}.

%%% Handling sync events

waiting_for_players({register, Conn, PlayerId}, _From, State) ->
    % TODO Add the player to the bingo_registry
    % * get a new card from the bingo module
    % If MIN_PLAYERS has been reached:
    % * send an async tick event
    % * transition to countdown using a timeout
    {reply, new_card, waiting_for_players, State}.

countdown({register, Conn, PlayerId}, _From, State) ->
    % TODO Add the player to the bingo_registry
    {reply, new_card, countdown, State}.

playing({line, Player}, _From, State) ->
    % TODO check whether a line has been claimed before
    % * Get the card for that player and the list of generated numbers
    % * Use the bingo module to validate the claim
    % * Return the result of the validation and update the line claim flag
    {reply, true, playing, State#state{line=true}};
playing({bingo, Player}, _From, #state{game_over_duration=GameOver}=State) ->
    % TODO
    % * Get the card for that player and the list of generated numbers
    % * Use the bingo module to validate the claim
    % If the claim is valid, annonce the winner and transition to game_over
    {reply, true, game_over, State, GameOver * 1000}.

%%% Handling events in all states

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

