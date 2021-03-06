-module(bingo_handler).

-behaviour(cowboy_websocket_handler).

%% API
-export([init/3]).



%% cowboy_http_websocket_handler
-export([websocket_init/3, websocket_handle/3,
         websocket_info/3, websocket_terminate/3]).



%%====================================================================
%% cowboy_http_handler callbacks
%%====================================================================

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

%%====================================================================
%% cowboy_http_wesocket_handler callbacks
%%====================================================================

websocket_init(tcp, Req, _) ->
    {UA, Req2} = cowboy_req:header(<<"user-agent">>, Req, <<"unknown">>),
    Req3 = cowboy_req:compact(Req2),
    bingo_registry:add_observer(self(), UA),
    {ok, Req3, undefined, hibernate}.

websocket_handle({text, Json}, Req, State) ->
    {Data} = jiffy:decode(Json),
    case proplists:get_value(<<"type">>, Data) of
        <<"bingoClaim">> ->
            Result = bingo_controller:claim(bingo, self()),
            Res = jiffy:encode({[{type, <<"bingoClaimResult">>}, {content, Result}]}),
            {reply, {text, Res}, Req, State};
            
        <<"lineClaim">> ->
            Result = bingo_controller:claim(line, self()),
            Res = jiffy:encode({[{type, <<"lineClaimResult">>}, {content, Result}]}),
            {reply, {text, Res}, Req, State};
            
        <<"register">> ->
            DisplayName = proplists:get_value(<<"content">>, Data),
            case bingo_controller:register_player(self(), DisplayName) of
                {ok, Card} -> 
                    Res = jiffy:encode({[{type, <<"card">>}, {content, bingo:card2json(Card)}]}),
                    {reply, {text, Res}, Req, State}; %%text ponse por cowboy para poder Res
                {error, wait_for_next_game} -> 
                    Msg = list_to_binary(io_lib:format(
                        "Error: registro inhabilitado hasta el final de la partida~n", [])),
                    Res = jiffy:encode({[{type, <<"notification">>}, {content, Msg}]}),
                    {reply, {text, Res}, Req, State}
            end
    end.

websocket_info({msg, {Type, Value}}, Req, State) ->
    Json = jiffy:encode({[{type, Type}, {content, Value}]}),
    {reply, {text, Json}, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    Conn = self(),
    bingo_controller:unregister_player(Conn).

%%====================================================================
%% Internal Functions
%%====================================================================

