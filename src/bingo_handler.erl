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
    io:format("Received from client: ~p~n", [Json]),
    Data = jiffy:decode(Json),
    io:format("Received from client: ~p~n", [Data]),
	{ok, Req, State}.

websocket_info(Msg, Req, State) ->
    Json = jiffy:encode(Msg),
    {reply, {text, Json}, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    io:format("Terminating connection~n"),
    bingo_registry:remove_player(self()) .


%%====================================================================
%% Internal Functions
%%====================================================================
%%commit 


%%commit
