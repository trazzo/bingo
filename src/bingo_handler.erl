-module(bingo_handler).

-behaviour(cowboy_websocket_handler).

%% API
-export([]).

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
    % TODO register the connection (self()) as a game observer in bingo_registry
    Req2 = cowboy_req:compact(Req),
    {ok, Req2, undefined, hibernate}.

websocket_handle({text, Msg}, Req, State) ->
	{reply, {text, << "That's what she said! ", Msg/binary >>}, Req, State};
websocket_handle(_Data, Req, State) ->
	{ok, Req, State}.

websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    % TODO handle the termination of the websocket connection and removing the
    % PID from the bingo_registry
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

