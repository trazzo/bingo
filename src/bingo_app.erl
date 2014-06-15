-module(bingo_app).

-behaviour(application).

-export([start/0]).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% API
%% ===================================================================
-spec start() -> ok.
start() ->
    lists:foreach(fun (Dep) -> application:start(Dep) end, [crypto, ranch, cowlib, cowboy]),
    application:start(bingo).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", cowboy_static, {file, "priv/index.html"}},
			{"/static/[...]", cowboy_static, {dir, "priv/static/"}},
			{"/bingo/:method", bingo_handler, []}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 100, [{port, 8081}],
		[{env, [{dispatch, Dispatch}]}]),
    bingo_sup:start_link().

stop(_State) ->
    ok.
