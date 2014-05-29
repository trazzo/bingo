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
    application:start(bingo).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", cowboy_static, {priv_file, bingo, "index.html"}},
			{"/bingo/:method", bingo_handler, []},
			{"/static/[...]", cowboy_static, {priv_dir, bingo, "static"}}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 100, [{port, 8080}],
		[{env, [{dispatch, Dispatch}]}]),
    bingo_sup:start_link().

stop(_State) ->
    ok.
