-module(ndbepi).

-include("internal.hrl").

%% -- public --
-export([start/0, stop/0]).

%% -- internal --
-type(reason() :: baseline:reason()).

%% == public ==

-spec start() -> ok|{error, reason()}.
start() ->
    baseline:start(?MODULE).

-spec stop() -> ok|{error, reason()}.
stop() ->
    baseline:stop(?MODULE).
