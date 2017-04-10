-module(ndbepi).

-include("internal.hrl").

%% -- public --
-export([start/0, stop/0]).
-export([connect/0]).

%% -- internal --
-type(reason() :: baseline:reason()).

%% == public ==

-spec start() -> ok|{error, reason()}.
start() ->
    baseline:start(?MODULE).

-spec stop() -> ok|{error, reason()}.
stop() ->
    baseline:stop(?MODULE).


-spec connect() -> {ok, pid()}|{error, _}.
connect() ->
    case baseline_app:find(ndbepi_sup, ndbepi_ets, 100, 1) of
        undefined ->
            {error, not_found};
        Pid ->
            connect(baseline_ets:tab(Pid), 10)
    end.

%% == internal ==

connect(Tab, Retry) ->
    case baseline_app:find(ndbepi_sup, ndbepi_connections, 100, 1) of
        undefined ->
            {error, not_found};
        Pid ->
            connect(Pid, max_block_no(Tab) + 1, Retry)
    end.

connect(_Pid, _BlockNo, 0) ->
    {error, ebusy};
connect(Pid, BlockNo, Retry) ->
    case supervisor:start_child(Pid, [BlockNo]) of
        {ok, Child} ->
            {ok, Child};
        {ok, Child, _Info} ->
            {ok, Child};
        {error, ebusy} ->
            connect(Pid, (BlockNo + 1) rem ?MAX_API_BLOCK_NO, Retry - 1);
        {error, Reason} ->
            {error, Reason}
    end.

max_block_no(Tab) ->
    lists:max([?MIN_API_BLOCK_NO|[ K || {K, _} <- ets:tab2list(Tab) ]]).
