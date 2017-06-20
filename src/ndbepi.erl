-module(ndbepi).

-include("internal.hrl").

%% -- public --
-export([start/0, stop/0]).
-export([connect/0, connect/1, connect/2, disconnect/1]).

%% -- internal --
-type(instance() :: integer()).
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
    connect(0).

-spec connect(node_id()) -> {ok, pid()}|{error, _}.
connect(NodeId) ->
    connect(NodeId, 0).

-spec connect(node_id(), instance()) -> {ok, pid()}|{error, _}.
connect(NodeId, Instance) ->
    case baseline_app:find(ndbepi_sup, ndbepi_connections, 100, 1) of
        undefined ->
            {error, not_found};
        SupRef ->
            case start_child(SupRef, 10) of
                {ok, Pid} ->
                    case ndbepi_connection:connect(Pid, NodeId, Instance) of
                        ok ->
                            {ok, Pid};
                        {error, Reason} ->
                            ok = supervisor:terminate_child(SupRef, Pid),
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end
    end.

-spec disconnect(pid()) -> ok|{error, _}.
disconnect(Pid) ->
    case baseline_app:find(ndbepi_sup, ndbepi_connections, 100, 1) of
        undefined ->
            {error, not_found};
        SupRef ->
            supervisor:terminate_child(SupRef, Pid)
    end.

%% == internal ==

max_block_no(Tab) ->
    lists:max([?MIN_API_BLOCK_NO|[ K || {K, _} <- ets:tab2list(Tab) ]]).

next_block_no(Tab) ->
    (max_block_no(Tab) + 1) rem ?MAX_API_BLOCK_NO.

start_child(SupRef, Retry) ->
    case baseline_app:find(ndbepi_sup, ndbepi_ets, 100, 1) of
        undefined ->
            {error, not_found};
        Pid ->
            start_child(SupRef, baseline_ets:tab(Pid), Retry)
    end.

start_child(_SupRef, _Tab, 0) ->
    {error, ebusy};
start_child(SupRef, Tab, Retry) ->
    case supervisor:start_child(SupRef, [next_block_no(Tab)]) of
        {ok, Child} ->
            {ok, Child};
        {ok, Child, _Info} ->
            {ok, Child};
        {error, ebusy} ->
            start_child(SupRef, Tab, Retry - 1);
        {error, Reason} ->
            {error, Reason}
    end.
