-module(ndbepi_cluster_mgr).

-include("internal.hrl").

%% -- private --
-export([start_link/2]).

-behaviour(ndbepi_gen_block).
-export([init/0, terminate/2, code_change/3,
         handle_call/6, handle_info/3]).

%% -- internal --
-record(data, {}).

%% == private ==

-spec start_link(node_id(), pos_integer()) -> {ok, pid()}|{error, _}.
start_link(NodeId, BlockNo) ->
    ndbepi_gen_block:start_link(?MODULE, NodeId, BlockNo, []).

%% -- behaviour: ndbepi_gen_block --

init() ->
    setup().

terminate(_Reason, Data) ->
    cleanup(Data).

code_change(_OldVsn, Data, _Extra) ->
    {ok, Data}.

handle_call(_Request, _NodeId, _BlockNo, _Signal, _From, Data) ->
    {stop, enosys, Data}.

handle_info(Signal, Binary, Data) ->
    received(Signal, Binary, Data).

%% == internal ==

cleanup(_) ->
    ok.

setup() ->
    {ok, #data{}}.


received(#signal{gsn=?GSN_API_REGCONF}, <<>>, Data) ->
    %%
    %% ~/include/kernel/signaldata/ApiRegSignalData.hpp: ApiRegConf
    %% ~/src/ndbapi/ClusterMgr.cpp: ClusterMgr::execAPI_REGCONF/2
    %%
    {noreply, Data};
received(#signal{gsn=?GSN_API_REGREF}=S, <<>>, Data) ->
    %%
    %% ~/include/kernel/signaldata/ApiRegSignalData.hpp: ApiRegRef
    %% ~/src/ndbapi/ClusterMgr.cpp: ClusterMgr::execAPI_REGREF/1
    %%
    Reason = case lists:nth(3, S#signal.signal_data) of
                 1 -> <<"WrongType">>;
                 2 -> <<"UnsupportedVersion">>
             end,
    {stop, {shutdown, Reason}, Data};
received(Signal, Binary, Data) ->
    ok = error_logger:warning_msg("[~p:~p] ~p,~p~n", [?MODULE, self(), Signal, Binary]),
    {noreply, Data}.
