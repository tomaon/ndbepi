-module(ndbepi_cluster_mgr).

-include("internal.hrl").

%% -- private --
-export([start_link/2]).

-behaviour(ndbepi_gen_block).
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_info/3]).

%% -- internal --
-record(data, {
          node_id  :: node_id(),
          block_no :: block_no()
         }).

%% == private ==

-spec start_link(node_id(), block_no()) -> {ok, pid()}|{error, _}.
start_link(NodeId, BlockNo) ->
    ndbepi_gen_block:start_link(?MODULE, BlockNo, [NodeId, BlockNo], []).

%% -- behaviour: ndbepi_gen_block --

init([NodeId, BlockNo]) ->
    {ok, #data{node_id = NodeId, block_no = BlockNo}}.

terminate(_Reason, _Data) ->
    ok.

code_change(_OldVsn, Data, _Extra) ->
    {ok, Data}.

handle_call(_Request, _Signal, Data) ->
    {stop, enosys, Data}.

handle_info(#signal{gsn=?GSN_API_REGCONF}, <<>>, Data) ->
    %%
    %% ~/include/kernel/signaldata/ApiRegSignalData.hpp: ApiRegConf
    %% ~/src/ndbapi/ClusterMgr.cpp: ClusterMgr::execAPI_REGCONF/2
    %%
    {noreply, Data};
handle_info(#signal{gsn=?GSN_API_REGREF, signal_data=D}, <<>>, Data) ->
    %%
    %% ~/include/kernel/signaldata/ApiRegSignalData.hpp: ApiRegRef
    %% ~/src/ndbapi/ClusterMgr.cpp: ClusterMgr::execAPI_REGREF/1
    %%
    Reason = case lists:nth(3, D) of
                 1 -> <<"WrongType">>;
                 2 -> <<"UnsupportedVersion">>
             end,
    {stop, {shutdown, Reason}, Data};
handle_info(Signal, Binary, Data) ->
    ok = error_logger:warning_msg("[~p:~p] ~p,~p~n", [?MODULE, self(), Signal, Binary]),
    {noreply, Data}.
