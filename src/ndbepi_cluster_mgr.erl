-module(ndbepi_cluster_mgr).

-include("internal.hrl").

%% -- private --
-export([start_link/2]).

-behaviour(gen_server).
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2]).

%% -- internal --
-record(state, {
          node_id  :: node_id(),
          block_no :: pos_integer(),
          ets      :: undefined|pid()
         }).

%% == private ==

-spec start_link(node_id(), pos_integer()) -> {ok, pid()}|{error, _}.
start_link(NodeId, BlockNo) ->
    gen_server:start_link(?MODULE, [NodeId, BlockNo], []).

%% -- behaviour: gen_server --

init(Args) ->
    setup(Args).

terminate(_Reason, State) ->
    cleanup(State).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call(_Request, _From, State) ->
    {stop, enosys, State}.

handle_cast(_Request, State) ->
    {stop, enosys, State}.

handle_info({#signal{}=S, Binary}, State) ->
    received(S, Binary, State);
handle_info(timeout, Args) ->
    initialized(Args);
handle_info({'EXIT', _Pid, Reason}, State) ->
    {stop, Reason, State}.

%% == internal ==

cleanup(#state{ets=E}=X)
  when E =/= undefined ->
    catch true = baseline_ets:delete(E, X#state.block_no),
    cleanup(X#state{ets = undefined});
cleanup(_) ->
    baseline:flush().

setup(Args) ->
    false = process_flag(trap_exit, true),
    {ok, Args, 0}.


initialized([NodeId, BlockNo]) ->
    case baseline_app:find(ndbepi_sup, ndbepi_ets, 100, 10) of
        undefined ->
            {stop, not_found, undefined};
        Pid ->
            found(#state{node_id = NodeId, block_no = BlockNo, ets = Pid})
    end.

found(#state{block_no=B, ets=E}=X) ->
    case baseline_ets:insert_new(E, {B, self()}) of
        true ->
            registered(X)
    end.

registered(State) ->
    {noreply, State}.


received(#signal{gsn=?GSN_API_REGCONF}, <<>>, State) ->
    %%
    %% ~/include/kernel/signaldata/ApiRegSignalData.hpp: ApiRegConf
    %% ~/src/ndbapi/ClusterMgr.cpp: ClusterMgr::execAPI_REGCONF/2
    %%
    {noreply, State};
received(#signal{gsn=?GSN_API_REGREF}=S, <<>>, State) ->
    %%
    %% ~/include/kernel/signaldata/ApiRegSignalData.hpp: ApiRegRef
    %% ~/src/ndbapi/ClusterMgr.cpp: ClusterMgr::execAPI_REGREF/1
    %%
    Reason = case lists:nth(3, S#signal.signal_data) of
                 1 -> <<"WrongType">>;
                 2 -> <<"UnsupportedVersion">>
             end,
    {stop, {shutdown, Reason}, State};
received(Signal, Binary, State) ->
    ok = error_logger:warning_msg("[~p:~p] ~p,~p~n", [?MODULE, self(), Signal, Binary]),
    {noreply, State}.
