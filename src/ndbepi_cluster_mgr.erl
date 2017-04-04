-module(ndbepi_cluster_mgr).

-include("internal.hrl").

-import(ndbepi_util, [find/2]).

%%
%% ~/src/ndbapi/ClusterMgr.cpp: ClusterMgr::trp_deliver_signal/2, ...
%%

%% -- private --
-export([start_link/0]).

-behaviour(gen_server).
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2]).

%% -- private --
-record(state, {
          block_mgr :: pid()
         }).

%% == public ==

-spec start_link() -> {ok, pid()}|{error, _}.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

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

handle_info(#signal{}=S, State) ->
    received(S, State);
handle_info(timeout, Args) ->
    initialized(Args);
handle_info({'EXIT', _Pid, Reason}, State) ->
    {stop, Reason, State}.

%% == internal ==

cleanup(#state{block_mgr=B}=S)
  when B =/= undefined ->
    catch true = baseline_ets:delete(B, ?API_CLUSTERMGR),
    cleanup(S#state{block_mgr = undefined});
cleanup(_) ->
    baseline:flush().

setup(Args) ->
    false = process_flag(trap_exit, true),
    {ok, Args, 0}.


initialized([]) ->
    case find(ndbepi_block_mgr, 10) of
        {ok, Pid} ->
            try baseline_ets:insert(Pid, {?API_CLUSTERMGR, self()}) of
                true ->
                    {noreply, #state{block_mgr = Pid}}
            catch
                error:Reason ->
                    {stop, Reason, undefined}
            end;
        {error, Reason} ->
            {stop, Reason, undefined}
    end.


received(#signal{gsn=?GSN_API_REGCONF}, State) ->
    %%
    %% ~/include/kernel/signaldata/ApiRegSignalData.hpp: ApiRegConf
    %% ~/src/ndbapi/ClusterMgr.cpp: ClusterMgr::execAPI_REGCONF/2
    %%
    %% ApiRegConf
    %% - qmgrRef               = 16515073   = {QMGR,1}
    %% - version               = 460037     = 0x070505
    %% - apiHeartbeatFrequency = 150 (1500/10)           % 100 =<, =< UINT_MAX32?
    %% - mysql_version         = 329489     = 0x050711
    %% - minDbVersion          = 460037     = 0x070505
    %% - nodeState : NodeStatePOD
    %% - - startLevel          = 3          = SL_STARTED
    %% - - nodeGroup           = 0
    %% - - masterNodeId?       = 4294967295 = 0xffffffff
    %% - - ?                   = 16777217
    %% - - ?                   = 1379017808
    %% - - ?                   = 32767
    %% - - singleUserMode      = 0
    %% - - singleUserApi       = 4294967295 = 0xffffffff
    %% - - m_connected_nodes : BitmaskPOD
    %% - - - data[0]           = 18                      % 0x00000012 : 1,4
    %% - - - data[1]           = 0
    %% - - - data[2]           = 134217728               % 0x08000000 : 91
    %% - - - data[3]           = 0
    %% - - - data[4]           = 0
    %% - - - data[5]           = 0
    %% - - - data[6]           = 512                     % 0x00000200 : 201
    %% - - - data[7]           = 0
    {noreply, State};
received(#signal{gsn=?GSN_API_REGREF}=S, State) ->
    %%
    %% ~/include/kernel/signaldata/ApiRegSignalData.hpp: ApiRegRef
    %% ~/src/ndbapi/ClusterMgr.cpp: ClusterMgr::execAPI_REGREF/1
    %%
    Reason = case lists:nth(3, S#signal.signal_data) of % errorCode
                 1 -> <<"WrongType">>;
                 2 -> <<"UnsupportedVersion">>;
                 N -> N
             end,
    {stop, Reason, State};
received(Signal, State) ->
    ok = error_logger:warning_msg("[~p:~p] s=~p~n", [?MODULE, self(), Signal]),
    {noreply, State}.
