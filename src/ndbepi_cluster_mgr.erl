-module(ndbepi_cluster_mgr).

-include("internal.hrl").

%% -- private --
-export([start_link/0]).

-behaviour(gen_server).
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2]).

%% -- internal --
-record(state, {
          ets :: undefined|pid()
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

handle_info({#signal{}=S, Binary}, State) ->
    received(S, Binary, State);
handle_info(timeout, Args) ->
    initialized(Args);
handle_info({'EXIT', _Pid, Reason}, State) ->
    {stop, Reason, State}.

%% == internal ==

cleanup(#state{ets=E}=X)
  when E =/= undefined ->
    catch true = baseline_ets:delete(E, ?API_CLUSTERMGR),
    cleanup(X#state{ets = undefined});
cleanup(_) ->
    baseline:flush().

setup(Args) ->
    false = process_flag(trap_exit, true),
    {ok, Args, 0}.


initialized([]) ->
    case baseline_app:find(ndbepi_sup, ndbepi_ets, 100, 10) of
        undefined ->
            {stop, not_found, undefined};
        Pid ->
            try baseline_ets:insert_new(Pid, {?API_CLUSTERMGR, self(), undefined}) of
                true ->
                    found(#state{ets = Pid})
            catch
                error:Reason ->
                    {stop, Reason, undefined}
            end
    end.

found(State) ->
    {noreply, State}.


received(#signal{gsn=?GSN_API_REGCONF, sections_length=0}, <<>>, State) ->
    %%
    %% ~/include/kernel/signaldata/ApiRegSignalData.hpp: ApiRegConf
    %% ~/src/ndbapi/ClusterMgr.cpp: ClusterMgr::execAPI_REGCONF/2
    %%
    {noreply, State};
received(#signal{gsn=?GSN_API_REGREF, sections_length=0}=S, <<>>, State) ->
    %%
    %% ~/include/kernel/signaldata/ApiRegSignalData.hpp: ApiRegRef
    %% ~/src/ndbapi/ClusterMgr.cpp: ClusterMgr::execAPI_REGREF/1
    %%
    Reason = case lists:nth(3, S#signal.signal_data) of % errorCode
                 1 -> <<"WrongType">>;
                 2 -> <<"UnsupportedVersion">>
             end,
    {stop, {shutdown, Reason}, State};
received(Signal, Binary, State) ->
    ok = error_logger:warning_msg("[~p:~p] ~p,~p~n", [?MODULE, self(), Signal, Binary]),
    {noreply, State}.
