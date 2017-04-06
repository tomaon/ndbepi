-module(ndbepi_connection).

-include("internal.hrl").

%% -- private --
-export([start_link/1]).

-behaviour(gen_server).
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2]).

%% -- internal --
-record(state, {
          block_no  :: pos_integer(),
          block_mgr :: undefined|pid(),
          tccon     :: undefined|integer() % Transaction Co-ordinator CONnection pointer
         }).

%% == public ==

-spec start_link(pos_integer()) -> {ok, pid()}|{error, _}.
start_link(BlockNo) ->
    gen_server:start_link(?MODULE, [BlockNo], []).

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
handle_info({'EXIT', _Pid, Reason}, State) ->
    {stop, Reason, State}.

%% == internal ==

cleanup(#state{block_no=N, block_mgr=M}=X)
  when M =/= undefined ->
    catch true = baseline_ets:delete(M, N),
    cleanup(X#state{block_mgr = undefined});
cleanup(_) ->
    baseline:flush().

setup([BlockNo]) ->
    false = process_flag(trap_exit, true),
    loaded(#state{block_no = BlockNo}).


loaded(#state{block_no=N}=X) ->
    case baseline_app:find(ndbepi_sup, ndbepi_block_mgr, 100, 1) of
        undefined ->
            {stop, not_found};
        Pid ->
            try baseline_ets:insert_new(Pid, {N, self()}) of
                true ->
                    initialized(X#state{block_mgr = Pid});
                false ->
                    {stop, ebusy} % -> retry
            catch
                error:Reason ->
                    {stop, Reason}
            end
    end.

initialized(#state{block_mgr=M}=X)
  when M =/= undefined ->
    {ok, X};
initialized(State) -> % TODO
    case baseline_app:find(ndbepi_sup, ndbepi_transporters, 100, 1) of
        undefined ->
            {stop, not_found};
        Pid ->
            initialized(hd(baseline_app:children(Pid)), State)
    end.

initialized(Pid, #state{block_no=N}=X) ->
    case start_transaction(Pid, ndbepi_transporter:default(Pid, 3000), N) of
        {ok, TCCon} ->
            {ok, X#state{tccon = TCCon}};
        {error, Reason} ->
            {stop, Reason}
    end.


received(Signal, State) ->
    ok = error_logger:warning_msg("[~p:~p] s=~p~n", [?MODULE, self(), Signal]),
    {noreply, State}.



start_transaction(Pid, #signal{send_node_id=S}=D, BlockNo) ->
    %%
    %% ~/src/ndbapi/Ndb.cpp: Ndb::NDB_connect/2
    %% ~/src/ndbapi/NdbTransaction.cpp: NdbTransaction::receiveTCSEIZECONF/1
    %% ~/src/ndbapi/NdbTransaction.cpp: NdbTransaction::receiveTCSEIZEREF/1
    %%
    case ndbepi_transporter:call(Pid, D#signal{
                                        gsn = ?GSN_TCSEIZEREQ,
                                        send_block_no = BlockNo,
                                        recv_block_no = ?DBTC,
                                        signal_data_length = 3,
                                        signal_data = [
                                                       0,
                                                       ?NUMBER_TO_REF(BlockNo, S),
                                                       0
                                                      ]
                                       }, [], 3000) of
        {ok, #signal{gsn=?GSN_TCSEIZECONF, signal_data=L}} ->
            {ok, lists:nth(2, L)};
        {ok, #signal{gsn=?GSN_TCSEIZEREF, signal_data=L}} ->
            {error, {shutdown, ndberror(lists:nth(2, L))}};
        {error, Reason} ->
            {error, Reason}
    end.


%%
%% ~/include/ndbapi/ndberror.hpp : ndberror_classification_enum
%%
ndberror( 0) -> <<"none">>;
ndberror( 1) -> <<"application">>;
ndberror( 2) -> <<"no_data_found">>;
ndberror( 3) -> <<"constraint_violation">>;
ndberror( 4) -> <<"schema_error">>;
ndberror( 5) -> <<"user_defined">>;
ndberror( 6) -> <<"insufficient_space">>;
ndberror( 7) -> <<"temporary_resource">>;
ndberror( 8) -> <<"node_recovery">>;
ndberror( 9) -> <<"overload">>;
ndberror(10) -> <<"timeout_expired">>;
ndberror(11) -> <<"unknown_result">>;
ndberror(12) -> <<"internal_error">>;
ndberror(13) -> <<"function_not_implemented">>;
ndberror(14) -> <<"unknown_error_code">>;
ndberror(15) -> <<"node_shutdown">>;
ndberror(16) -> <<"configuration">>;
ndberror(17) -> <<"schema_object_already_exists">>;
ndberror(18) -> <<"internal_temporary">>.
