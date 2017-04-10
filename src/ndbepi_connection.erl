-module(ndbepi_connection).

-include("internal.hrl").

%% -- private --
-export([start_link/1]).

-behaviour(gen_server).
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2]).

%% -- internal --
-record(state, {
          id  :: pos_integer(),
          ets :: undefined|pid()
         }).

%% == public ==

-spec start_link(pos_integer()) -> {ok, pid()}|{error, _}.
start_link(Id) ->
    gen_server:start_link(?MODULE, [Id], []).

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

cleanup(#state{id=I, ets=E}=X)
  when E =/= undefined ->
    catch true = baseline_ets:delete(E, I),
    cleanup(X#state{ets = undefined});
cleanup(_) ->
    baseline:flush().

setup([Id]) ->
    false = process_flag(trap_exit, true),
    loaded(#state{id = Id}).


loaded(#state{id=I}=X) ->
    case baseline_app:find(ndbepi_sup, ndbepi_block_mgr, 100, 1) of
        undefined ->
            {stop, not_found};
        Pid ->
            try baseline_ets:insert_new(Pid, {I, self(), undefined}) of
                true ->
                    initialized(X#state{ets = Pid});
                false ->
                    {stop, ebusy} % -> retry
            catch
                error:Reason ->
                    {stop, Reason}
            end
    end.

initialized(State) ->
    case baseline_app:find(ndbepi_sup, ndbepi_transporters, 100, 1) of
        undefined ->
            {stop, not_found};
        Pid ->
            initialized(hd(baseline_app:children(Pid)), State)
    end.

initialized(Pid, #state{id=I}=X) ->
    case get_table_by_name(Pid, <<"test/def/city">>, ndbepi_transporter:default(Pid, 3000), I) of
        {ok, Signal} ->
            error_logger:info_msg("~p~n", [Signal]),
            {ok, X};
        {error, Reason} ->
            {stop, Reason}
    end.


received(Signal, State) ->
    ok = error_logger:warning_msg("[~p:~p] s=~p~n", [?MODULE, self(), Signal]),
    {noreply, State}.



get_table_by_name(Pid, Name, #signal{send_node_id=S}=D, BlockNo) ->
    %%
    %% ~/include/kernel/signaldata/GetTabInfo.hpp
    %%
    case ndbepi_transporter:call(Pid, D#signal{gsn = ?GSN_GET_TABINFOREQ,
                                               send_block_no = BlockNo,
                                               recv_block_no = ?DBDICT,
                                               signal_data_length = 5,
                                               signal_data = [
                                                              0,                          % senderData
                                                              ?NUMBER_TO_REF(BlockNo, S), % senderRef
                                                              3,                          % requestType
                                                              byte_size(Name) + 1,        % tableNameLen
                                                              0                           % schemaTransId
                                                        ],
                                               sections_length = 1
                                              }, [ Name ], 3000) of
        {ok, #signal{gsn=?GSN_GET_TABINFO_CONF}=X} ->
            {ok, X};
        {ok, #signal{gsn=?GSN_GET_TABINFOREF, signal_data=L}} ->
            {error, {shutdown, ndberror(lists:nth(6, L))}};
        {error, Reason} ->
            {error, Reason}
    end.

%% start_transaction(Pid, #signal{send_node_id=S}=D, BlockNo) ->
%%     %%
%%     %% ~/src/ndbapi/Ndb.cpp: Ndb::NDB_connect/2
%%     %% ~/src/ndbapi/NdbTransaction.cpp: NdbTransaction::receiveTCSEIZECONF/1
%%     %% ~/src/ndbapi/NdbTransaction.cpp: NdbTransaction::receiveTCSEIZEREF/1
%%     %%
%%     case ndbepi_transporter:call(Pid, D#signal{
%%                                         gsn = ?GSN_TCSEIZEREQ,
%%                                         send_block_no = BlockNo,
%%                                         recv_block_no = ?DBTC,
%%                                         signal_data_length = 3,
%%                                         signal_data = [
%%                                                        0,
%%                                                        ?NUMBER_TO_REF(BlockNo, S),
%%                                                        0
%%                                                       ]
%%                                        }, [], 3000) of
%%         {ok, #signal{gsn=?GSN_TCSEIZECONF, signal_data=L}} ->
%%             {ok, lists:nth(2, L)};
%%         {ok, #signal{gsn=?GSN_TCSEIZEREF, signal_data=L}} ->
%%             {error, {shutdown, ndberror(lists:nth(2, L))}};
%%         {error, Reason} ->
%%             {error, Reason}
%%     end.

%%
%% ~/include/ndbapi/ndberror.hpp : ndberror_classification_enum
%%
%% ndberror( 0) -> <<"none">>;
%% ndberror( 1) -> <<"application">>;
%% ndberror( 2) -> <<"no_data_found">>;
%% ndberror( 3) -> <<"constraint_violation">>;
%% ndberror( 4) -> <<"schema_error">>;
%% ndberror( 5) -> <<"user_defined">>;
%% ndberror( 6) -> <<"insufficient_space">>;
%% ndberror( 7) -> <<"temporary_resource">>;
%% ndberror( 8) -> <<"node_recovery">>;
%% ndberror( 9) -> <<"overload">>;
%% ndberror(10) -> <<"timeout_expired">>;
%% ndberror(11) -> <<"unknown_result">>;
%% ndberror(12) -> <<"internal_error">>;
%% ndberror(13) -> <<"function_not_implemented">>;
%% ndberror(14) -> <<"unknown_error_code">>;
%% ndberror(15) -> <<"node_shutdown">>;
%% ndberror(16) -> <<"configuration">>;
%% ndberror(17) -> <<"schema_object_already_exists">>;
%% ndberror(18) -> <<"internal_temporary">>.

ndberror(701) -> <<"Busy = 701">>;
ndberror(702) -> <<"TableNameTooLong">>;
ndberror(709) -> <<"InvalidTableId">>;
ndberror(710) -> <<"NoFetchByName">>;
ndberror(723) -> <<"TableNotDefined">>;
ndberror(_)   -> <<"?">>.
