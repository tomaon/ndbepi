-module(ndbepi_connection).

-include("internal.hrl").

%% -- private --
-export([start_link/2]).

-behaviour(gen_server).
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2]).

%% -- internal --
-record(state, {
          node_id    :: node_id(),
          block_no   :: pos_integer(),
          request_id :: non_neg_integer(),
          fragments  :: map(),
          ets        :: undefined|pid(),
          default    :: undefined|{node_id(), pid(), signal()}
         }).

%% == public ==

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

handle_info(#signal{fragment_info=0}=S, State) ->
    received(S, State);
handle_info(#signal{fragment_info=1, sections=B}=S, #state{fragments=F}=X) ->
    {noreply, X#state{fragments = append_fragments(fragment_id(S), B, F)}};
handle_info(#signal{fragment_info=3, sections=B}=S, #state{fragments=F}=X) ->
    {L, M} = remove_fragments(fragment_id(S), B, F),
    received(S#signal{sections = L}, X#state{fragments = M});
handle_info({'EXIT', _Pid, Reason}, State) ->
    {stop, Reason, State}.

%% == internal ==

cleanup(#state{ets=E}=X)
  when E =/= undefined ->
    catch true = baseline_ets:delete(E, X#state.block_no),
    cleanup(X#state{ets = undefined});
cleanup(_) ->
    baseline:flush().

setup([NodeId, BlockNo]) ->
    false = process_flag(trap_exit, true),
    loaded(#state{node_id = NodeId, block_no = BlockNo,
                  request_id = 0, fragments = maps:new()}).


loaded(#state{block_no=B}=X) ->
    case baseline_app:find(ndbepi_sup, ndbepi_ets, 100, 1) of
        undefined ->
            {stop, not_found};
        Pid ->
            try baseline_ets:insert_new(Pid, {B, self(), undefined}) of
                true ->
                    initialized(X#state{ets = Pid});
                false ->
                    {stop, ebusy} % -> retry
            catch
                error:Reason ->
                    {stop, Reason}
            end
    end.

initialized(#state{block_no=B, ets=E}=X) ->
    try baseline_ets:select(E, [{{'$1', '_', '_'}, [{'<', '$1', ?MAX_NODES_ID}], ['$_']}]) of
        [] ->
            {stop, not_found};
        List ->
            get_table_by_name(X#state{default = lists:nth(1 + B rem length(List), List)})
    catch
        error:Reason ->
            {stop, Reason}
    end.


received(Signal, State) ->
    ok = error_logger:warning_msg("[~p:~p] s=~p~n", [?MODULE, self(), Signal]),
    {noreply, State}.


fragment(Binary) ->
    {_, B} = split_binary(Binary, ?WORD(1)), % 1=size(B)
    B.

fragment_id(#signal{signal_data_length=L, signal_data=D}) ->
    lists:nth(L, D).

append_fragments(Key, Binary, Map) ->
    List = maps:get(Key, Map, []),
    maps:put(Key, [fragment(Binary)|List], Map).

remove_fragments(Key, Binary, Map) ->
    List = maps:get(Key, Map, []),
    {list_to_binary(lists:reverse([fragment(Binary)|List])), maps:remove(Key, Map)}.


get_table_by_name(#state{default=D, request_id=R}=X) ->
    {_, P, S} = D,
    case get_table_by_name(P, <<"test/def/city">>, S, X#state{request_id = R + 1}) of
        ok ->
            {ok, X}
    end.

get_table_by_name(Pid, Name, #signal{}=D, #state{node_id=N, block_no=B, request_id=R}) ->
    %%
    %% ~/include/kernel/signaldata/GetTabInfo.hpp
    %%
    ndbepi_transporter:cast(Pid, D#signal{gsn = ?GSN_GET_TABINFOREQ,
                                          send_block_no = B,
                                          recv_block_no = ?DBDICT,
                                          signal_data_length = 5,
                                          signal_data = [
                                                         R,                    % senderData
                                                         ?NUMBER_TO_REF(B, N), % senderRef
                                                         3,                    % requestType
                                                         byte_size(Name) + 1,  % tableNameLen
                                                         0                     % schemaTransId
                                                        ],
                                          sections_length = 1
                                         }, [ Name ]).

%% {ok, #signal{gsn=?GSN_GET_TABINFO_CONF}=X} ->
%%     {ok, X};
%% {ok, #signal{gsn=?GSN_GET_TABINFOREF, signal_data=L}} ->
%%     {error, {shutdown, ndberror(lists:nth(6, L))}};

 %%                #signal{gsn=?GSN_GET_TABINFOCONF,
 %%                        fragment_info=1,signal_data_length=D,no_of_sections=1,binary=B} ->

 %%                    %% 0 < F -> D = 8 = 6 + 1(UNKNOWN) + 1(FragmentId), TODO

 %%                    FragmentId = binary_part(B, ?WORD(D-1), ?WORD(1)),
 %%                    Fragment = binary_part(B, ?WORD(D+1), byte_size(B)-?WORD(D+1)),

 %%                    X = fragments(1, FragmentId, [Fragment]),
 %%                    L = unpack(X, 0, byte_size(X), []),

 %%                    io:format("GSN_GET_TABINFOCONF:1 ~p~n", [L]);

 %%                #signal{gsn=?GSN_GET_TABINFOCONF,
 %%                        fragment_info=0,signal_data_length=D,no_of_sections=1,binary=B} ->
 %%                    %% C0 = binary_to_word(B, ?WORD(0), E), % senderData
 %%                    %% C1 = binary_to_word(B, ?WORD(1), E), % tableId
 %%                    %% C2 = binary_to_word(B, ?WORD(2), E), % gci       (|freeWordsHi)
 %%                    %% C3 = binary_to_word(B, ?WORD(3), E), % totalLen  (|freeExtents|freeWordsLo)
 %%                    %% C4 = binary_to_word(B, ?WORD(4), E), % tableType
 %%                    %% C5 = binary_to_word(B, ?WORD(5), E), % senderRef
 %%                    X = binary_part(B, ?WORD(D+1), byte_size(B)-?WORD(D+1)),
 %%                    io:format("GSN_GET_TABINFOCONF:0 ~p~n", [unpack(X,0,byte_size(X),[])]);


%% fragments(3, _FragmentId, List) ->
%%     list_to_binary(lists:reverse(List));
%% fragments(1, FragmentId, List) ->
%%     receive
%%         #signal{gsn=?GSN_GET_TABINFO_CONF,
%%                 fragment_info=F,signal_data_length=D,no_of_sections=1,binary=B}
%%           when FragmentId =:= binary_part(B, ?WORD(D-1), ?WORD(1)) ->

%%             Fragment = binary_part(B, ?WORD(D+1), byte_size(B)-?WORD(D+1)),

%%             fragments(F, FragmentId, [Fragment|List])
%%     end.

unpack(_Binary, _Start, 0, List) ->
    lists:reverse(List);
unpack(Binary, Start, Length, List) ->
    %%
    %% ~/include/util/SimpleProperties.hpp: SimpleProperties::ValueType
    %% ~/src/common/util/SimpleProperties.cpp: SimpleProperties::Reader::readValue/0
    %%
    <<W:?WORD(1)/big-unit:8>> = binary_part(Binary, Start, ?WORD(1)), % << ntohl = big
    {T, N} = case {W band 16#ffff, W bsr 16} of
                 {K, 0} -> % Uint32Value
                     <<V:?WORD(1)/big-unit:8>> = binary_part(Binary, Start + ?WORD(1), ?WORD(1)),
                     {{K, V}, ?WORD(2)};
                 {K, 1} -> % StringValue
                     <<L:?WORD(1)/big-unit:8>> = binary_part(Binary, Start + ?WORD(1), ?WORD(1)),
                     {{K, binary_part(Binary, Start + ?WORD(2), L - 1)}, ?WORD(2 + ((L + 3) bsr 2))};
                 {K, 2} -> % BinaryValue
                     <<L:?WORD(1)/big-unit:8>> = binary_part(Binary, Start + ?WORD(1), ?WORD(1)),
                     {{K, binary_part(Binary, Start + ?WORD(2), L)}, ?WORD(2 + ((L + 3) bsr 2))}
             end,
    unpack(Binary, Start + N, Length - N, [T|List]).



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

%% ndberror(701) -> <<"Busy = 701">>;
%% ndberror(702) -> <<"TableNameTooLong">>;
%% ndberror(709) -> <<"InvalidTableId">>;
%% ndberror(710) -> <<"NoFetchByName">>;
%% ndberror(723) -> <<"TableNotDefined">>;
%% ndberror(_)   -> <<"?">>.
