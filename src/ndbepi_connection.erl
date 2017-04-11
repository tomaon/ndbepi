-module(ndbepi_connection).

-include("internal.hrl").

%% -- private --
-export([start_link/2]).
-export([call/1]).

-behaviour(gen_server).
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2]).

%% -- internal --
-record(state, {
          node_id    :: node_id(),
          block_no   :: pos_integer(),
          request_id :: non_neg_integer(),
          ets        :: undefined|pid(),
          tab        :: undefined|ets:tab(),
          from       :: undefined|{pid(), reference()},
          fragments  :: undefined|map()
         }).

%% == public ==

-spec start_link(node_id(), pos_integer()) -> {ok, pid()}|{error, _}.
start_link(NodeId, BlockNo) ->
    gen_server:start_link(?MODULE, [NodeId, BlockNo], []).


call(Pid) ->
    gen_server:call(Pid, {get_table_by_name, <<"test/def/city">>}).

%% -- behaviour: gen_server --

init(Args) ->
    setup(Args).

terminate(_Reason, State) ->
    cleanup(State).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call(Request, From, #state{block_no=B, tab=T, request_id=R}=X) ->
    try ets:select(T, [{{'$1', '_', '_'}, [{'<', '$1', ?MAX_NODES_ID}], ['$_']}]) of
        [] ->
            {stop, not_found, X};
        List ->
            ready(Request, lists:nth(1 + B rem length(List), List),
                  X#state{request_id = R + 1, from = From})
    catch
        error:Reason ->
            {error, Reason}
    end.

handle_cast(_Request, State) ->
    {stop, enosys, State}.

handle_info({#signal{fragment_info=0}=S, Binary}, State) ->
    received(S, Binary, State);
handle_info({#signal{fragment_info=1}=S, Binary}, #state{fragments=F}=X) ->
    {noreply, X#state{fragments = append_fragments(fragment_id(S), Binary, F)}};
handle_info({#signal{fragment_info=3}=S, Binary}, #state{fragments=F}=X) ->
    {L, M} = remove_fragments(fragment_id(S), Binary, F),
    received(S, binary:list_to_bin(L), X#state{fragments = M});
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
    initialized(#state{node_id = NodeId, block_no = BlockNo, request_id = 0}).


initialized(#state{block_no=B}=X) ->
    case baseline_app:find(ndbepi_sup, ndbepi_ets, 100, 1) of
        undefined ->
            {stop, not_found};
        Pid ->
            try baseline_ets:insert_new(Pid, {B, self(), undefined}) of
                true ->
                    found(X#state{ets = Pid});
                false ->
                    {stop, ebusy} % -> retry
            catch
                error:Reason ->
                    {stop, Reason}
            end
    end.

found(#state{ets=E}=X) ->
    try baseline_ets:tab(E) of
        Tab ->
            configured(X#state{tab = Tab})
    catch
        error:Reason ->
            {stop, Reason, X}
    end.

configured(State) ->
    {ok, State}.


ready({get_table_by_name, Name}, V, #state{}=X) ->
    %%
    %% ~/include/kernel/signaldata/GetTabInfo.hpp
    %%
    N = X#state.node_id,
    B = X#state.block_no,
    R = X#state.request_id,
    {_, P, D} = V,
    ok = ndbepi_transporter:cast(P, D#signal{gsn = ?GSN_GET_TABINFOREQ,
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
                                            }, [ Name ]),
    {noreply, X}.

%% ndbepi_connection:call(element(2, ndbepi:connect())).

received(#signal{gsn=?GSN_GET_TABINFO_CONF}, Binary, #state{}=X) ->
    L = unpack(Binary, 0, size(Binary), []),
    _ = gen_server:reply(X#state.from, {ok, L}),
    {noreply, X#state{from = undefined}};
received(#signal{gsn=?GSN_GET_TABINFOREF}=S, <<>>, #state{}=X) ->
    %%
    %% ~/include/kernel/signaldata/GetTabInfo.hpp: GetTabInfoRef
    %% ~/src/ndbapi/NdbDictionalyImpl.cpp: NdbDictInterface::execGET_TABINFO_REF/2
    %%
    Reason = case lists:nth(6, S#signal.signal_data) of
                 701 -> <<"Busy">>;
                 702 -> <<"TableNameTooLong">>;
                 709 -> <<"InvalidTableId">>;
                 710 -> <<"NoFetchByName">>;
                 723 -> <<"TableNotDefined">>
             end,
    _ = gen_server:reply(X#state.from, {error, Reason}),
    {noreply, X#state{from = undefined}};
received(Signal, Binary, State) ->
    ok = error_logger:warning_msg("[~p:~p] ~p,~p~n", [?MODULE, self(), Signal, Binary]),
    {noreply, State}.


append_fragments(Key, Binary, undefined) ->
    append_fragments(Key, Binary, maps:new());
append_fragments(Key, Binary, Map) ->
    List = maps:get(Key, Map, []),
    maps:put(Key, [fragment(Binary)|List], Map).

fragment(Binary) ->
    {_, B} = split_binary(Binary, ?WORD(1)),  % element(1, _) = size(B)
    B.

fragment_id(#signal{signal_data_length=L, signal_data=D}) ->
    lists:nth(L, D).

remove_fragments(Key, Binary, Map) ->
    List = maps:get(Key, Map, []),
    {lists:reverse([fragment(Binary)|List]), maps:remove(Key, Map)}.

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
