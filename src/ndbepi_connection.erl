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
          fragments  :: map(),
          ets        :: undefined|pid(),
          tab        :: undefined|ets:tab(),
          from       :: undefined|{pid(), reference()}
         }).

%% == private ==

-spec start_link(node_id(), pos_integer()) -> {ok, pid()}|{error, _}.
start_link(NodeId, BlockNo) ->
    gen_server:start_link(?MODULE, [NodeId, BlockNo], []).


call(Pid) ->
    {ok, L1} = gen_server:call(Pid, {get_table_by_name, <<"test/def/city">>, undefined}),
    [io:format("~p~n~n", [E]) || E <- L1 ],
    {ok, L2} = gen_server:call(Pid, {get_table_by_id, 1, undefined}),
    [io:format("~p~n~n", [E]) || E <- L2 ].

%% ndbepi_connection:call(element(2, ndbepi:connect())).

%% -- behaviour: gen_server --

init(Args) ->
    setup(Args).

terminate(_Reason, State) ->
    cleanup(State).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call(Request, From, #state{request_id=R, from=undefined}=X) ->
    ready(Request, X#state{request_id = R + 1, from = From}).

handle_cast(_Request, State) ->
    {stop, enosys, State}.

handle_info({#signal{fragment_info=0}=S, Binary}, State) ->
    received(S, Binary, State);
handle_info({#signal{fragment_info=3}=S, Binary}, State) ->
    {L, M} = maps:take(key(S), State#state.fragments),
    received(S, list_to_binary(lists:reverse([value(Binary)|L])), State#state{fragments = M});
handle_info({#signal{fragment_info=2}=S, Binary}, State) ->
    M = maps:update_with(key(S), fun(V) -> [value(Binary)|V] end, State#state.fragments),
    {noreply, State#state{fragments = M}};
handle_info({#signal{fragment_info=1}=S, Binary}, State) ->
    M = maps:put(key(S), [value(Binary)], State#state.fragments),
    {noreply, State#state{fragments = M}};
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
    loaded(#state{node_id = NodeId, block_no = BlockNo, request_id = 0, fragments = maps:new()}).


loaded(#state{}=X) ->
    case baseline_app:find(ndbepi_sup, ndbepi_ets, 100, 1) of
        undefined ->
            {stop, not_found};
        Pid ->
            found(X#state{ets = Pid})
    end.

found(#state{block_no=B, ets=E}=X) ->
    case baseline_ets:insert_new(E, {B, self()}) of
        true ->
            configured(X#state{tab = baseline_ets:tab(E)});
        false ->
            {stop, ebusy} % -> retry
    end.

configured(State) ->
    {ok, State}.


ready({_, _, Z}=A, #state{node_id=N, block_no=B, request_id=R, tab=T}=X) ->
    try select(T, Z, B) of
        {_, P, D} ->
            ok = apply(ndbepi_transporter, cast, [P|signal(A, N, B, R, D)]),
            {noreply, X}
    catch
        error:Reason ->
            {stop, Reason, X}
    end.


select(Tab, undefined, Seed) ->
    case ets:select(Tab, [{{'$1', '_', '_'}, [], ['$_']}]) of
        [] ->
            error(badarg);
        List ->
            lists:nth(1 + Seed rem length(List), List)
    end;
select(Tab, NodeId, BlockNo) ->
    case ets:select(Tab, [{{'$1', '_', '_'}, [{'=', '$1', NodeId}], ['$_']}]) of
        [] ->
            select(Tab, undefined, BlockNo);
        List ->
            hd(List)
    end.

signal({get_table_by_id, Id, _}, NodeId, BlockNo, RequestId, Default) ->
    %%
    %% ~/include/kernel/signaldata/GetTabInfo.hpp: GetTabInfoReq
    %%
    [
     Default#signal{gsn = ?GSN_GET_TABINFOREQ,
                    send_block_no = BlockNo,
                    recv_block_no = ?DBDICT,
                    signal_data_length = 5,
                    signal_data = [
                                   RequestId,
                                   ?NUMBER_TO_REF(BlockNo, NodeId),
                                   2, % 0(=RequestById) + 2(=LongSignalConf)
                                   Id,
                                   0  % TODO
                                  ],
                    sections_length = 0
                   },
     []
    ];
signal({get_table_by_name, Name, _}, NodeId, BlockNo, RequestId, Default) ->
    %%
    %% ~/include/kernel/signaldata/GetTabInfo.hpp: GetTabInfoReq
    %%
    [
     Default#signal{gsn = ?GSN_GET_TABINFOREQ,
                    send_block_no = BlockNo,
                    recv_block_no = ?DBDICT,
                    signal_data_length = 5,
                    signal_data = [
                                   RequestId,
                                   ?NUMBER_TO_REF(BlockNo, NodeId),
                                   3, % 1(=RequestByName) + 2(=LongSignalConf)
                                   size(Name) + 1, % NULL terminated
                                   0
                                  ],
                    sections_length = 1
                   },
     [Name]
    ].

received(#signal{gsn=?GSN_GET_TABINFO_CONF, signal_data=D}, Binary, State) ->
    %%
    %% ~/include/kernel/signaldata/GetTabInfo.hpp: GetTabInfoConf
    %% ~/src/ndbapi/NdbDictionalyImpl.cpp: NdbDictInterface::execGET_TABINFO_CONF/2
    %%
    case lists:nth(1, D) =:= State#state.request_id of
        true ->
            {noreply, reply({ok, unpack(Binary, 0, size(Binary), [])}, State)};
        false ->
            {noreply, State}
    end;
received(#signal{gsn=?GSN_GET_TABINFOREF, signal_data=D}, <<>>, State) ->
    %%
    %% ~/include/kernel/signaldata/GetTabInfo.hpp: GetTabInfoRef
    %% ~/src/ndbapi/NdbDictionalyImpl.cpp: NdbDictInterface::execGET_TABINFO_REF/2
    %%
    case lists:nth(1, D) =:= State#state.request_id of
        true ->
            Reason = case lists:nth(6, D) of
                         701 -> <<"Busy">>;
                         702 -> <<"TableNameTooLong">>;
                         709 -> <<"InvalidTableId">>;
                         710 -> <<"NoFetchByName">>;
                         723 -> <<"TableNotDefined">>
                     end,
            {noreply, reply({error, Reason}, State)};
        false ->
            {noreply, State}
    end;
received(Signal, Binary, State) ->
    ok = error_logger:warning_msg("[~p:~p] ~p,~p~n", [?MODULE, self(), Signal, Binary]),
    {noreply, State}.


reply(Reply, #state{from=F}=X) ->
    _ = gen_server:reply(F, Reply),
    X#state{from = undefined}.

key(#signal{signal_data_length=L, signal_data=D}) ->
    lists:nth(L, D).

value(Binary) ->
    {_, B} = split_binary(Binary, ?WORD(1)),  % element(1, _) = size(B)
    B.


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
