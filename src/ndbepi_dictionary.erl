-module(ndbepi_dictionary).

-include("internal.hrl").

-import(ndbepi_util, [bin_to_word/4]).

%% -- private --
-export([start_link/2]).
-export([call/0]).

-behaviour(ndbepi_gen_block1).
-export([init/0, terminate/2, code_change/3,
         handle_call/5, handle_info/3]).

%% -- internal --
-record(data, {
          request_id :: non_neg_integer()
         }).

%% == private ==

-spec start_link(node_id(), pos_integer()) -> {ok, pid()}|{error, _}.
start_link(NodeId, BlockNo) ->
    ndbepi_gen_block1:start_link(?MODULE, NodeId, BlockNo, []).


call() ->
    P = baseline_app:find(ndbepi_sup, ndbepi_dictionary, 100, 1),
    {ok, L1} = ndbepi_gen_block1:call(P, {get_table_by_name, [<<"test/def/city">>], undefined}),
    [io:format("~p~n", [E]) || E <- L1 ], io:format("~n~n"),
    {ok, L2} = ndbepi_gen_block1:call(P, {get_table_by_id, [1], undefined}),
    io:format("~p~n", [length(L2)]),
    ok.

%% -- behaviour: ndbepi_gen_block1 --

init() ->
    {ok, #data{request_id = 0}}.

terminate(_Reason, _Data) ->
    ok.

code_change(_OldVsn, Data, _Extra) ->
    {ok, Data}.

handle_call(Request, NodeId, BlockNo, Signal, Data) ->
    R = Data#data.request_id + 1, % TODO
    {noreply, signal(Request, NodeId, BlockNo, R, Signal), Data#data{request_id = R}}.

handle_info(#signal{gsn=?GSN_GET_TABINFO_CONF}=S, Binary, Data) ->
    %%
    %% ~/include/kernel/signaldata/GetTabInfo.hpp: GetTabInfoConf
    %% ~/src/ndbapi/NdbDictionalyImpl.cpp: NdbDictInterface::execGET_TABINFO_CONF/2
    %%
    case lists:nth(1, S#signal.signal_data) =:= Data#data.request_id of
        true ->
            {reply, {ok, unpack(Binary)}, Data};
        false ->
            {noreply, Data}
    end;
handle_info(#signal{gsn=?GSN_GET_TABINFOREF}=S, <<>>, Data) ->
    %%
    %% ~/include/kernel/signaldata/GetTabInfo.hpp: GetTabInfoRef
    %% ~/src/ndbapi/NdbDictionalyImpl.cpp: NdbDictInterface::execGET_TABINFO_REF/2
    %%
    case lists:nth(1, S#signal.signal_data) =:= Data#data.request_id of
        true ->
            Reason = case lists:nth(6, S#signal.signal_data) of
                         701 -> <<"Busy">>;
                         702 -> <<"TableNameTooLong">>;
                         709 -> <<"InvalidTableId">>;
                         710 -> <<"NoFetchByName">>;
                         723 -> <<"TableNotDefined">>
                     end,
            {noreply, {error, Reason}, Data};
        false ->
            {noreply, Data}
    end;
handle_info(Signal, Binary, Data) ->
    ok = error_logger:warning_msg("[~p:~p] ~p,~p~n", [?MODULE, self(), Signal, Binary]),
    {noreply, Data}.

%% == internal ==

signal({get_table_by_id, [Id]}, NodeId, BlockNo, RequestId, Default) ->
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
                    sections_length = 0},
     []
    ];
signal({get_table_by_name, [Name]}, NodeId, BlockNo, RequestId, Default) ->
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
                    sections_length = 1},
     [Name]
    ].


unpack(Binary) ->
    unpack(Binary, 0, ?WORD(size(Binary)), []).

unpack(_Binary, _Start, 0, List) ->
    lists:reverse(List);
unpack(Binary, Start, Length, List) ->
    %%
    %% ~/include/util/SimpleProperties.hpp: SimpleProperties::ValueType
    %% ~/src/common/util/SimpleProperties.cpp: SimpleProperties::Reader::readValue/0
    %%
    W = bin_to_word(Binary, Start, 1, big),
    {T, N} = case {W band 16#ffff, W bsr 16} of
                 {K, 0} -> % Uint32Value
                     V = bin_to_word(Binary, Start + 1, 1, big),
                     {{K, V}, 2};
                 {K, 1} -> % StringValue
                     L = bin_to_word(Binary, Start + 1, 1, big),
                     {{K, binary_part(Binary, ?BYTE(Start + 2), L - 1)}, 2 + ?WORD(L + 3)};
                 {K, 2} -> % BinaryValue
                     L = bin_to_word(Binary, Start + 1, 1, big),
                     {{K, binary_part(Binary, ?BYTE(Start + 2), L)}, 2 + ?WORD(L + 3)}
             end,
    unpack(Binary, Start + N, Length - N, [T|List]).
