-module(ndbepi_connection).

-include("internal.hrl").

-import(ndbepi_util, [bin_to_word/4]).

%% -- private --
-export([start_link/2]).
-export([call/1]).

-behaviour(ndbepi_gen_block2).
-export([init/0, terminate/2, code_change/3,
         handle_call/6, handle_info/3]).

%% -- internal --
-record(data, {
          request_id :: non_neg_integer(),
          from       :: undefined|{pid(), term()}
         }).

%% == private ==

-spec start_link(node_id(), pos_integer()) -> {ok, pid()}|{error, _}.
start_link(NodeId, BlockNo) ->
    ndbepi_gen_block2:start_link(?MODULE, NodeId, BlockNo, []).


call(Pid) ->
    {ok, L1} = ndbepi_gen_block2:call(Pid, {get_table_by_name, [<<"test/def/city">>], undefined}),
    [io:format("~p~n", [E]) || E <- L1 ], io:format("~n~n"),
    {ok, L2} = ndbepi_gen_block2:call(Pid, {get_table_by_id, [1], undefined}),
    io:format("~p~n", [length(L2)]),
    {ok, I3} = ndbepi_gen_block2:call(Pid, {startTransaction, [0], undefined}),
    io:format("~p~n~n", [I3]),
    ok = ndbepi_gen_block2:call(Pid, {closeTransaction, [I3], undefined}).

%% ndbepi_connection:call(element(2, ndbepi:connect())).

%% -- behaviour: ndbepi_gen_block2 --

init() ->
    setup().

terminate(_Reason, Data) ->
    cleanup(Data).

code_change(_OldVsn, Data, _Extra) ->
    {ok, Data}.

handle_call(Request, NodeId, BlockNo, Signal, From, #data{request_id=R, from=undefined}=D) ->
    ready(Request, NodeId, BlockNo, Signal, D#data{request_id = R + 1, from = From}).

handle_info(Signal, Binary, Data) ->
    received(Signal, Binary, Data).

%% == internal ==

cleanup(_) ->
    ok.

setup() ->
    {ok, #data{request_id = 0}}.


ready(Request, NodeId, BlockNo, Default, #data{request_id = R}=D) ->
    {noreply, signal(Request, NodeId, BlockNo, R, Default), D}.


signal({startTransaction, [Instance]}, NodeId, BlockNo, _RequestId, Default) ->
    [
     Default#signal{gsn = ?GSN_TCSEIZEREQ,
                    send_block_no = BlockNo,
                    recv_block_no = ?DBTC,
                    signal_data_length = 3,
                    signal_data = [
                                   999,
                                   ?NUMBER_TO_REF(BlockNo, NodeId),
                                   Instance
                                  ],
                    sections_length = 0},
     []
    ];
signal({closeTransaction, [Id]}, NodeId, BlockNo, _RequestId, Default) ->
    %%
    %% ~/src/ndbapi/NdbTransaction.cpp: NdbTransaction::execute/3
    %%
    [
     Default#signal{gsn = ?GSN_TCRELEASEREQ,
                    send_block_no = BlockNo,
                    recv_block_no = ?DBTC,
                    signal_data_length = 3,
                    signal_data = [
                                   Id,
                                   ?NUMBER_TO_REF(BlockNo, NodeId),
                                   999
                                  ],
                    sections_length = 0},
     []
    ];
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

received(#signal{gsn=?GSN_TCRELEASECONF}, <<>>, Data) ->
    {noreply, reply(ok, Data)};
received(#signal{gsn=?GSN_TCSEIZECONF, signal_data=D}, <<>>, Data) ->
    %%
    %% signal_data_length != 3 ?, TODO
    %%
    %% ~/src/ndbapi/NdbTransaction.cpp: NdbTransaction::receiveTCSEIZECONF/1
    %%
    {noreply, reply({ok, lists:nth(2, D)}, Data)};
received(#signal{gsn=?GSN_TCSEIZEREF, signal_data=D}, <<>>, Data) ->
    %%
    %% ~/src/ndbapi/NdbTransaction.cpp: NdbTransaction::receiveTCSEIZEREF/1
    %%
    {noreply, reply({error, lists:nth(2, D)}, Data)}; % ndberror.c, TODO
received(#signal{gsn=?GSN_GET_TABINFO_CONF, signal_data=D}, Binary, Data) ->
    %%
    %% ~/include/kernel/signaldata/GetTabInfo.hpp: GetTabInfoConf
    %% ~/src/ndbapi/NdbDictionalyImpl.cpp: NdbDictInterface::execGET_TABINFO_CONF/2
    %%
    case lists:nth(1, D) =:= Data#data.request_id of
        true ->
            {noreply, reply({ok, unpack(Binary, 0, ?WORD(size(Binary)), [])}, Data)};
        false ->
            {noreply, Data}
    end;
received(#signal{gsn=?GSN_GET_TABINFOREF, signal_data=D}, <<>>, Data) ->
    %%
    %% ~/include/kernel/signaldata/GetTabInfo.hpp: GetTabInfoRef
    %% ~/src/ndbapi/NdbDictionalyImpl.cpp: NdbDictInterface::execGET_TABINFO_REF/2
    %%
    case lists:nth(1, D) =:= Data#data.request_id of
        true ->
            Reason = case lists:nth(6, D) of
                         701 -> <<"Busy">>;
                         702 -> <<"TableNameTooLong">>;
                         709 -> <<"InvalidTableId">>;
                         710 -> <<"NoFetchByName">>;
                         723 -> <<"TableNotDefined">>
                     end,
            {noreply, reply({error, Reason}, Data)};
        false ->
            {noreply, Data}
    end;
received(Signal, Binary, Data) ->
    ok = error_logger:warning_msg("[~p:~p] ~p,~p~n", [?MODULE, self(), Signal, Binary]),
    {noreply, Data}.


reply(Reply, #data{from=F}=X) ->
    _ = gen_server:reply(F, Reply),
    X#data{from = undefined}.

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
