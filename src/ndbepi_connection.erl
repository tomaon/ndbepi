-module(ndbepi_connection).

-include("internal.hrl").

%% -- private --
-export([start_link/2]).
-export([call/0]).

-behaviour(ndbepi_gen_block2).
-export([init/0, terminate/2, code_change/3,
         handle_call/5, handle_info/3]).

%% -- internal --
-record(data, {
         }).

%% == private ==

-spec start_link(node_id(), pos_integer()) -> {ok, pid()}|{error, _}.
start_link(NodeId, BlockNo) ->
    ndbepi_gen_block2:start_link(?MODULE, NodeId, BlockNo, []).


call() ->
    {ok, P} = ndbepi:connect(),
    {ok, I3} = ndbepi_gen_block2:call(P, {startTransaction, [0], undefined}),
    io:format("~p~n~n", [I3]),
    ok = ndbepi_gen_block2:call(P, {closeTransaction, [I3], undefined}).

%% ndbepi_connection:call()).

%% -- behaviour: ndbepi_gen_block2 --

init() ->
    {ok, #data{}}.

terminate(_Reason, _Data) ->
    ok.

code_change(_OldVsn, Data, _Extra) ->
    {ok, Data}.

handle_call(Request, NodeId, BlockNo, Signal, Data) ->
    {noreply, signal(Request, NodeId, BlockNo, Signal), Data}.

handle_info(#signal{gsn=?GSN_TCRELEASECONF}, <<>>, Data) ->
    {reply, ok, Data};
handle_info(#signal{gsn=?GSN_TCSEIZECONF}=S, <<>>, Data) ->
    %%
    %% signal_data_length != 3 ?, TODO
    %%
    %% ~/src/ndbapi/NdbTransaction.cpp: NdbTransaction::receiveTCSEIZECONF/1
    %%
    {reply, {ok, lists:nth(2, S#signal.signal_data)}, Data};
handle_info(#signal{gsn=?GSN_TCSEIZEREF}=S, <<>>, Data) ->
    %%
    %% ~/src/ndbapi/NdbTransaction.cpp: NdbTransaction::receiveTCSEIZEREF/1
    %%
    {reply, {error, lists:nth(2, S#signal.signal_data)}, Data}; % ndberror.c, TODO
handle_info(Signal, Binary, Data) ->
    ok = error_logger:warning_msg("[~p:~p] ~p,~p~n", [?MODULE, self(), Signal, Binary]),
    {noreply, Data}.

%% == internal ==

signal({startTransaction, [Instance]}, NodeId, BlockNo, Default) ->
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
signal({closeTransaction, [Id]}, NodeId, BlockNo, Default) ->
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
    ].
