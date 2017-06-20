-module(ndbepi_connection).

-include("internal.hrl").

%% -- private --
-export([start_link/2]).
-export([connect/3, disconnect/2]).

-behaviour(ndbepi_gen_block2).
-export([init/0, terminate/2, code_change/3,
         handle_call/5, handle_info/3]).

%% -- internal --
-record(data, {
          id :: undefined|integer() % TODO
         }).

%% == private ==

-spec start_link(node_id(), pos_integer()) -> {ok, pid()}|{error, _}.
start_link(NodeId, BlockNo) ->
    ndbepi_gen_block2:start_link(?MODULE, NodeId, BlockNo, []).


-spec connect(pid(), node_id(), integer()) -> ok|{error, _}.
connect(Pid, NodeId, Instance) ->
    ndbepi_gen_block2:call(Pid, {seize, [Instance], NodeId}).

-spec disconnect(pid(), node_id()) -> ok|{error, _}.
disconnect(Pid, NodeId) ->
    ndbepi_gen_block2:call(Pid, {release, [], NodeId}).

%% -- behaviour: ndbepi_gen_block2 --

init() ->
    {ok, #data{}}.

terminate(_Reason, _Data) -> % TODO: id
    ok.

code_change(_OldVsn, Data, _Extra) ->
    {ok, Data}.

handle_call(Request, NodeId, BlockNo, Signal, Data) ->
    {noreply, signal(Request, NodeId, BlockNo, Signal, Data), Data}.

handle_info(#signal{gsn=?GSN_TCRELEASECONF}, <<>>, Data) ->
    {reply, ok, Data#data{id = undefined}};
handle_info(#signal{gsn=?GSN_TCSEIZECONF}=S, <<>>, Data) ->
    %%
    %% signal_data_length != 3 ?, TODO
    %%
    %% ~/src/ndbapi/NdbTransaction.cpp: NdbTransaction::receiveTCSEIZECONF/1
    %%
    {reply, ok, Data#data{id = lists:nth(2, S#signal.signal_data)}};
handle_info(#signal{gsn=?GSN_TCSEIZEREF}=S, <<>>, Data) ->
    %%
    %% ~/src/ndbapi/NdbTransaction.cpp: NdbTransaction::receiveTCSEIZEREF/1
    %%
    {reply, {error, lists:nth(2, S#signal.signal_data)}, Data}; % ndberror.c, TODO
handle_info(Signal, Binary, Data) ->
    ok = error_logger:warning_msg("[~p:~p] ~p,~p~n", [?MODULE, self(), Signal, Binary]),
    {noreply, Data}.

%% == internal ==

%% 00f50001

%% Ndb.cpp : Ndb::startTransaction/4 (NdbRecord/char)
%% - computeHash
%% - startTransaction/2 (Table/PartitionId)

%% Ndb.cpp : Ndb::startTransaction/4 (Table/Key_part_ptr)
%% - computeHash
%% - startTransaction/2 (Table/partitionId)

%% Ndb.cpp : Ndb::startTransaction/4 (Table/partitionId)
%% - table + partitionId => node_id
%% - startTransactionLocal(0, node, 0) % PRIORITY! node instance?

%% NDb.cpp : Ndb::startTransactionLocal/3
%% - doConnect => NdbTransaction!
%% - - NDB_connect : GSN_TCSEIZEREQ
%% - - getConnectedNdbTransaction (pool) => NdbTransaction
%% - tFirstTransId, aPriority > NdbTransaction

signal({seize, [Instance]}, NodeId, BlockNo, Default, _Data) ->
    %%
    %% ~/src/ndbapi/src/Ndb.cpp: Ndb::NDB_connect/2
    %%
    [
     Default#signal{gsn = ?GSN_TCSEIZEREQ,
                    send_block_no = BlockNo,
                    recv_block_no = ?DBTC,
                    signal_data_length = 3,
                    signal_data = [
                                   0, % pool.index
                                   ?NUMBER_TO_REF(BlockNo, NodeId),
                                   Instance
                                  ],
                    sections_length = 0},
     []
    ];
signal({release, []}, NodeId, BlockNo, Default, Data) ->
    %%
    %% ~/src/ndbapi/Ndblist.cpp: Ndb::releaseConnectToNdb/1
    %%
    [
     Default#signal{gsn = ?GSN_TCRELEASEREQ,
                    send_block_no = BlockNo,
                    recv_block_no = ?DBTC,
                    signal_data_length = 3,
                    signal_data = [
                                   Data#data.id,
                                   ?NUMBER_TO_REF(BlockNo, NodeId),
                                   0
                                  ],
                    sections_length = 0},
     []
    ].

%% NdbTransaction.cpp : NdbTransaction::getNdbOperation/2
%% % NdbOperation.cpp : NdbOperation::init/2
%% %  GSN_TCKEYREQ

%% NdbTransaction.cpp : NdbTransaction::getNdbScanOperation/1
%% % NdbScanOperation.cpp : NdbScanOperation::init/2
%% % - NdbOperation::init/2 ?!
%% % - Ndb.cpp: Ndb::hupp/1 ?!
%% % - - startTransactionLocal ?!

%% NdbOperationDefine.cpp : NdbOperation::readTuple/1, LM_Read
%% - NdbOperation::readTuple - NdbOperationDefine.cpp

%% NdbOperation.cpp : NdbOperation::equal/2
%% - table.gtColumn(attrName|attrId) => col
%% - NdbOperation::equal_impl/2 - NdbOperationSearch.cpp

%% NdbOperation.cpp : NdbOperation::getValue/2
%% - NdbOperation::getValue_impl/2 - NdbOperationSearch.cpp

%% NdbTransaction.cpp : NdbTransaction::execute/3
%% - executeNoBlobs/3  NoCommit,DefaultAbortOption,0
%% - - executeAsynchPrepare/4  NoCommit,NULL,NULL,DefaultAbortOption
%% - - - NdbQueryImpl::prepareSend/0 NdbQueryOperation.cpp
%% - - - - NdbQueryOperationImpl::prepareAttrInfo/1 NdbQueryOperation.cpp
%% - - - - - NdbQueryOperationImpl::serializeProject/1 NdbQueryOperation.cpp
%%
%% - - Ndb::sendPollNdb/3 Ndbif.cpp
%% - - - Ndb::sendPrepTrans/1 Ndbif.cpp
%% - - - - NdbTransaction::doSend/0 NdbTransaction.cpp
%%
%% - - - - - NdbQueryImpl::doSend/2 NdbQueryOperation.cpp
%%             (scan:scanTabReq)
%% - - - - - - NdbImpl::sendFragmentedSignal/4 NdbImpl.hpp
%%            (lookup:tcKeyReq)
%% - - - - - - NdbImpl::sendSignal/4 NdbImpl.hpp
%%
%% - - - - - NdbOperation::doSend/2 NdbOperationExec.cpp
%% - - - - - - NdbOperation::doSendKeyReq/4 NdbOperationExec.cpp
%% - - - - - - - NdbOperation::setRequestInfoTCKEYREQ/2 NdbOperationExec.cpp
%% - - - - - - - NdbImpl::sendSignal/4 NdbImpl.hpop
