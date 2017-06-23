-module(ndbepi_connection).

-include("internal.hrl").

%% -- private --
-export([start_link/2]).
-export([startTransaction/3, closeTransaction/2]).

-behaviour(ndbepi_gen_block2).
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_info/3]).

%%
%% transactionId = BlockNo + nodeId + counter
%%


%% -- internal --
-record(data, {
          node_id  :: node_id(),
          block_no :: block_no(),
          index    :: non_neg_integer()
         }).

%% == private ==

-spec start_link(node_id(), block_no()) -> {ok, pid()}|{error, _}.
start_link(NodeId, BlockNo) ->
    ndbepi_gen_block2:start_link(?MODULE, BlockNo, [NodeId, BlockNo], []).


-spec startTransaction(pid(), node_id(), integer()) -> ok|{error, _}.
startTransaction(Pid, NodeId, Instance) ->
    ndbepi_gen_block2:call(Pid, {seize, [Instance], NodeId}).

-spec closeTransaction(pid(), node_id()) -> ok|{error, _}.
closeTransaction(Pid, NodeId) ->
    ndbepi_gen_block2:call(Pid, {release, [], NodeId}).

%% -- behaviour: ndbepi_gen_block2 --

init([NodeId, BlockNo]) ->
    {ok, #data{node_id = NodeId, block_no = BlockNo, index = 0}}.

terminate(_Reason, _Data) -> % TODO: id
    ok.

code_change(_OldVsn, Data, _Extra) ->
    {ok, Data}.

handle_call(Request, Signal, Data) ->
    {noreply, signal(Request, Signal, Data), Data}.

handle_info(#signal{gsn=?GSN_TCRELEASECONF}, <<>>, Data) ->
    %%
    %% ~/src/ndbapi/NdbTransaction.cpp: NdbTransaction::receiveTCRELEASECONF/1
    %%
    {reply, ok, Data#data{index = 0}};
handle_info(#signal{gsn=?GSN_TCRELEASEREF}=S, <<>>, Data) ->
    %%
    %% ~/src/kernel/blocks/dbtc/Dbtc.hpp
    %% ~/src/ndbapi/NdbTransaction.cpp: NdbTransaction::receiveTCSEIZEREF/1
    %%
    Reason = case lists:nth(2, S#signal.signal_data) of
                 229 -> <<"InvalidConnection">>
             end,
    {reply, {error, Reason}, Data};
handle_info(#signal{gsn=?GSN_TCSEIZECONF}=S, <<>>, Data) ->
    %%
    %% ~/src/ndbapi/NdbTransaction.cpp: NdbTransaction::receiveTCSEIZECONF/1
    %%
    {reply, ok, Data#data{index = lists:nth(2, S#signal.signal_data)}};
handle_info(#signal{gsn=?GSN_TCSEIZEREF}=S, <<>>, Data) ->
    %%
    %% ~/src/kernel/blocks/dbtc/Dbtc.hpp
    %% ~/src/ndbapi/NdbTransaction.cpp: NdbTransaction::receiveTCSEIZEREF/1
    %%
    Reason = case lists:nth(2, S#signal.signal_data) of
                 203 -> <<"SystemNotStartedError">>;
                 219 -> <<"NoFreeApiConnection">>
             end,
    {reply, {error, Reason}, Data};
handle_info(Signal, Binary, Data) ->
    ok = error_logger:warning_msg("[~p:~p] ~p,~p~n", [?MODULE, self(), Signal, Binary]),
    {noreply, Data}.

%% == internal ==

signal({seize, [Instance]}, Default, Data) ->
    %%
    %% ~/src/ndbapi/src/Ndb.cpp: Ndb::NDB_connect/2
    %%
    [
     Default#signal{gsn = ?GSN_TCSEIZEREQ,
                    send_block_no = Data#data.block_no,
                    recv_block_no = ?DBTC,
                    signal_data_length = 3,
                    signal_data = [
                                   0,
                                   ?NUMBER_TO_REF(Data#data.block_no, Data#data.node_id),
                                   Instance
                                  ],
                    sections_length = 0},
     []
    ];
signal({release, []}, Default, Data) ->
    %%
    %% ~/src/ndbapi/Ndblist.cpp: Ndb::releaseConnectToNdb/1
    %%
    [
     Default#signal{gsn = ?GSN_TCRELEASEREQ,
                    send_block_no = Data#data.block_no,
                    recv_block_no = ?DBTC,
                    signal_data_length = 3,
                    signal_data = [
                                   Data#data.index,
                                   ?NUMBER_TO_REF(Data#data.block_no, Data#data.node_id),
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
