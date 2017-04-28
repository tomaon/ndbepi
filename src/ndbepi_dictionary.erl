-module(ndbepi_dictionary).

-include("internal.hrl").
-include("../include/ndbepi.hrl").

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


names() ->
    [
     %%{?ATTRIBUTE_NAME, <<>>},
     {?ATTRIBUTE_ID, <<"AttributeId">>},
     {?ATTRIBUTE_TYPE, <<"AttributeType">>},
     {?ATTRIBUTE_SIZE, <<"AttributeSize">>},
     {?ATTRIBUTE_ARRAY_SIZE, <<"AttributeArraySize">>},
     {?ATTRIBUTE_KEY_FLAG, <<"AttributeKeyFlag">>},
     {?ATTRIBUTE_STORAGE_TYPE, <<"AttributeStorageType">>},
     {?ATTRIBUTE_NULLABLE_FLAG, <<"AttributeNullableFlag">>},
     %%{?ATTRIBUTE_DYNAMIC, <<>>},
     {?ATTRIBUTE_D_KEY, <<"AttributeDKey">>},
     %% AttributeGroup
     {?ATTRIBUTE_EXT_TYPE, <<"AttributeExtType">>},
     {?ATTRIBUTE_EXT_PRECISION, <<"AttributeExtPrecision">>},
     {?ATTRIBUTE_EXT_SCALE, <<"AttributeExtScale">>},
     {?ATTRIBUTE_EXT_LENGTH, <<"AttributeExtLength">>},
     {?ATTRIBUTE_AUTO_INCREMENT, <<"AttributeAutoIncrement">>},
     {?ATTRIBUTE_ARRAY_TYPE, <<"AttributeArrayType">>},
     {?ATTRIBUTE_DEFAULT_VALUE_LEN, <<"AttributeDefaultValueLen">>},
     {?ATTRIBUTE_DEFAULT_VALUE, <<"AttributeDefaultValue">>}
    ].

xx() ->
    [
     {?TABLE_VERSION, <<"Version">>},
     {?FRAGMENT_TYPE, <<"Fragment type">>},
     {?TABLE_K_VALUE, <<"K Value">>},
     {?MIN_LOAD_FACTOR, <<"Min load factor">>},
     {?MAX_LOAD_FACTOR, <<"Max load factor">>},
     {?TABLE_LOGGED_FLAG, <<"Temporary table">>}, % "no"|"yes"
     {xx, <<"Number of attributes">>}, % m_columns.size()
     {xx, <<"Number of primary keys">>}, % m_noOfKeys
     {xx, <<"Length of frm data">>}, % m_frm.length
     {xx, <<"Max Rows">>}, % m_max_rows
     {?ROW_CHECKSUM_FLAG, <<"Row Checksum">>},
     {?ROW_GCI_FLAG, <<"Row GCI">>},
     {?SINGLE_USER_MODE, <<"SingleUserMode">>},
     {?FORCE_VAR_PART_FLAG, <<"ForceVarPart">>},
     {?PARTITION_COUNT, <<"PartitionCount">>},
     {?FRAGMENT_COUNT, <<"FragmentCount">>},
     {?PARTITION_BALANCE, <<"PartitionBalance">>},
     {?EXTRA_ROW_GCI_BITS, <<"ExtraRowGciBits">>},
     {?EXTRA_ROW_AUTHOR_BITS, <<"ExtraRowAuthorBits">>},
     {xx, <<"TableStatus">>}, % m_status
     {xx, <<"Table options">>} % READ_BACKUP_FLAG,FULLY_REPLICATED_FLAG
     %% REFERENCES m_references[0] / m_references[2]
     %% on update, on delete
    ].

x1() ->
    [
     {?FRAG_UNDEFINED, <<"FragUndefined">>},
     {?FRAG_SINGLE, <<"FragSingle">>},
     {?FRAG_ALL_SMALL, <<"FragAllSmall">>},
     {?FRAG_ALL_MEDIUM, <<"FragAllMedium">>},
     {?FRAG_ALL_LARGE, <<"FragAllLarge">>},
     {?DISTR_KEY_HASH, <<"DistrKeyHash">>},
     {?DISTR_KEY_LIN, <<"DistrKeyLin">>},
     {?USER_DEFINED, <<"UserDefined">>},
     {?HASH_MAP_PARTITION, <<"HashMapPartition">>}
    ].

x2() ->
    [
     {?TYPE_UNDEFINED, <<"Undefined">>},
     {?SYSTEM_TABLE, <<"SystemTable">>},
     {?USER_TABLE, <<"UserTable">>},
     {?UNIQUE_HASH_INDEX, <<"UniqueHashIndex">>},
     {?ORDERED_INDEX, <<"OrderedIndex">>},
     {?HASH_INDEX_TRIGGER, <<"HashIndexTrigger">>},
     {?INDEX_TRIGGER, <<"IndexTrigger">>},
     {?SUBSCRIPTION_TRIGGER, <<"SubscriptionTrigger">>},
     {?READ_ONLY_CONSTRAINT, <<"ReadOnlyConstraint">>},
     {?TABLE_EVENT, <<"TableEvent">>},
     {?TABLESPACE, <<"Tablespace">>},
     {?LOGFILE_GROUP, <<"LogfileGroup">>},
     {?DATAFILE, <<"Datafile">>},
     {?UNDOFILE, <<"Undofile">>},
     {?REORG_TRIGGER, <<"ReorgTrigger">>},
{xx, <<"FullyReplicatedTrigger">>},
     {?HASH_MAP, <<"HashMap">>},
     {?FOREIGN_KEY, <<"ForeignKey">>},
     {?FK_PARENT_TRIGGER, <<"FKParentTrigger">>},
     {?FK_CHILD_TRIGGER, <<"FKChildTrigger">>}
    ].

%% index : Version, Base table, Number of attributes, Logging, Index (type|status)
%% index.type : Undefined, UniqueHashIndex, OrderedIndex

%% status : New, Changed, Retrieved, Invalid, Altered
