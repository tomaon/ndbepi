#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin -config priv/conf/n2 -s ndbepi

-include_lib("ndbepi/src/internal.hrl").

-define(N, (?MIN_API_BLOCK_NO + 1)).

-define(TRANS_ID1, 16#00000000).
-define(TRANS_ID2, 16#0010c900).

%%
%% -- ~/src/ndbapi/Ndbif.cpp: Ndb::connected/1 --
%% Ndb.theFirstTransId += ((Uint64)tBlockNo << 52) + ((Uint64)tmpTheNode << 40);
%%                                  2 << 52
%% -- ~/src/ndbapi/Ndb.cpp: Ndb::startTransactionLocal/3 --  << startTransaction/2,3
%%                                                              << ...
%%                                                           << hupp/1
%%                                                              << NdbQueryImpl::prepareSend/0
%%                                                              << NdbScanOperation::init/2
%% if ((tFirstTransId & 0xFFFFFFFF) == 0xFFFFFFFF) {
%%   //---------------------------------------------------
%%   // Transaction id rolling round. We will start from
%%   // consecutive identity 0 again.
%%   //---------------------------------------------------
%%   theFirstTransId = ((tFirstTransId >> 32) << 32);
%% } else {
%%   theFirstTransId = tFirstTransId + 1;
%% }//if
%%
%% -- ~/src/ndbapi/Ndb.cpp: Ndb::allocate_transaction_id/0 -- << NdbTransaction::restart/1
%% if ((theFirstTransId & 0xFFFFFFFF) == 0xFFFFFFFF) {
%%   theFirstTransId = (theFirstTransId >> 32) << 32;
%% } else {
%%   theFirstTransId++;
%% }
%%
%% -- ~/src/ndbapi/Ndb.cpp: Ndb::getNextTransactionId/0 -- << (Test Only)
%% return theFirstTransId;
%%

run(?GSN_TCKEYREQ, P, [_,Q2,_]) ->

    %% - RequestInfo -                  Short Long
    %%  a : Attr Info in TCKEYREQ =  3      o    -       0
    %%  b : Distribution Key Ind  =  1      o    o       -
    %%  c : Commit Indicator      =  1      o    o       0=false
    %%  d : Dirty Indicator       =  1      o    o       1=true
    %%  e : Scan Indicator        =  1      o    o       -
    %%  i : Interpreted Indicator =  1      o    o       0?, TODO
    %%  k : Key length            = 12      o    -       0
    %%  l : Execute               =  1      o    o       1?,lastFlag
    %%  n : No disk flag          =  1      o    o       ?,!m_diskInUserProjection
    %%  o : Operation Type        =  3      o    o       0=ReadRequest
    %%  p : Simple Indicator      =  1      o    o       1=true
    %%  q : Queue on redo problem =  1      -    o       -
    %%  r : reorg flag            =  1      o    o       -
    %%  s : Start Indicator       =  1      o    o       1?=m_startIndicator
    %%  v : Via SPJ               =  1      -    o       1=true
    %%  x : Coordinated Tx flag   =  1      -    o       -
    %%  y : Commit Type           =  2      o    o       2=AO_IgnoreError
    %%  D : deferred constraint   =  1      -    o       -
    %%
    %%          3          2          1          0
    %%         10987654 32109876 54321098 76543210
    %% (Short) kkkkkkkk kkkkraaa ieyysl p oooc bnd
    %%  Long                r Dx ieyyslqp ooocvbnd  y=2 s=1 l=1 q=1 n=1 d=1
    %%         00000000 00000000 00101110 00000011  11778=0x2e02

    %%
    %% @see
    %%   ~/include/kernel/singaldata/TcKeyConf.hpp TcKeyConf
    %%   ~/include/kernel/singaldata/TcKeyRef.hpp  TcKeyRef
    %%   ~/include/kernel/singaldata/TcKeyReq.hpp  TcKeyReq
    %%   ~/include/kernel/singaldata/TcRollbackRep TcRollbackRep
    %%   ~/src/ndbapi/Ndbif.cpp: NdbImpl::trp_deliver_signal/2
    %%   ~/src/ndbapi/NdbOperationExec.cpp: NdbOperation::doSendKeyReq/3
    %%
    case call(P, #signal{ % NDBD_LONG_TCKEYREQ > 6.4.0
                    gsn = ?GSN_TCKEYREQ,
                    senders_block_no = ?N,
                    receivers_block_no = ?DBTC,
                    signal_data_length = 8,
                    no_of_sections = 2,
                    signal_data = [
                                   %% "must be present in signal", 8
                                   Q2,                   % apiConnectPtr
                                   20,     % ?           % apiOperationPtr    |senderData
                                   0,      % LONG(FORCE) % attrLen
                                   10,                   % tableId
                                   11778,                % requestInfo
                                   16777217,             % tableSchemaVersion
                                   ?TRANS_ID1,           % transId1
                                   ?TRANS_ID2            % transId2
                                  ]
                   }, [
                       1532,                             % keyInfo[MaxKeyInfo=8]
                       81603526660 % 0x00000012-fff30004 % attrInfo[MaxAttrInfo=5]
                      ]) of
        ok ->

            receive % 0xffffff00 -> lookup?, magic = 0x37412619?
                #signal{gsn=?GSN_TCKEYCONF,byte_order=E,binary=B,no_of_sections=N} % -> TC_COMMIT_ACK, TODO
                  when ?WORD(5) < byte_size(B) ->
                    C0 = binary_to_word(B, ?WORD(0), E), % apiConnectPtr
                    C1 = binary_to_word(B, ?WORD(1), E), % gci_hi
                    C2 = binary_to_word(B, ?WORD(2), E), % confInfo >> noOfOperations,...
                    C3 = binary_to_word(B, ?WORD(3), E), % transId1
                    C4 = binary_to_word(B, ?WORD(4), E), % transId2
                    O = binary_to_words(B, ?WORD(5), E), % operations[noOfOperations < 10], gci_lo!
                    io:format("GSN_TCKEYCONF:~p ~p,~p,~p,[~p,~p] ~p~n", [N,C0,C1,C2,C3,C4,O]);
                #signal{gsn=?GSN_TCKEYREF,byte_order=E,binary=B}
                  when ?WORD(5) =:= byte_size(B) ->
                    R0 = binary_to_word(B, ?WORD(0), E), % connectPtr
                    R1 = binary_to_word(B, ?WORD(1), E), % transid[0]?
                    R2 = binary_to_word(B, ?WORD(2), E), % transid[1]?
                    R3 = binary_to_word(B, ?WORD(3), E), % errCode
                    R4 = binary_to_word(B, ?WORD(4), E), % errorData
                    io:format("GSN_TCKEYREF: ~p,[~p,~p],~p,~p~n", [R0,R1,R2,R3,R4]);

                %% GSN_TRANSID_AI
                %% {signal,5,249,32770,0,1,0,1,0,1,0,0,21,16,0,3013734,[],
                %%  <<20,0,0,0,0,0,0,0,0,201,32,0,4,0,243,255,18,0,
                %%    0,0,84,111,107,121,111,32,32,32,32,32,32,32,
                %%    32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,
                %%    32,32,32,32,32,32,32,32,0,198,196,121,0>>}

                #signal{gsn=?GSN_TCROLLBACKREP,byte_order=E,binary=B} % ?!
                  when ?WORD(5) =:= byte_size(B) ->
                    R0 = binary_to_word(B, ?WORD(0), E), % connectPtr
                    R1 = binary_to_word(B, ?WORD(1), E), % transid[0]?
                    R2 = binary_to_word(B, ?WORD(2), E), % transid[1]?
                    R3 = binary_to_word(B, ?WORD(3), E), % errCode
                    R4 = binary_to_word(B, ?WORD(4), E), % errorData
                    io:format("GSN_TCROLLBACKREP: ~p,[~p,~p],~p,~p~n", [R0,R1,R2,R3,R4]);

                S ->
                    io:format("ERROR: GSN_TCKEYREQ -> ~p~n", [S])
            end
    end;
run(?GSN_TCROLLBACKREQ, P, [_,Q2,_,T1,T2]) ->
    %%
    %% @see
    %%   ~/src/kernel/blocks/dbtc/Dbtc.hpp
    %%   ~/src/kernel/blocks/dbtc/DbtcMain.cpp: Dbtc::execTCROLLBACKREQ/1
    %%   ~/src/ndbapi/NdbTransaction.cpp: NdbTransaction::sendROLLBACK/0
    %%
    case call(P, #signal{
                    gsn = ?GSN_TCROLLBACKREQ,
                    senders_block_no = ?N,
                    receivers_block_no = ?DBTC,
                    signal_data_length = 3,
                    signal_data = [
                                   Q2,                   % tuserpointer(apiConnectptr.i)
                                   T1,                   % tTransId1 (L)
                                   T2                    % tTransId2 (H)
                                  ]
                   }, []) of
        ok ->
            receive
                #signal{gsn=?GSN_TCROLLBACKCONF,byte_order=E,binary=B}
                  when ?WORD(3) =:= byte_size(B) ->
                    C0 = binary_to_word(B, ?WORD(0), E), % ndbapiConnect?
                    C1 = binary_to_word(B, ?WORD(1), E), % transid[0]?
                    C2 = binary_to_word(B, ?WORD(2), E), % transid[1]?
                    io:format("GSN_TCROLLBACKCONF: ~p,[~p,~p]~n", [C0,C1,C2]);
                #signal{gsn=?GSN_TCROLLBACKREF,byte_order=E,binary=B}
                  when ?WORD(5) =:= byte_size(B) ->
                    R0 = binary_to_word(B, ?WORD(0), E), % ndbapiConnect?
                    R1 = binary_to_word(B, ?WORD(1), E), % transid[0]?
                    R2 = binary_to_word(B, ?WORD(2), E), % transid[1]?
                    R3 = binary_to_word(B, ?WORD(3), E), % errCode
                    R4 = binary_to_word(B, ?WORD(4), E), % apiConnectstate
                    io:format("GSN_TCROLLBACKREF: ~p,[~p,~p],~p,~p~n", [R0,R1,R2,R3,R4]);
                S ->
                    io:format("ERROR: GSN_TCROLLBACKREQ -> ~p~n", [S])
            end
    end;
run(?GSN_TCRELEASEREQ, P, [Q1,Q2,_]) ->
    %%
    %% @see
    %%   ~/src/kernel/blocks/dbtc/Dbtc.hpp
    %%   ~/src/kernel/blocks/dbtc/DbtcMain.cpp: Dbtc::execTCRELEASEREQ/1
    %%   ~/src/ndbapi/src/Ndblist.cpp: Ndb::releaseConnectToNdb/1 << Ndb::~Ndb/0
    %%
    case call(P, #signal{
                    gsn = ?GSN_TCRELEASEREQ,
                    senders_block_no = ?N,
                    receivers_block_no = ?DBTC,
                    signal_data_length = 3,
                    signal_data = [
                                   Q2,                   % tuserpointer(apiConnectptr.i), !tapiPointer
                                   numberToRef(?N,201) , % tapiBlockerRef
                                   Q1                    % tapiPointer, !tuserpointer
                                  ]
                   }, []) of
        ok ->
            receive
                #signal{gsn=?GSN_TCRELEASECONF,byte_order=E,binary=B}
                  when ?WORD(1) =:= byte_size(B) ->
                    C0 = binary_to_word(B, ?WORD(0), E), % tapiPointer
                    io:format("GSN_TCRELEASECONF: ~p~n", [C0]);
                #signal{gsn=?GSN_TCRELEASEREF,byte_order=E,binary=B}
                  when ?WORD(1) =:= byte_size(B) ->
                    R0 = binary_to_word(B, ?WORD(0), E), % tapiPointer
                    io:format("GSN_TCRELEASEREF: ~p~n", [R0]);
                #signal{gsn=?GSN_TCRELEASEREF,byte_order=E,binary=B}
                  when ?WORD(3) =:= byte_size(B) ->
                    R0 = binary_to_word(B, ?WORD(0), E), % tapiPointer
                    R1 = binary_to_word(B, ?WORD(1), E), % errCode
                    R2 = binary_to_word(B, ?WORD(2), E), % line
                    io:format("GSN_TCRELEASEREF: ~p,~p,~p~n", [R0,R1,R2]);
                #signal{gsn=?GSN_TCRELEASEREF,byte_order=E,binary=B}
                  when ?WORD(4) =:= byte_size(B) ->
                    R0 = binary_to_word(B, ?WORD(0), E), % tapiPointer
                    R1 = binary_to_word(B, ?WORD(1), E), % errCode
                    R2 = binary_to_word(B, ?WORD(2), E), % line
                    R3 = binary_to_word(B, ?WORD(3), E), % apiConnectstate
                    io:format("GSN_TCRELEASEREF: ~p,~p,~p,~p~n", [R0,R1,R2,R3]);
                #signal{gsn=?GSN_TCRELEASEREF,byte_order=E,binary=B}
                  when ?WORD(5) =:= byte_size(B) ->
                    R0 = binary_to_word(B, ?WORD(0), E), % tapiPointer
                    R1 = binary_to_word(B, ?WORD(1), E), % errCode
                    R2 = binary_to_word(B, ?WORD(2), E), % line
                    R3 = binary_to_word(B, ?WORD(3), E), % tapiBlockref
                    R4 = binary_to_word(B, ?WORD(4), E), % apiConnectstate
                    io:format("GSN_TCRELEASEREF: ~p,~p,~p,~p,~p~n", [R0,R1,R2,ref_to_tuple(R3),R4]);
                S ->
                    io:format("ERROR: GSN_TCRELEASEREQ -> ~p~n", [S])
            end
    end;
run(?GSN_TCSEIZEREQ, P, []) ->
    %%
    %% tapiPointer: "Set my block reference" = connection_pool.index (LOCAL)
    %%                'connection object' = NdbTransaction << Ndb::theConIdleList
    %%                'pointer' << NdbImpl::theNdbObjectIdMap.index
    %% instance   : "Set requested instance" => DBTC[n=0] (REMOTE)
    %%
    %% @see
    %%   ~/src/kernel/blocks/dbtc/Dbtc.hpp
    %%   ~/src/kernel/blocks/dbtc/DbtcMain.cpp: Dbtc::execTCSEIZEREQ/1
    %%   ~/src/ndbapi/src/Ndb.cpp: Ndb::NDB_connect/2
    %%
    case call(P, #signal{
                    gsn = ?GSN_TCSEIZEREQ,
                    senders_block_no = ?N,
                    receivers_block_no = ?DBTC,
                    signal_data_length = 3,
                    signal_data = [
                                   999,                  % tapiPointer
                                   numberToRef(?N,201),  % tapiBlockerRef
                                   0                     % instance
                                  ]
                   }, []) of
        ok ->
            receive
                #signal{gsn=?GSN_TCSEIZECONF,byte_order=E,binary=B}
                  when ?WORD(3) =:= byte_size(B) ->
                    C0 = binary_to_word(B, ?WORD(0), E), % tapiPointer(apiConnectptr.p->ndbapiConnect)
                    C1 = binary_to_word(B, ?WORD(1), E), % tuserpointer(apiConnectptr.i)
                    C2 = binary_to_word(B, ?WORD(2), E), % reference = 0x00f50001 = DBTC[1]
                    io:format("GSN_TCSEIZECONF: ~p,~p,~p~n", [C0,C1,ref_to_tuple(C2)]),
                    run(?GSN_TCKEYREQ, P, [C0,C1,C2]),
                    run(?GSN_TCRELEASEREQ, P, [C0,C1,C2]);
                #signal{gsn=?GSN_TCSEIZEREF,byte_order=E,binary=B}
                  when ?WORD(2) =:= byte_size(B) ->
                    R0 = binary_to_word(B, ?WORD(0), E), % tapiPointer
                    R1 = binary_to_word(B, ?WORD(1), E), % errCode
                    io:format("GSN_TCSEIZEREF: ~p,~p~n", [R0,R1]);
                S ->
                    io:format("ERROR: GSN_TCSEIZEREQ -> ~p~n", [S])
            end
    end;
run(?GSN_GET_TABINFOREQ, P, []) -> % check: ndb_show_tables (Event,Trigger,other?)
    %%
    %% @see
    %%   ~/include/kernel/singaldata/GetTabInfo.hpp GetTabInfo(Conf|Ref|Req)
    %%   ~/src/ndbapi/NdbDictionaryImpl.cpp: NdbDictInterface::getTable/2
    %%   ~/src/ndbapi/NdbDictionaryImpl.cpp: NdbDictInterface::execGET_TABINFO_CONF/2
    %%   ~/src/ndbapi/NdbDictionaryImpl.cpp: NdbDictInterface::execGET_TABINFO_REF/2
    %%

    %% -- ByName|ById --, fragment_info=1,3
    Name = <<"test/def/city">>,

    %% -- ByName -> HASHMAP -> ById --, fragment_info=0
    %%Id = 1, %% 1=TabInfo.HASH_MAP_OBJECT_ID(153) if HASH_MAP_PARTITION(9) = TabInfo.FRAGMENT_TYPE(13)

    case call(P, #signal{
                    gsn = ?GSN_GET_TABINFOREQ,
                    senders_block_no = ?N,
                    receivers_block_no = ?DBDICT,
                    signal_data_length = 5,
                    %% -- ByName --
                    no_of_sections = 1,
                    signal_data = [
                                   0,                    % senderData
                                   numberToRef(?N,201),  % senderRef
                                   1+2,                  % requestType
                                   byte_size(Name)+1,    % tableNameLen
                                   0                     % schemaTransId
                                  ]
                    %% -- ById --
                    %% no_of_sections = 0,
                    %% signal_data = [
                    %%                0,                    % senderData
                    %%                numberToRef(?N,201),  % senderRef
                    %%                0+2,                  % requestType
                    %%                Id,                   % tableId
                    %%                0                     % schemaTransId
                    %%               ]
                   }, [
                       Name
                      ]) of
        ok ->
            receive
                #signal{gsn=?GSN_GET_TABINFOCONF,
                        fragment_info=1,signal_data_length=D,no_of_sections=1,binary=B} ->

                    %% 0 < F -> D = 8 = 6 + 1(UNKNOWN) + 1(FragmentId), TODO

                    FragmentId = binary_part(B, ?WORD(D-1), ?WORD(1)),
                    Fragment = binary_part(B, ?WORD(D+1), byte_size(B)-?WORD(D+1)),

                    X = fragments(1, FragmentId, [Fragment]),
                    L = unpack(X, 0, byte_size(X), []),

                    io:format("GSN_GET_TABINFOCONF:1 ~p~n", [L]);

                #signal{gsn=?GSN_GET_TABINFOCONF,
                        fragment_info=0,signal_data_length=D,no_of_sections=1,binary=B} ->
                    %% C0 = binary_to_word(B, ?WORD(0), E), % senderData
                    %% C1 = binary_to_word(B, ?WORD(1), E), % tableId
                    %% C2 = binary_to_word(B, ?WORD(2), E), % gci       (|freeWordsHi)
                    %% C3 = binary_to_word(B, ?WORD(3), E), % totalLen  (|freeExtents|freeWordsLo)
                    %% C4 = binary_to_word(B, ?WORD(4), E), % tableType
                    %% C5 = binary_to_word(B, ?WORD(5), E), % senderRef
                    X = binary_part(B, ?WORD(D+1), byte_size(B)-?WORD(D+1)),
                    io:format("GSN_GET_TABINFOCONF:0 ~p~n", [unpack(X,0,byte_size(X),[])]);
                #signal{gsn=?GSN_GET_TABINFOREF,byte_order=E,binary=B}
                  when ?WORD(7) =:= byte_size(B) ->
                    R0 = binary_to_word(B, ?WORD(0), E), % senderData
                    R1 = binary_to_word(B, ?WORD(1), E), % senderRef
                    R2 = binary_to_word(B, ?WORD(2), E), % requestType
                    R3 = binary_to_word(B, ?WORD(3), E), % tableNameLen|tableId
                    R4 = binary_to_word(B, ?WORD(4), E), % schemaTransId
                    R5 = binary_to_word(B, ?WORD(5), E), % errorCode
                    R6 = binary_to_word(B, ?WORD(6), E), % apiConnectstateerrorLine
                    io:format("GSN_GET_TABINFOREF: ~p,~p,~p,~p,~p,~p,~p~n", [R0,R1,R2,R3,R4,R5,R6]);
                S ->
                    io:format("ERROR: GSN_GET_TABINFOREQ -> ~p~n", [S])
            end
    end.

main(_) ->
    timer:sleep(timer:seconds(1)),
    P = find([ndbepi_sup,16#0000,1]),
    %%run(?GSN_GET_TABINFOREQ, P, []);
    run(?GSN_TCSEIZEREQ, P, []).

%% -- ~/include/ndb_constants.h --
-define(NDB_TYPE_UNDEFINED,           0).
-define(NDB_TYPE_TINYINT,             1).
-define(NDB_TYPE_TINYUNSIGNED,        2).
-define(NDB_TYPE_SMALLINT,            3).
-define(NDB_TYPE_SMALLUNSIGNED,       4).
-define(NDB_TYPE_MEDIUMINT,           5).
-define(NDB_TYPE_MEDIUMUNSIGNED,      6).
-define(NDB_TYPE_INT,                 7).
-define(NDB_TYPE_UNSIGNED,            8).
-define(NDB_TYPE_BIGINT,              9).
-define(NDB_TYPE_BIGUNSIGNED,        10).
-define(NDB_TYPE_FLOAT,              11).
-define(NDB_TYPE_DOUBLE,             12).
-define(NDB_TYPE_OLDDECIMAL,         13).
-define(NDB_TYPE_CHAR,               14).
-define(NDB_TYPE_VARCHAR,            15).
-define(NDB_TYPE_BINARY,             16).
-define(NDB_TYPE_VARBINARY,          17).
-define(NDB_TYPE_DATETIME,           18).
-define(NDB_TYPE_DATE,               19).
-define(NDB_TYPE_BLOB,               20).
-define(NDB_TYPE_TEXT,               21).
-define(NDB_TYPE_BIT,                22).
-define(NDB_TYPE_LONGVARCHAR,        23).
-define(NDB_TYPE_LONGVARBINARY,      24).
-define(NDB_TYPE_TIME,               25).
-define(NDB_TYPE_YEAR,               26).
-define(NDB_TYPE_TIMESTAMP,          27).
-define(NDB_TYPE_OLDDECIMALUNSIGNED, 28).
-define(NDB_TYPE_DECIMAL,            29).
-define(NDB_TYPE_DECIMALUNSIGNED,    30).
-define(NDB_TYPE_TIME2,              31).
-define(NDB_TYPE_DATETIME2,          32).
-define(NDB_TYPE_TIMESTAMP2,         33).
-define(NDB_TYPE_MAX,                34).

%% -- ~/include/kernel/signaldata/DictTabInfo.hpp --

%% enum DictTabInfo::KeyValues -> NdbTableImpl
-define(TABLE_NAME,                     1). % m_internalName, m_externalName, m_mysqlName?
-define(TABLE_ID,                       2). % m_id
-define(TABLE_VERSION,                  3). % m_version
-define(TABLE_LOGGED_FLAG,              4). % m_logging
-define(NO_OF_KEY_ATTR,                 5).
-define(NO_OF_ATTRIBUTES,               6). % >> attrDesc
-define(NO_OF_NULLABLE,                 7).
-define(NO_OF_VARIABLE,                 8).
-define(TABLE_KVALUE,                   9). % m_kvalue
-define(MIN_LOAD_FACTOR,               10). % m_minLoadFactor
-define(MAX_LOAD_FACTOR,               11). % m_maxLoadFactor
-define(KEY_LENGTH,                    12).
-define(FRAGMENT_TYPE,                 13). % m_fragmentType, m_hash_map, TODO
%%                                    14-17
-define(TABLE_TYPE,                    18). % m_indexType
-define(PRIMARY_TABLE,                 19). % m_primaryTable
-define(PRIMARY_TABLE_ID,              20).                     % m_primaryTableId
-define(INDEX_STATE,                   21).
-define(INSERT_TRIGGER_ID,             22).
-define(UPDATE_TRIGGER_ID,             23).
-define(DELETE_TRIGGER_ID,             24).
-define(CUSTOM_TRIGGER_ID,             25).
-define(FRM_LEN,                       26). % m_frm (1)
-define(FRM_DATA,                      27). % m_frm (2)
-define(TABLE_TEMPORARY_FLAG,          28). % m_temporary
-define(FORCE_VAR_PART_FLAG,           29). % m_force_var_part
%%                                   30-127
-define(FRAGMENT_COUNT,               128). % m_fragmentCount
-define(FRAGMENT_DATA_LEN,            129). % m_fd (1)
-define(FRAGMENT_DATA,                130). % m_fd (2)
-define(TABLESPACE_ID,                131).                      % m_tablespace_id(, m_tablespace_name)
-define(TABLESPACE_VERSION,           132).                      % m_tablespace_version
-define(TABLESPACE_DATA_LEN,          133).
-define(TABLESPACE_DATA,              134).
-define(RANGE_LIST_DATA_LEN,          135). % m_range (1)
-define(RANGE_LIST_DATA,              136). % m_range (2)
-define(REPLICA_DATA_LEN,             137). % >> REPLICA_DATA
-define(REPLICA_DATA,                 138). % m_replicaCount, m_fragmentCount, m_fragments,
%%                                            m_hashValueMask, m_hashpointerValue
-define(MAX_ROWS_LOW,                 139). % max_rows (1)
-define(MAX_ROWS_HIGH,                140). % max_rows (2)
-define(DEFAULT_NO_PART_FLAG,         141). % m_default_no_part_flag
-define(LINEAR_HASH_FLAG,             142). % m_linear_flag
-define(MIN_ROWS_LOW,                 143). % min_rows (1)
-define(MIN_ROWS_HIGH,                144). % min_rows (2)
%%                                  145-149
-define(ROW_GCI_FLAG,                 150). % m_row_gci
-define(ROW_CHECKSUM_FLAG,            151). % m_row_checksum
-define(SINGLE_USER_MODE,             152). % m_single_user_mode
-define(HASH_MAP_OBJECT_ID,           153). % m_hash_map_id
-define(HASH_MAP_VERSION,             154). % m_hash_map_version
-define(TABLE_STORAGE_TYPE,           155). % m_storageType
-define(EXTRA_ROW_GCI_BITS,           156). % m_extra_row_gci_bits
-define(EXTRA_ROW_AUTHOR_BITS,        157). % m_extra_row_author_bits
%%                                  158-998
-define(TABLE_END,                    999).

%% >> NdbColumnImpl = m_columns
-define(ATTRIBUTE_NAME,              1000). % m_name
-define(ATTRIBUTE_ID,                1001). % m_attrId
-define(ATTRIBUTE_TYPE,              1002).
-define(ATTRIBUTE_SIZE,              1003). % m_attrSize,m_orgAttrSize << ATTRIBUTE_EXT_TYPE
%%                                   1004
-define(ATTRIBUTE_ARRAY_SIZE,        1005). % m_arraySize              << ATTRIBUTE_EXT_TYPE
-define(ATTRIBUTE_KEY_FLAG,          1006). % m_pk
-define(ATTRIBUTE_STORAGE_TYPE,      1007). % m_storageType
-define(ATTRIBUTE_NULLABLE_FLAG,     1008). % m_nullable
-define(ATTRIBUTE_DYNAMIC,           1009). % m_dynamic
-define(ATTRIBUTE_DISTRIBUTION_KEY,  1010). % m_distributionKey
%%                                1011-1012
-define(ATTRIBUTE_EXT_TYPE,          1013). % m_type
-define(ATTRIBUTE_EXT_PRECISION,     1014). % m_precision & 0xffff, 0xffff0000 -> charset
-define(ATTRIBUTE_EXT_SCALE,         1015). % m_scale
-define(ATTRIBUTE_EXT_LENGTH,        1016). % m_length
-define(ATTRIBUTE_AUTO_INCREMENT,    1017). % m_autoIncrement, m_autoIncrementInitialValue
%%                                   1018
-define(ATTRIBUTE_ARRAY_TYPE,        1019). % m_arrayType              << ATTRIBUTE_EXT_TYPE
-define(ATTRIBUTE_DEFAULT_VALUE_LEN, 1020). % m_defaultValue (1)
-define(ATTRIBUTE_DEFAULT_VALUE,     1021). % m_defaultValue (2)
%%                                1022-1998
-define(ATTRIBUTE_END,               1999).

%% NdbTableImpl::buildColumnHash/0
%%   m_columnHashMask, m_columnHash
%%
%% NdbTableImpl::computeAggregates/0
%%   m_keyLenInWords, m_noOfAutoIncColumns
%%   m_noOfKeys, m_noOfDistributionKeys, m_noOfBlobs, m_noOfDiskColumns,
%%
%% NdbDictionaryImpl::createDefaultNdbRecord/2
%%  m_ndbrecord, m_pkMask

%% enum DictTabInfo::FragmentType
-define(FRAG_UNDEFINED,        0).
-define(FRAG_SINGLE,           1).
-define(FRAG_ALL_SMALL,        2).
-define(FRAG_ALL_MEDIUM,       3).
-define(FRAG_ALL_LARGE,        4).
-define(DISTR_KEY_HASH,        5).
-define(DISTR_KEY_LIN,         6).
-define(USER_DEFINED,          7).
%%                             8
-define(HASH_MAP_PARTITION,    9).

%% enum DictHashMapInfo::KeyValues
-define(HASH_MAP_NAME,         1).
-define(HASH_MAP_BUCKETS,      2).
-define(HASH_MAP_VALUES,       3).
%%efine(HASH_MAP_OBJECT_ID,  153). = DictTabInfo::KeyValues::*
%%efine(HASH_MAP_VERSION,    154). = DictTabInfo::KeyValues::*

%% -- ~/include/ndbepi/NdbDictionary.hpp --

%% enum NdbDictionary::Type
%%efine(TYPE_UNDEFINED,        0).
%%efine(SYSTEM_TABLE,          1).
%%efine(USER_TABLE,            2).
%%efine(UNIQUE_HASH_INDEX,     3).
%%efine(ORDERED_INDEX,         6).
%%efine(HASH_INDEX_TRIGGER,    7).
%%efine(INDEX_TRIGGER,         8).
%%efine(SUBSCRIPTION_TRIGGER,  9).
%%efine(READ_ONLY_CONSTRAINT, 10).
%%efine(TABLE_EVENT,          11).
%%efine(TABLESPACE,           20).
%%efine(LOGFILE_GROUP,        21).
%%efine(DATA_FILE,            22).
%%efine(UNDO_FILE,            23).
%%efine(REORG_TRIGGER,        19).
%%efine(HASH_MAP,             24).
%%efine(FOREIGN_KEY,          25).
%%efine(FK_PARENT_TRIGGER,    26).
%%efine(FK_CHILD_TRIGGER,     27).

fragments(3, _FragmentId, List) ->
    list_to_binary(lists:reverse(List));
fragments(1, FragmentId, List) ->
    receive
        #signal{gsn=?GSN_GET_TABINFOCONF,
                fragment_info=F,signal_data_length=D,no_of_sections=1,binary=B}
          when FragmentId =:= binary_part(B, ?WORD(D-1), ?WORD(1)) ->

            Fragment = binary_part(B, ?WORD(D+1), byte_size(B)-?WORD(D+1)),

            fragments(F, FragmentId, [Fragment|List])
    end.

unpack(_Binary, _Start, 0, List) ->
    lists:reverse(List);
unpack(Binary, Start, Length, List) ->
    %%
    %% @see
    %%   ~/include/util/SimpleProperties.hpp: SimpleProperties::ValueType
    %%   ~/src/common/util/SimpleProperties.cpp: SimpleProperties::Reader::readValue/0
    %%
    <<W:?WORD(1)/big-unit:8>> = binary_part(Binary, Start, ?WORD(1)), % << ntohl = big
    {T, N} = case {W band 16#ffff, W bsr 16} of
                 {K, 0} -> % Uint32Value
                     <<V:?WORD(1)/big-unit:8>> = binary_part(Binary, Start+?WORD(1), ?WORD(1)),
                     {{K,V}, ?WORD(2)};
                 {K, 1} -> % StringValue
                     <<L:?WORD(1)/big-unit:8>> = binary_part(Binary, Start+?WORD(1), ?WORD(1)),
                     {{K,binary_part(Binary,Start+?WORD(2),L-1)}, ?WORD(2+((L+3) bsr 2))};
                 {K, 2} -> % BinaryValue
                     <<L:?WORD(1)/big-unit:8>> = binary_part(Binary, Start+?WORD(1), ?WORD(1)),
                     {{K,binary_part(Binary,Start+?WORD(2),L)}, ?WORD(2+((L+3) bsr 2))}
             end,
    unpack(Binary, Start+N, Length-N, [T|List]).


find([H|T]) ->
    find(H, T).

find(P, []) ->
    P;
find(P, [H|T]) ->
    find(baseline_sup:find(P,H), T).

ref_to_tuple(R) ->
    {refToBlock(R),refToNode(R)}.


call(Pid, #signal{}=S, Sections) ->
    ndbepi_transporter:call(Pid, S#signal{
                                   byte_order = 0,
                                   signal_id_included = 0,
                                   compression = 0,
                                   checksum_included = 0}, Sections).

numberToRef(Block, Node) ->
    ndbepi_protocol:numberToRef(Block, Node).

refToBlock(Ref) ->
    ndbepi_protocol:refToBlock(Ref).

refToNode(Ref) ->
    ndbepi_protocol:refToNode(Ref).

binary_to_word(Binary, Start, ByteOrder) ->
    ndbepi_protocol:binary_to_word(Binary, Start, ByteOrder).

binary_to_words(Binary, Start, ByteOrder) ->
    ndbepi_protocol:binary_to_words(binary_part(Binary,Start,byte_size(Binary)-Start), ByteOrder).
