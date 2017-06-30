-ifndef(ndbepi).
-define(ndbepi, true).

%% == define ==

%% ndb->init() -> block_no

%% -- ~/include/kernel/signaldata/DictTabInfo.hpp --

%% enum DictTabInfo::RequestType
%%efine(CREATE_TABLE_FROM_API,   1).
%%efine(ADD_TABLE_FROM_DICT,     2). % Between DICT's
%%efine(COPY_TABLE,              3). % Between DICT's
%%efine(READ_TABLE_FROM_DISK_SR, 4). % Local in DICT
%%efine(GET_TAB_INFO_CONF,       5).
%%efine(ALTER_TABLE_FROM_API,    6).

%% NdbDictionaryImpl.cpp : NdbDictInterface::parseTableInfo/5

%% enum DictTabInfo::KeyValues              % NdbTableImpl
%%efine(TABLE_NAME,                     1). % m_internalName
%%efine(TABLE_ID,                       2). % m_id
-define(TABLE_VERSION,                  3). % m_version
-define(TABLE_LOGGED_FLAG,              4). % m_logging
%%efine(NO_OF_KEY_ATTR,                 5).
%%efine(NO_OF_ATTRIBUTES,               6).
%%efine(NO_OF_NULLABLE,                 7).
%%efine(NO_OF_VARIABLE,                 8).
-define(TABLE_K_VALUE,                  9). % m_kvalue
-define(MIN_LOAD_FACTOR,               10). % m_minLoadFactor
-define(MAX_LOAD_FACTOR,               11). % m_maxLoadFactor
%%efine(KEY_LENGTH,                    12).
-define(FRAGMENT_TYPE,                 13). % m_fragmentType
%%                                    14-17
%%efine(TABLE_TYPE,                    18).
%%efine(PRIMARY_TABLE,                 19). % m_primaryTable
%%efine(PRIMARY_TABLE_ID,              20).
%%efine(INDEX_STATE,                   21).
%%efine(INSERT_TRIGGER_ID,             22).
%%efine(UPDATE_TRIGGER_ID,             23).
%%efine(DELETE_TRIGGER_ID,             24).
%%efine(CUSTOM_TRIGGER_ID,             25).
-define(FRM_LEN,                       26). % m_frm (1/2)
-define(FRM_DATA,                      27). % m_frm (2/2)
%%efine(TABLE_TEMPORARY_FLAG,          28). % m_temporary
-define(FORCE_VAR_PART_FLAG,           29). % m_force_var_part
%%                                   30-126
-define(PARTITION_BALANCE,            127). % m_partitionBalance
-define(FRAGMENT_COUNT,               128). % m_fragmentCount
%%efine(FRAGMENT_DATA_LEN,            129). % m_fd (1/2)
%%efine(FRAGMENT_DATA,                130). % m_fd (2/2)
%%efine(TABLESPACE_ID,                131).
%%efine(TABLESPACE_VERSION,           132).
%%efine(TABLESPACE_DATA_LEN,          133).
%%efine(TABLESPACE_DATA,              134).
%%efine(RANGE_LIST_DATA_LEN,          135). % m_range (1/2)
%%efine(RANGE_LIST_DATA,              136). % m_range (2/2)
%%efine(REPLICA_DATA_LEN,             137).
%%efine(REPLICA_DATA,                 138). % m_replicaCount, m_fragmentCount
-define(MAX_ROWS_LOW,                 139). % m_max_rows (1/2)
-define(MAX_ROWS_HIGH,                140). % m_max_rows (2/2)
%%efine(DEFAULT_NO_PART_FLAG,         141). % m_default_no_part_flag
%%efine(LINEAR_HASH_FLAG,             142). % m_linear_flag
%%efine(MIN_ROWS_LOW,                 143). % m_min_rows (1/2)
%%efine(MIN_ROWS_HIGH,                144). % m_min_rows (2/2)
%%                                  145-149
-define(ROW_GCI_FLAG,                 150). % m_row_gci
-define(ROW_CHECKSUM_FLAG,            151). % m_row_checksum
-define(SINGLE_USER_MODE,             152). % m_single_user_mode
%%efine(HASH_MAP_OBJECT_ID,           153). % m_hash_map_id
%%efine(HASH_MAP_VERSION,             154). % m_hash_map_version
%%efine(TABLE_STORAGE_TYPE,           155). % m_storageType
-define(EXTRA_ROW_GCI_BITS,           156). % m_extra_row_gci_bits
-define(EXTRA_ROW_AUTHOR_BITS,        157). % m_extra_row_author_bits
-define(READ_BACKUP_FLAG,             158). % m_read_backup
-define(FULLY_REPLICATED_FLAG,        159). % m_fully_replicated
-define(PARTITION_COUNT,              160). % m_partitionCount
%%efine(FULLY_REPLICATED_TRIGGER_ID,  161).
%%                                  162-998
%%efine(TABLE_END,                    999).
%%                                          % NdbColumnImpl
-define(ATTRIBUTE_NAME,              1000). % m_name
-define(ATTRIBUTE_ID,                1001). % m_attrId
%%efine(ATTRIBUTE_TYPE,              1002).
-define(ATTRIBUTE_SIZE,              1003). % m_orgAttrSize, m_attrSize (0|3|4|5|6|7)
%%                                   1004
-define(ATTRIBUTE_ARRAY_SIZE,        1005). % m_arraySize
-define(ATTRIBUTE_KEY_FLAG,          1006). % m_pk
-define(ATTRIBUTE_STORAGE_TYPE,      1007). % m_storageType
-define(ATTRIBUTE_NULLABLE_FLAG,     1008). % m_nullable
%%efine(ATTRIBUTE_DYNAMIC,           1009). % m_dynamic, deprecated
-define(ATTRIBUTE_D_KEY,             1010). % m_distributionKey
%%                                1011-1012
-define(ATTRIBUTE_EXT_TYPE,          1013). % m_type
-define(ATTRIBUTE_EXT_PRECISION,     1014). % m_precision, m_cs
-define(ATTRIBUTE_EXT_SCALE,         1015). % m_scale
-define(ATTRIBUTE_EXT_LENGTH,        1016). % m_length
-define(ATTRIBUTE_AUTO_INCREMENT,    1017). % m_autoIncrement
%%                                   1018
-define(ATTRIBUTE_ARRAY_TYPE,        1019). % m_arrayType
-define(ATTRIBUTE_DEFAULT_VALUE_LEN, 1020). % m_defaultValue (1/2)
-define(ATTRIBUTE_DEFAULT_VALUE,     1021). % m_defaultValue (2/2)
%%                                1022-1998
-define(ATTRIBUTE_END,               1999).

%% -- ~/include/ndb_constants.h --
-define(NDB_TYPE_UNDEFINED,           0). % Undefined
-define(NDB_TYPE_TINYINT,             1). % Tinyint
-define(NDB_TYPE_TINYUNSIGNED,        2). % Tinyunsigned
-define(NDB_TYPE_SMALLINT,            3). % Smallint
-define(NDB_TYPE_SMALLUNSIGNED,       4). % Smallunsigned
-define(NDB_TYPE_MEDIUMINT,           5). % Mediumint
-define(NDB_TYPE_MEDIUMUNSIGNED,      6). % Mediumunsigned
-define(NDB_TYPE_INT,                 7). % Int
-define(NDB_TYPE_UNSIGNED,            8). % Unsigned
-define(NDB_TYPE_BIGINT,              9). % Bigint
-define(NDB_TYPE_BIGUNSIGNED,        10). % Bigunsigned
-define(NDB_TYPE_FLOAT,              11). % Float
-define(NDB_TYPE_DOUBLE,             12). % Double
-define(NDB_TYPE_OLDDECIMAL,         13). % Olddecimal, precision scale
-define(NDB_TYPE_CHAR,               14). % Char, length csname
-define(NDB_TYPE_VARCHAR,            15). % Varchar, length csname
-define(NDB_TYPE_BINARY,             16). % Binary, length
-define(NDB_TYPE_VARBINARY,          17). % Varbinary, length
-define(NDB_TYPE_DATETIME,           18). % Datetime
-define(NDB_TYPE_DATE,               19). % Date
-define(NDB_TYPE_BLOB,               20). % Blob, inline_size part_size stripe_size
-define(NDB_TYPE_TEXT,               21). % Text, inline_size part_size stripe_size csname
-define(NDB_TYPE_BIT,                22). % Bit, length
-define(NDB_TYPE_LONGVARCHAR,        23). % Longvarchar, length
-define(NDB_TYPE_LONGVARBINARY,      24). % Longvarbinary, length
-define(NDB_TYPE_TIME,               25). % Time
-define(NDB_TYPE_YEAR,               26). % Year
-define(NDB_TYPE_TIMESTAMP,          27). % Timestamp
-define(NDB_TYPE_OLDDECIMALUNSIGNED, 28). % Olddecimalunsigned, precision scale
-define(NDB_TYPE_DECIMAL,            29). % Decimal, precision scale
-define(NDB_TYPE_DECIMALUNSIGNED,    30). % Decimalunsigned, precision scale
-define(NDB_TYPE_TIME2,              31). % Time2, precision
-define(NDB_TYPE_DATETIME2,          32). % Datetime2, precision
-define(NDB_TYPE_TIMESTAMP2,         33). % Timestamp2, precision
%%efine(NDB_TYPE_MAX,                34).

%% -- ~/include/ndbepi/NdbDictionary.hpp --

%% NdbDictionary
%%   Table
%%   Column
%%   Index
%%   Event?
%%   LogfileGroup
%%   Tablespace
%%   Datafile
%%   Undofile
%%   HashMap
%%   ForeignKey
%%   Dictionary? (List-Element)

%% enum NdbDictionary::Type          enum DictTabInfo::TableType
-define(TYPE_UNDEFINED,        0).   % UndefTableType
-define(SYSTEM_TABLE,          1).   % =
-define(USER_TABLE,            2).   % =
-define(UNIQUE_HASH_INDEX,     3).   % =
%%                             4     % HashIndex
%%                             5     % UniqueOrderedIndex
-define(ORDERED_INDEX,         6).   % =
-define(HASH_INDEX_TRIGGER,    7).   % 16?
-define(INDEX_TRIGGER,         8).   % 18?
-define(SUBSCRIPTION_TRIGGER,  9).   % 16?
-define(READ_ONLY_CONSTRAINT, 10).   % 17?
-define(TABLE_EVENT,          11).   % HashIndexTrigger
%%                            16     % SubscriptionTrigger
%%                            17     % ReadOnlyConstraint
%%                            18     % IndexTrigger
-define(REORG_TRIGGER,        19).   % =
-define(TABLESPACE,           20).   % =
-define(LOGFILE_GROUP,        21).   % =
-define(DATAFILE,             22).   % =
-define(UNDOFILE,             23).   % =
-define(HASH_MAP,             24).   % =
-define(FOREIGN_KEY,          25).   % =
-define(FK_PARENT_TRIGGER,    26).   % =
-define(FK_CHILD_TRIGGER,     27).   % =
%%                            28     % FullyReplicatedTrigger
%%                            30     % SchemaTransaction

%% enum NdbDictionary::State         enum DictTabInfo::ObjectState
%%efine(STATE_UNDEFINED,       0).   % =
%%efine(STATE_OFFLINE,         1).   % =
%%efine(STATE_BUILDING,        2).   % =
%%efine(STATE_DROPPING,        3).   % =
%%efine(STATE_ONLINE,          4).   % =
%%efine(STATE_BACKUP,          5).   % =
%%efine(STATE_BROKEN,          9).   % =

%% enum NdbDictionary:Store          enum DictTabInfo::ObjectStore
%%efine(STORE_UNDEFINED,       0).   % =
%%efine(STORE_NOT_LOGGED,      1).   % =
%%efine(STORE_PERMANENT,       2).   % =

%% enum NdbDictionary:FragmentType   enum DictTabInfo::FragmentType
-define(FRAG_UNDEFINED,        0).   % AllNodesSmallTable
-define(FRAG_SINGLE,           1).   % AllNodesMediumTable
-define(FRAG_ALL_SMALL,        2).   % AllNodesLargeTable
-define(FRAG_ALL_MEDIUM,       3).   % SingleFragment
-define(FRAG_ALL_LARGE,        4).   % DistrKeyHash
-define(DISTR_KEY_HASH,        5).   % DistrKeyLin
-define(DISTR_KEY_LIN,         6).   % UserDefined
-define(USER_DEFINED,          7).
%%                             8     % DistrKeyOrderedIndex
-define(HASH_MAP_PARTITION,    9).   % =

%% enum NdbDictionary::PartitionBalance

%% enum Column::Type = Attribute
%% Undefined = NDB_TYPE_UNDEFINED,
%% Tinyint = NDB_TYPE_TINYINT,
%% Tinyunsigned = NDB_TYPE_TINYUNSIGNED,
%% Smallint = NDB_TYPE_SMALLINT,
%% Smallunsigned = NDB_TYPE_SMALLUNSIGNED,
%% Mediumint = NDB_TYPE_MEDIUMINT,
%% Mediumunsigned = NDB_TYPE_MEDIUMUNSIGNED,
%% Int = NDB_TYPE_INT,
%% Unsigned = NDB_TYPE_UNSIGNED,
%% Bigint = NDB_TYPE_BIGINT,
%% Bigunsigned = NDB_TYPE_BIGUNSIGNED,
%% Float = NDB_TYPE_FLOAT,
%% Double = NDB_TYPE_DOUBLE,
%% Olddecimal = NDB_TYPE_OLDDECIMAL,
%% Olddecimalunsigned = NDB_TYPE_OLDDECIMALUNSIGNED,
%% Decimal = NDB_TYPE_DECIMAL,
%% Decimalunsigned = NDB_TYPE_DECIMALUNSIGNED,
%% Char = NDB_TYPE_CHAR,
%% Varchar = NDB_TYPE_VARCHAR,
%% Binary = NDB_TYPE_BINARY,
%% Varbinary = NDB_TYPE_VARBINARY,
%% Datetime = NDB_TYPE_DATETIME,
%% Date = NDB_TYPE_DATE,
%% Blob = NDB_TYPE_BLOB,
%% Text = NDB_TYPE_TEXT,
%% Bit = NDB_TYPE_BIT,
%% Longvarchar = NDB_TYPE_LONGVARCHAR,
%% Longvarbinary = NDB_TYPE_LONGVARBINARY,
%% Time = NDB_TYPE_TIME,
%% Year = NDB_TYPE_YEAR,
%% Timestamp = NDB_TYPE_TIMESTAMP,
%% Time2 = NDB_TYPE_TIME2,
%% Datetime2 = NDB_TYPE_DATETIME2,
%% Timestamp2 = NDB_TYPE_TIMESTAMP2

%% enum Column::ArrayType
%% ArrayTypeFixed = NDB_ARRAYTYPE_FIXED,
%% ArrayTypeShortVar = NDB_ARRAYTYPE_SHORT_VAR,
%% ArrayTypeMediumVar = NDB_ARRAYTYPE_MEDIUM_VAR

%% enum Column::StorageType
%% StorageTypeMemory = NDB_STORAGETYPE_MEMORY,
%% StorageTypeDisk = NDB_STORAGETYPE_DISK,
%% StorageTypeDefault = NDB_STORAGETYPE_DEFAULT

%% enum Event::TableEvent, EventDurability, EventReport


%% enum DictTabInfo::ExtType

%% enum DictFilegroupInfo::KeyValues, FileTypeValues

%% enum DictHashMapInfo::KeyValues
-define(HASH_MAP_NAME,         1).
-define(HASH_MAP_BUCKETS,      2).
-define(HASH_MAP_VALUES,       3).
%%efine(HASH_MAP_OBJECT_ID,  153). = DictTabInfo::KeyValues::*
%%efine(HASH_MAP_VERSION,    154). = DictTabInfo::KeyValues::*

%% DictForeignKeyInfo::KeyValues

-endif. % ndbepi
