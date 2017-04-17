-ifndef(ndbepi).
-define(ndbepi, true).

%% == define ==

%% -- ~/include/kernel/signaldata/DictTabInfo.hpp --

%% enum DictTabInfo::KeyValues              % NdbTableImpl
-define(TABLE_NAME,                     1). % m_internalName
-define(TABLE_ID,                       2). % m_id
-define(TABLE_VERSION,                  3). % m_version
-define(TABLE_LOGGED_FLAG,              4). % m_logging
-define(NO_OF_KEY_ATTR,                 5).
-define(NO_OF_ATTRIBUTES,               6).
-define(NO_OF_NULLABLE,                 7).
-define(NO_OF_VARIABLE,                 8).
-define(TABLE_K_VALUE,                  9). % m_kvalue
-define(MIN_LOAD_FACTOR,               10). % m_minLoadFactor
-define(MAX_LOAD_FACTOR,               11). % m_maxLoadFactor
-define(KEY_LENGTH,                    12).
-define(FRAGMENT_TYPE,                 13). % m_fragmentType
%%                                    14-17
-define(TABLE_TYPE,                    18).
-define(PRIMARY_TABLE,                 19). % m_primaryTable
-define(PRIMARY_TABLE_ID,              20).
-define(INDEX_STATE,                   21).
-define(INSERT_TRIGGER_ID,             22).
-define(UPDATE_TRIGGER_ID,             23).
-define(DELETE_TRIGGER_ID,             24).
-define(CUSTOM_TRIGGER_ID,             25).
-define(FRM_LEN,                       26). % m_frm (1/2)
-define(FRM_DATA,                      27). % m_frm (2/2)
-define(TABLE_TEMPORARY_FLAG,          28). % m_temporary
-define(FORCE_VAR_PART_FLAG,           29). % m_force_var_part
%%                                   30-126
-define(PARTITION_BALANCE,            127). % m_partitionBalance
-define(FRAGMENT_COUNT,               128). % m_fragmentCount
-define(FRAGMENT_DATA_LEN,            129). % m_fd (1/2)
-define(FRAGMENT_DATA,                130). % m_fd (2/2)
-define(TABLESPACE_ID,                131).
-define(TABLESPACE_VERSION,           132).
-define(TABLESPACE_DATA_LEN,          133).
-define(TABLESPACE_DATA,              134).
-define(RANGE_LIST_DATA_LEN,          135). % m_range (1/2)
-define(RANGE_LIST_DATA,              136). % m_range (2/2)
-define(REPLICA_DATA_LEN,             137).
-define(REPLICA_DATA,                 138). % m_replicaCount, m_fragmentCount
-define(MAX_ROWS_LOW,                 139). % m_max_rows (1/2)
-define(MAX_ROWS_HIGH,                140). % m_max_rows (2/2)
-define(DEFAULT_NO_PART_FLAG,         141). % m_default_no_part_flag
-define(LINEAR_HASH_FLAG,             142). % m_linear_flag
-define(MIN_ROWS_LOW,                 143). % m_min_rows (1/2)
-define(MIN_ROWS_HIGH,                144). % m_min_rows (2/2)
%%                                  145-149
-define(ROW_GCI_FLAG,                 150). % m_row_gci
-define(ROW_CHECKSUM_FLAG,            151). % m_row_checksum
-define(SINGLE_USER_MODE,             152). % m_single_user_mode
-define(HASH_MAP_OBJECT_ID,           153). % m_hash_map_id
-define(HASH_MAP_VERSION,             154). % m_hash_map_version
-define(TABLE_STORAGE_TYPE,           155). % m_storageType
-define(EXTRA_ROW_GCI_BITS,           156). % m_extra_row_gci_bits
-define(EXTRA_ROW_AUTHOR_BITS,        157). % m_extra_row_author_bits
-define(READ_BACKUP_FLAG,             158). % m_read_backup
-define(FULLY_REPLICATED_FLAG,        159). % m_fully_replicated
-define(PARTITION_COUNT,              160). % m_partitionCount
-define(FULLY_REPLICATED_TRIGGER_ID,  161).
%%                                  162-998
-define(TABLE_END,                    999).
%%                                          % NdbColumnImpl
-define(ATTRIBUTE_NAME,              1000). % m_name
-define(ATTRIBUTE_ID,                1001). % m_attrId
-define(ATTRIBUTE_TYPE,              1002).
-define(ATTRIBUTE_SIZE,              1003). % m_orgAttrSize, m_attrSize
%%                                   1004
-define(ATTRIBUTE_ARRAY_SIZE,        1005). % m_arraySize
-define(ATTRIBUTE_KEY_FLAG,          1006). % m_pk
-define(ATTRIBUTE_STORAGE_TYPE,      1007). % m_storageType
-define(ATTRIBUTE_NULLABLE_FLAG,     1008). % m_nullable
-define(ATTRIBUTE_DYNAMIC,           1009). % m_dynamic
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

-endif. % ndbepi
