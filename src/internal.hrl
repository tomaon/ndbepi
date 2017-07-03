-ifndef(internal).
-define(internal, true).

%%nclude_lib("baseline/include/baseline.hrl").
-include_lib("../.rebar3/default/lib/baseline/include/baseline.hrl").

%%nclude_lib("mgmepi/include/mgmepi.hrl").
-include_lib("../.rebar3/default/lib/mgmepi/include/mgmepi.hrl").

%% == define ==

%%
%% https://dev.mysql.com/doc/ndb-internals/en/ndb-internals-ndb-protocol-messages.html
%%
%% NDB message = signal
%%  1) request : REQ
%%  1-1) data requests <  KEYINFO (key), ATTRINFO (nokey)
%%  1-1-1) primary key lookup operations  : TCKEY
%%  1-1-2) unique key lookup operations   : TCINDEX
%%  1-1-3) table or index scan operations : SCANTAB, SCAN_NEXT
%%  1-2) transactional requests
%%  1-2-1) commits and rollbacks          : TC_COMMIT, TCROLLBACK
%%  1-2-2) transaction record requests    : TCSEIZE, TCRELEASE
%%  2) response
%%  2-1) success : CONF (confirmed)       + TRANSID_AI (resultset)
%%  2-2) failure : REF  (refused)
%%  2-?) ?       : REP  (?)
%%

%% -- ~/include/kernel/BlockNumbers.h --
%%efine(BACKUP,           16#00f4). % 243: online BACKUPs and checkpoints
-define(DBTC,             16#00f5). % 245: Transaction Coordinator
%%efine(DBDIH,            16#00f6). % 246: DIstribution Handler, local and global checkpoints
-define(DBLQH,            16#00f7). % 247: Local Query Handler, coordinator of 2-phase commits
%%efine(DBACC,            16#00f8). % 248: ACCess control and lock management
%%efine(DBTUP,            16#00f9). % 249: TUPle manager, physical storage
-define(DBDICT,           16#00fa). % 250: data DICTionary
%%efine(DBCNTR,           16#00fb). % 251: startup CoordiNaToR, initialisation and configuration
-define(QMGR,             16#00fc). % 252: Queue ManaGeR
%%efine(NDBFS,            16#00fd). % 253: NDB File System abstraction layer
%%efine(CMVMI,            16#00fe). % 254: Cluster Manager Virtual Machine Interface
%%efine(TRIX,             16#00ff). % 255: TRansaction and IndeXes, triggers and unique indexes
%%efine(DBUTIL,           16#0100). % 256: provides internal interfaces
%%efine(SUMA,             16#0101). % 257: cluster SUbscription MAnager, event logging and reporting
%%efine(DBTUX,            16#0102). % 258: ?, ordered indexes
%%efine(TSMAN,            16#0103). % 259: TableSpace MANager
%%efine(LGMAN,            16#0104). % 260: Log Group MANager, undo logs
%%efine(PGMAN,            16#0105). % 261: PaGe and buffer MANagement services
%%efine(RESTORE,          16#0106). % 262: restoration from online backups
%%efine(DBINFO,           16#0107). % 263: ?, ndbinfo
%%efine(DBSPJ,            16#0108). % 264: ?, multiple cursors, pushed-down joins
%%efine(THRMAN,           16#0109). % 265: THRead MANagement
%%efine(TRPMAN,           16#010a). % 266: signal TRansPort MANagement

%%efine(API_PACKED,       16#07ff). % 2047

-define(API_CLUSTERMGR,   16#0fa2). % 4002
%%efine(MGM_CONFIG_MAN,   16#0fa3). % 4003

-define(MIN_API_BLOCK_NO, 16#8000). % 32768

%% -- ~/include/kernel/GlobalSignalNumbers.h --
-define(GSN_API_REGCONF,                    1).
-define(GSN_API_REGREF,                     2).
-define(GSN_API_REGREQ,                     3).
-define(GSN_GET_TABINFO_CONF,             190).
-define(GSN_GET_TABINFOREF,                23).
-define(GSN_GET_TABINFOREQ,                24).
-define(GSN_LQHKEYCONF,                   314).
-define(GSN_LQHKEYREF,                    315).
-define(GSN_LQHKEYREQ,                    316).
-define(GSN_TC_COMMIT_ACK,                469). % -> GSN_TCRELEASECONF ?!
%%efine(GSN_TC_COMMITCONF,                 17).
%%efine(GSN_TC_COMMITREF,                  18).
%%efine(GSN_TC_COMMITREQ,                  19).
%%efine(GSN_TCKEYCONF,                     10).
%%efine(GSN_TCKEYREF,                      11).
%%efine(GSN_TCKEYREQ,                      12).
-define(GSN_TCRELEASECONF,                 34).
-define(GSN_TCRELEASEREF,                  35).
-define(GSN_TCRELEASEREQ,                  36).
%%efine(GSN_TCROLLBACKCONF,                13).
%%efine(GSN_TCROLLBACKREF,                 14).
%%efine(GSN_TCROLLBACKREQ,                 15).
-define(GSN_TCSEIZECONF,                   37).
-define(GSN_TCSEIZEREF,                    38).
-define(GSN_TCSEIZEREQ,                    39).
%%efine(GSN_TRANSID_AI,                     5).

-define(MAX_GSN,                          782).

%% -- other --
-define(API_DOCTIONARY,   (?MIN_API_BLOCK_NO + 1)). % 32769

-define(MAX_API_BLOCK_NO, 16#ffff).

-define(LS, $\n).

-define(BYTE(N), ((N) bsl 2)).
-define(WORD(N), ((N) bsr 2)).

-define(NUMBER_TO_REF(Block, Node), ((Block bsl 16) bor Node)).

-define(REF_TO_BLOCK(Ref), (Ref bsr 16)).

-define(REF_TO_NODE(Ref), (Ref band 16#0000ffff)).

-define(NUMBER_TO_INDEX(Number), (Number - ?MIN_API_BLOCK_NO)).

-define(INDEX_TO_NUMBER(Index), (Index + ?MIN_API_BLOCK_NO)).

%% == record ==

-record(signal,
        {
          gsn                    :: undefined|1 .. ?MAX_GSN,
          send_node_id           :: undefined|node_id(),
          send_block_no          :: undefined|block_no(),
          recv_node_id           :: undefined|node_id(),
          recv_block_no          :: undefined|block_no(),
          byte_order             :: 0|1,                         % < mgmepi
          checksum_included      :: 0|1,                         % < mgmepi
          signal_id_included     :: 0|1,                         % < mgmepi
          compressed = 0         :: 0,
          message_length = 0     :: non_neg_integer(),
          fragment_info = 0      :: non_neg_integer(),
          prio = 1               :: non_neg_integer(),           % 1=JBB
          version_id = 0         :: non_neg_integer(),
          trace = 1              :: 1,                           % 1=TraceAPI
          signal_data_length = 0 :: non_neg_integer(),
          signal_data = []       :: [integer()],
          sections_length = 0    :: non_neg_integer(),
          signal_id = 0          :: non_neg_integer()
        }).

%% == type ==

-type(block_no() :: pos_integer()).

-type(signal() :: #signal{}).

-endif. % internal
