-module(ndbepi_attr).

%% -- ~/include/kernel/signaldata/TcKeyReq.hpp --

%%
%% Request Info
%%  b : Distribution Key Indicator  1
%%  c : Commit Indicator            1
%%  d : Dirty Indicator             1
%%  e : Scan Indicator              1
%%  i : Interpreted Indicator       1
%%  l : Execute                     1
%%  n : No disk flag                1
%%  o : Operation Type              3
%%  p : Simple Indicator            1
%%  q : Queue on redo problem       1
%%  r : reorg flag                  1
%%  s : Start Indicator             1
%%  v : Via SPJ                     1
%%  x : Coordinated Tx flag         1
%%  y : Commit Type                 2
%%  D : Deferred constraint         1
%%  f : Disable FK constraint       1
%%  R : Read Committed base         1
%%
%% Protocol6  3          2          1          0
%%           10987654 32109876 54321098 76543210
%%  - Word - ........ ...RrfDx ieyyslqp ooocvbnd
%%
-define(WORD_MASK_DIRTY_INDICATOR,            16#00000001).
-define(WORD_MASK_NO_DISK_FLAG,               16#00000002).
-define(WORD_MASK_DISTRIBUTION_KEY_INDICATOR, 16#00000004).
-define(WORD_MASK_VIA_SPJ,                    16#00000008).
-define(WORD_MASK_COMMIT_INDICATOR,           16#00000010).
-define(WORD_MASK_OPERATION_TYPE,             16#000000e0).
-define(WORD_MASK_SIMPLE_INDICATOR,           16#00000100).
-define(WORD_MASK_QUEUE_ON_REDO_PROBLEM,      16#00000200).
-define(WORD_MASK_EXECUTE,                    16#00000400).
-define(WORD_MASK_START_INDICATOR,            16#00000800).
-define(WORD_MASK_COMMIT_TYPE,                16#00003000).
-define(WORD_MASK_SCAN_INDICATOR,             16#00004000).
-define(WORD_MASK_INTERPRETED_INDICATOR,      16#00008000).
-define(WORD_MASK_COORDINATED_TX_FLAG,        16#00010000).
-define(WORD_MASK_DEFERRED_CONSTRAINT,        16#00020000).
-define(WORD_MASK_DISABLE_FK_CONSTRAINT,      16#00040000).
-define(WORD_MASK_REORG_FLAG,                 16#00080000).
-define(WORD_MASK_READ_COMMITTED_BASE,        16#00100000).

-define(WORD_SHIFT_DIRTY_INDICATOR,            0).
-define(WORD_SHIFT_NO_DISK_FLAG,               1).
-define(WORD_SHIFT_DISTRIBUTION_KEY_INDICATOR, 2).
-define(WORD_SHIFT_VIA_SPJ,                    3).
-define(WORD_SHIFT_COMMIT_INDICATOR,           4).
-define(WORD_SHIFT_OPERATION_TYPE,             5).
-define(WORD_SHIFT_SIMPLE_INDICATOR,           8).
-define(WORD_SHIFT_QUEUE_ON_REDO_PROBLEM,      9).
-define(WORD_SHIFT_EXECUTE,                   10).
-define(WORD_SHIFT_START_INDICATOR,           11).
-define(WORD_SHIFT_COMMIT_TYPE,               12).
-define(WORD_SHIFT_SCAN_INDICATOR,            14).
-define(WORD_SHIFT_INTERPRETED_INDICATOR,     15).
-define(WORD_SHIFT_COORDINATED_TX_FLAG,       16).
-define(WORD_SHIFT_DEFERRED_CONSTRAINT,       17).
-define(WORD_SHIFT_DISABLE_FK_CONSTRAINT,     18).
-define(WORD_SHIFT_REORG_FLAG,                19).
-define(WORD_SHIFT_READ_COMMITTED_BASE,       20).
