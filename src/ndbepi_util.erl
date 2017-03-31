-module(ndbepi_util).

-include("internal.hrl").

%% -- private --
-export([pack/2, unpack/2]).
-export([checksum/2]).
-export([number_to_ref/2, ref_to_block/1, ref_to_node/1]).
-export([number_to_index/1, index_to_number/1]).
-export([binary_to_word/3, binary_to_words/2,
         word_to_binary/2, words_to_binary/2]).
-export([sections_to_words/2]).
-export([find/2]).

%% -- internal --

%%
%% ~/src/common/transporter/Packer.(hpp|cpp)
%% ~/src/common/transporter/TransporterInternalDefinitions.hpp
%%

%%  b : Byte order        ,  1 * 3(4?, TODO) = 1(big), 0(little)
%%  c : Checksum included ,  1
%%  d : Signal data length,  5
%%  f : FragmentInfo1     ,  1 (FragmentInfo & 2)
%%  g : GSN               , 16
%%  h : FragmentInfo2     ,  1 (FragmentInfo & 1)
%%  i : Signal id included,  1
%%  m : Message length    , 16
%%  n : No of segments    ,  2
%%  p : Prio              ,  2 = 1(JBB)
%%  r : Recievers block no, 16
%%  s : Senders block no  , 16
%%  t : Trace             ,  6
%%  v : Version id        ,  4
%% (z): Compression       ,  1 = 0
%%
%% Protocol6  3          2          1          0
%%           10987654 32109876 54321098 76543210
%% - Word1 - 0dddddhb mmmmmmmm mmmmmmmm bppczifb
%%           00000000 00000000 00000000 00000000  0x00000000
%% - Word2 - ....nntt ttttvvvv gggggggg gggggggg
%%           00000000 00000000 00000000 00000000  0x00000000
%% - Word3 - rrrrrrrr rrrrrrrr ssssssss ssssssss
%%           00000000 00000000 00000000 00000000  0x00000000

-define(WORD1_MASK_BYTE_ORDER_1,        16#00000001).
-define(WORD1_MASK_FRAGMENT_INFO_1,     16#00000002).
-define(WORD1_MASK_SIGNAL_ID_INCLUDED,  16#00000004).
-define(WORD1_MASK_COMPRESSED,          16#00000008).
-define(WORD1_MASK_CHECKSUM_INCLUDED,   16#00000010).
-define(WORD1_MASK_PRIO,                16#00000060).
-define(WORD1_MASK_BYTE_ORDER_2,        16#00000080).
-define(WORD1_MASK_MESSAGE_LENGTH,      16#00ffff00).
-define(WORD1_MASK_BYTE_ORDER_3,        16#01000000).
-define(WORD1_MASK_FRAGMENT_INFO_2,     16#02000000).
-define(WORD1_MASK_SIGNAL_DATA_LENGTH,  16#7c000000).
-define(WORD1_MASK_BYTE_ORDER_4,        16#00000000). % !=16#80000000

-define(WORD1_SHIFT_BYTE_ORDER_1,                 0).
-define(WORD1_SHIFT_FRAGMENT_INFO_1,              0). % !=1
-define(WORD1_SHIFT_SIGNAL_ID_INCLUDED,           2).
-define(WORD1_SHIFT_COMPRESSED,                   3).
-define(WORD1_SHIFT_CHECKSUM_INCLUDED,            4).
-define(WORD1_SHIFT_PRIO,                         5).
-define(WORD1_SHIFT_BYTE_ORDER_2,                 7).
-define(WORD1_SHIFT_MESSAGE_LENGTH,               8).
-define(WORD1_SHIFT_BYTE_ORDER_3,                24).
-define(WORD1_SHIFT_FRAGMENT_INFO_2,             25).
-define(WORD1_SHIFT_SIGNAL_DATA_LENGTH,          26).
-define(WORD1_SHIFT_BYTE_ORDER_4,                31).

-define(WORD2_MASK_GSN,                 16#0000ffff).
-define(WORD2_MASK_VERSION_ID,          16#000f0000).
-define(WORD2_MASK_TRACE,               16#03f00000).
-define(WORD2_MASK_NO_OF_SECTIONS,      16#0c000000).

-define(WORD2_SHIFT_GSN,                          0).
-define(WORD2_SHIFT_VERSION_ID,                  16).
-define(WORD2_SHIFT_TRACE,                       20).
-define(WORD2_SHIFT_NO_OF_SECTIONS,              26).

-define(WORD3_MASK_SEND_BLOCK_NO,       16#0000ffff).
-define(WORD3_MASK_RECV_BLOCK_NO,       16#ffff0000).

-define(WORD3_SHIFT_SEND_BLOCK_NO,                0).
-define(WORD3_SHIFT_RECV_BLOCK_NO,               16).

-type(byte_order() :: 0|1).

%% == private ==

-spec pack(signal(), [term()]) -> binary().
pack(#signal{byte_order=B, checksum_included=C, signal_id_included=I,
             no_of_sections=N, signal_data_length=D}=S, Sections) ->

    {SN, SL} = case 0 < N of false -> {0, []}; true -> sections_to_words(Sections, B) end,
    T = S#signal{message_length = 3 + (I + D + N) + C + SN},

    L = lists:flatten([
                       %% header: 3
                       bpack(T, [
                                 {#signal.byte_order,
                                  ?WORD1_SHIFT_BYTE_ORDER_1, ?WORD1_MASK_BYTE_ORDER_1},
                                 {#signal.fragment_info,
                                  ?WORD1_SHIFT_FRAGMENT_INFO_1, ?WORD1_MASK_FRAGMENT_INFO_1},
                                 {#signal.signal_id_included,
                                  ?WORD1_SHIFT_SIGNAL_ID_INCLUDED, ?WORD1_MASK_SIGNAL_ID_INCLUDED},
                                 {#signal.compressed,
                                  ?WORD1_SHIFT_COMPRESSED, ?WORD1_MASK_COMPRESSED},
                                 {#signal.checksum_included,
                                  ?WORD1_SHIFT_CHECKSUM_INCLUDED, ?WORD1_MASK_CHECKSUM_INCLUDED},
                                 {#signal.prio,
                                  ?WORD1_SHIFT_PRIO, ?WORD1_MASK_PRIO},
                                 {#signal.byte_order,
                                  ?WORD1_SHIFT_BYTE_ORDER_2, ?WORD1_MASK_BYTE_ORDER_2},
                                 {#signal.message_length,
                                  ?WORD1_SHIFT_MESSAGE_LENGTH, ?WORD1_MASK_MESSAGE_LENGTH},
                                 {#signal.byte_order,
                                  ?WORD1_SHIFT_BYTE_ORDER_3, ?WORD1_MASK_BYTE_ORDER_3},
                                 {#signal.fragment_info,
                                  ?WORD1_SHIFT_FRAGMENT_INFO_2, ?WORD1_MASK_FRAGMENT_INFO_2},
                                 {#signal.signal_data_length,
                                  ?WORD1_SHIFT_SIGNAL_DATA_LENGTH, ?WORD1_MASK_SIGNAL_DATA_LENGTH},
                                 {#signal.byte_order,
                                  ?WORD1_SHIFT_BYTE_ORDER_4, ?WORD1_MASK_BYTE_ORDER_4}
                                ]),
                       bpack(T, [
                                 {#signal.gsn,
                                  ?WORD2_SHIFT_GSN, ?WORD2_MASK_GSN},
                                 {#signal.version_id,
                                  ?WORD2_SHIFT_VERSION_ID, ?WORD2_MASK_VERSION_ID},
                                 {#signal.trace,
                                  ?WORD2_SHIFT_TRACE, ?WORD2_MASK_TRACE},
                                 {#signal.no_of_sections,
                                  ?WORD2_SHIFT_NO_OF_SECTIONS, ?WORD2_MASK_NO_OF_SECTIONS}
                                ]),
                       bpack(T, [
                                 {#signal.send_block_no,
                                  ?WORD3_SHIFT_SEND_BLOCK_NO, ?WORD3_MASK_SEND_BLOCK_NO},
                                 {#signal.recv_block_no,
                                  ?WORD3_SHIFT_RECV_BLOCK_NO, ?WORD3_MASK_RECV_BLOCK_NO}
                                ]),
                       %% signal_id: 0 .. 1
                       case I of 0 -> []; 1 -> T#signal.signal_id end,
                       %% signal_data: 0 .. 25
                       case D of 0 -> []; _ -> T#signal.signal_data end,
                       %% sections: 0 .. 3
                       SL
                      ]),

    %% checksum: 0 .. 1
    words_to_binary(case C of 0 -> L; 1 -> L ++ [bchecksum(L, 0)] end, B).

-spec unpack(binary(), signal()) -> signal().
unpack(Binary, #signal{}=S) ->

    <<W:?WORD(1)/big-unit:8>> = binary_part(Binary, 0, ?WORD(1)),
    ByteOrder = bget(W, ?WORD1_SHIFT_BYTE_ORDER_1, ?WORD1_MASK_BYTE_ORDER_1), % 1 or 3

    W1 = binary_to_word(Binary, ?WORD(0), ByteOrder),
    W2 = binary_to_word(Binary, ?WORD(1), ByteOrder),
    W3 = binary_to_word(Binary, ?WORD(2), ByteOrder),

    S#signal {
      gsn =
          bget(W2, ?WORD2_SHIFT_GSN, ?WORD2_MASK_GSN),
      send_block_no =
          bget(W3, ?WORD3_SHIFT_SEND_BLOCK_NO, ?WORD3_MASK_SEND_BLOCK_NO),
      recv_block_no =
          bget(W3, ?WORD3_SHIFT_RECV_BLOCK_NO, ?WORD3_MASK_RECV_BLOCK_NO),
      message_length =
          bget(W1, ?WORD1_SHIFT_MESSAGE_LENGTH, ?WORD1_MASK_MESSAGE_LENGTH),
      byte_order =
          ByteOrder,
      checksum_included =
          bget(W1, ?WORD1_SHIFT_CHECKSUM_INCLUDED, ?WORD1_MASK_CHECKSUM_INCLUDED),
      signal_id_included =
          bget(W1, ?WORD1_SHIFT_SIGNAL_ID_INCLUDED, ?WORD1_MASK_SIGNAL_ID_INCLUDED),
      compressed =
          bget(W1, ?WORD1_SHIFT_COMPRESSED, ?WORD1_MASK_COMPRESSED),
      fragment_info =
          bget(W1, ?WORD1_SHIFT_FRAGMENT_INFO_1, ?WORD1_MASK_FRAGMENT_INFO_1)
          bor
          bget(W1, ?WORD1_SHIFT_FRAGMENT_INFO_2, ?WORD1_MASK_FRAGMENT_INFO_2),
      prio =
          bget(W1, ?WORD1_SHIFT_PRIO, ?WORD1_MASK_PRIO),
      version_id =
          bget(W2, ?WORD2_SHIFT_VERSION_ID, ?WORD2_MASK_VERSION_ID),
      trace =
          bget(W2, ?WORD2_SHIFT_TRACE, ?WORD2_MASK_TRACE),
      no_of_sections =
          bget(W2, ?WORD2_SHIFT_NO_OF_SECTIONS, ?WORD2_MASK_NO_OF_SECTIONS),
      signal_data_length =
          bget(W1, ?WORD1_SHIFT_SIGNAL_DATA_LENGTH, ?WORD1_MASK_SIGNAL_DATA_LENGTH)
     }.


-spec checksum(binary(), endianness()) -> non_neg_integer().
checksum(Binary, Endianness) ->
    bchecksum(baseline_binary:binary_to_words(Binary, 0, Endianness), 0).

%%
%% ~/include/kernel/RefConvert.hpp
%%

-spec number_to_ref(pos_integer(), pos_integer()) -> pos_integer().
number_to_ref(Block, Node) ->
    (Block bsl 16) bor Node.

-spec ref_to_block(pos_integer()) -> pos_integer().
ref_to_block(Ref) ->
    Ref bsr 16.

-spec ref_to_node(pos_integer()) -> pos_integer().
ref_to_node(Ref) ->
    Ref band 16#0000ffff.

%%
%% ~/src/ndbapi/TransporterFacade.cpp
%%

-spec number_to_index(non_neg_integer()) -> non_neg_integer().
number_to_index(Number) ->
    Number - ?MIN_API_BLOCK_NO.

-spec index_to_number(non_neg_integer()) -> non_neg_integer().
index_to_number(Index) ->
    Index + ?MIN_API_BLOCK_NO.


-spec binary_to_word(binary(), non_neg_integer(), byte_order()) -> non_neg_integer().
binary_to_word(Binary, Start, 0) -> baseline_binary:binary_to_word(Binary, Start, little);
binary_to_word(Binary, Start, 1) -> baseline_binary:binary_to_word(Binary, Start, big).

-spec binary_to_words(binary(), byte_order()) -> [non_neg_integer()].
binary_to_words(Binary, 0) -> baseline_binary:binary_to_words(Binary, 0, little);
binary_to_words(Binary, 1) -> baseline_binary:binary_to_words(Binary, 0, big).

-spec word_to_binary(non_neg_integer(), byte_order()) -> binary().
word_to_binary(Word, 0) -> baseline_binary:word_to_binary(Word, little);
word_to_binary(Word, 1) -> baseline_binary:word_to_binary(Word, big).

-spec words_to_binary([non_neg_integer()], byte_order()) -> binary().
words_to_binary(List, 0) -> baseline_binary:words_to_binary(List, little);
words_to_binary(List, 1) -> baseline_binary:words_to_binary(List, big).


-spec sections_to_words([term()], byte_order()) -> {non_neg_integer(), [integer()]}.
sections_to_words(Sections, ByteOrder) ->
    sections_to_words(Sections, ByteOrder, 0, [], []).


-spec find(id(), pos_integer()) -> {ok, pid()}|{error, _}.
find(_Id, 0) ->
    {error, not_found};
find(Id, Retry) ->
    case baseline_app:find(ndbepi_sup, Id) of
        undefined ->
            ok = timer:sleep(300),
            find(Id, Retry - 1);
        Pid ->
            {ok, Pid}
    end.

%% == internal ==

sections_to_words([], _ByteOrder, N, List1, List2) ->
    {N, lists:reverse(List1) ++ lists:reverse(List2)};
sections_to_words([H|T], ByteOrder, N, List1, List2) ->
    L = case H of
            H when is_integer(H), H =< 16#ffffffff ->
                [H];
            H when is_integer(H) -> % TODO
                case ByteOrder of
                    0 -> [H band 16#ffffffff, H bsr 32];
                    1 -> [H bsr 32, H band 16#ffffffff]
                end;
            H when is_binary(H) ->
                binary_to_words(<<H/binary, 0:?WORD(1)/big-unit:8>>, 0)
        end,
    sections_to_words(T, ByteOrder, N + length(L), [length(L)|List1], [L|List2]).


bchecksum(List, Acc) ->
    lists:foldl(fun(E, A) -> A bxor E end, Acc, List).

bget(Word, Shift, Mask) ->
    (Word band Mask) bsr Shift.

bpack(Tuple, List) ->
    lists:foldl(fun({N, S, M}, A) -> A bor ((element(N, Tuple) bsl S) band M) end, 0, List).
