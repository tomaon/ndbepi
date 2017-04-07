-module(ndbepi_util).

-include("internal.hrl").

%% -- private --
-export([pack/2, unpack/2]).
-export([binary_to_word/3, binary_to_words/4,
         word_to_binary/2, words_to_binary/2]).

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

-define(GET(Word, Shift, Mask), ((Word band Mask) bsr Shift)).

-type(byte_order() :: 0|1).

%% == private ==

-spec pack(signal(), [term()]) -> binary().
pack(#signal{byte_order=B, checksum_included=C, signal_id_included=I,
             signal_data_length=D, sections_length=N}=S, Sections) ->

    {SN, SL} = case N of 0 -> {0, []}; _ -> sections_to_words(Sections, B) end,
    T = S#signal{message_length = 3 + C + I + D + N + SN},

    L = lists:flatten([
                       %% header: 3
                       word(T, [
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
                       word(T, [
                                {#signal.gsn,
                                 ?WORD2_SHIFT_GSN, ?WORD2_MASK_GSN},
                                {#signal.version_id,
                                 ?WORD2_SHIFT_VERSION_ID, ?WORD2_MASK_VERSION_ID},
                                {#signal.trace,
                                 ?WORD2_SHIFT_TRACE, ?WORD2_MASK_TRACE},
                                {#signal.sections_length,
                                 ?WORD2_SHIFT_NO_OF_SECTIONS, ?WORD2_MASK_NO_OF_SECTIONS}
                               ]),
                       word(T, [
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
    words_to_binary(case C of 0 -> L; 1 -> L ++ [mgmepi_util:checksum(L)] end, B).

-spec unpack(binary(), signal()) -> signal().
unpack(Binary, #signal{}=S) ->

    <<W:?WORD(1)/big-unit:8>> = binary_part(Binary, 0, ?WORD(1)),
    ByteOrder = ?GET(W, ?WORD1_SHIFT_BYTE_ORDER_1, ?WORD1_MASK_BYTE_ORDER_1), % 1 or 3

    W1 = binary_to_word(Binary, ?WORD(0), ByteOrder),
    W2 = binary_to_word(Binary, ?WORD(1), ByteOrder),
    W3 = binary_to_word(Binary, ?WORD(2), ByteOrder),

    S#signal {
      gsn =
          ?GET(W2, ?WORD2_SHIFT_GSN, ?WORD2_MASK_GSN),
      send_block_no =
          ?GET(W3, ?WORD3_SHIFT_SEND_BLOCK_NO, ?WORD3_MASK_SEND_BLOCK_NO),
      recv_block_no =
          ?GET(W3, ?WORD3_SHIFT_RECV_BLOCK_NO, ?WORD3_MASK_RECV_BLOCK_NO),
      byte_order =
          ByteOrder,
      checksum_included =
          ?GET(W1, ?WORD1_SHIFT_CHECKSUM_INCLUDED, ?WORD1_MASK_CHECKSUM_INCLUDED),
      signal_id_included =
          ?GET(W1, ?WORD1_SHIFT_SIGNAL_ID_INCLUDED, ?WORD1_MASK_SIGNAL_ID_INCLUDED),
      compressed =
          ?GET(W1, ?WORD1_SHIFT_COMPRESSED, ?WORD1_MASK_COMPRESSED),
      message_length =
          ?GET(W1, ?WORD1_SHIFT_MESSAGE_LENGTH, ?WORD1_MASK_MESSAGE_LENGTH),
      fragment_info =
          ?GET(W1, ?WORD1_SHIFT_FRAGMENT_INFO_1, ?WORD1_MASK_FRAGMENT_INFO_1)
          bor
          ?GET(W1, ?WORD1_SHIFT_FRAGMENT_INFO_2, ?WORD1_MASK_FRAGMENT_INFO_2),
      prio =
          ?GET(W1, ?WORD1_SHIFT_PRIO, ?WORD1_MASK_PRIO),
      version_id =
          ?GET(W2, ?WORD2_SHIFT_VERSION_ID, ?WORD2_MASK_VERSION_ID),
      trace =
          ?GET(W2, ?WORD2_SHIFT_TRACE, ?WORD2_MASK_TRACE),
      signal_data_length =
          ?GET(W1, ?WORD1_SHIFT_SIGNAL_DATA_LENGTH, ?WORD1_MASK_SIGNAL_DATA_LENGTH),
      sections_length =
          ?GET(W2, ?WORD2_SHIFT_NO_OF_SECTIONS, ?WORD2_MASK_NO_OF_SECTIONS)
     }.


-spec binary_to_word(binary(), non_neg_integer(), byte_order()) -> non_neg_integer().
binary_to_word(Binary, Start, ByteOrder) ->
    baseline_binary:decode_unsigned(Binary, Start, ?WORD(1), endianness(ByteOrder)).

-spec binary_to_words(binary(), non_neg_integer(), pos_integer(), byte_order()) -> [non_neg_integer()].
binary_to_words(Binary, Start, Length, ByteOrder) ->
    baseline_binary:decode_unsigned(Binary, Start, Length, endianness(ByteOrder), ?WORD(1)).

-spec word_to_binary(non_neg_integer(), byte_order()) -> binary().
word_to_binary(Word, ByteOrder) ->
    baseline_binary:encode_unsigned(Word, ?WORD(1), endianness(ByteOrder)).

-spec words_to_binary([non_neg_integer()], byte_order()) -> binary().
words_to_binary(Words, ByteOrder) ->
    list_to_binary(lists:map(fun(E) -> word_to_binary(E, ByteOrder) end, Words)).

%% == internal ==

endianness(0) -> little;
endianness(1) -> big.

sections_to_words(Sections, ByteOrder) -> % TODO: fragment
    sections_to_words(Sections, ByteOrder, 0, [], []).

sections_to_words([], _ByteOrder, N, List1, List2) ->
    {N, lists:reverse(List1) ++ lists:reverse(List2)};
sections_to_words([H|T], ByteOrder, N, List1, List2) ->
    L = case H of
            H when is_binary(H) ->
                B = <<H/binary, 0, 0,0,0>>,
                binary_to_words(B, 0, byte_size(B), ByteOrder)
        end,
    sections_to_words(T, ByteOrder, N + length(L), [length(L)|List1], [L|List2]).


word(Tuple, List) ->
    lists:foldl(fun({N, S, M}, A) -> A bor ((element(N, Tuple) bsl S) band M) end, 0, List).
