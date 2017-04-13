-module(ndbepi_util).

-include("internal.hrl").

%% -- private --
-export([binary_to_words/4, words_to_binary/2]).
-export([endianness/1]).

%% -- internal --
-type(byte_order() :: 0|1).

%% == private ==

-spec binary_to_words(binary(), non_neg_integer(), pos_integer(), byte_order())
                     -> [non_neg_integer()].
binary_to_words(Binary, Start, Length, ByteOrder) ->
    baseline_binary:decode_unsigned(Binary, Start, Length, endianness(ByteOrder), ?WORD(1)).


word_to_binary(Word, ByteOrder) ->
    baseline_binary:encode_unsigned(Word, ?WORD(1), endianness(ByteOrder)).

-spec words_to_binary([non_neg_integer()], byte_order())
                     -> binary().
words_to_binary(Words, ByteOrder) ->
    list_to_binary(lists:map(fun(E) -> word_to_binary(E, ByteOrder) end, Words)).

%% == internal ==

endianness(0) -> little;
endianness(1) -> big.
