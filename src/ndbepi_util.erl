-module(ndbepi_util).

-include("internal.hrl").

%% -- private --
-export([sections_to_words/2]).
-export([binary_to_word/3, binary_to_words/4,
         word_to_binary/2, words_to_binary/2]).

%% -- internal --
-type(byte_order() :: 0|1).

%% == private ==

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
                B = <<H/binary, 0, 0, 0, 0>>,
                binary_to_words(B, 0, byte_size(B), ByteOrder)
        end,
    sections_to_words(T, ByteOrder, N + length(L), [length(L)|List1], [L|List2]).
