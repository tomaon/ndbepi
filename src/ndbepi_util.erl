-module(ndbepi_util).

-include("internal.hrl").

%% -- private --
-export([endianness/1]).
-export([part/3, split/2, word/1]).
-export([bin_to_word/4, bin_to_words/5]).
-export([words_to_bin/2]).
-export([checksum/1, checksum/5]).

%% == private ==

-spec endianness(0|1) -> little|big.
endianness(0) -> little;
endianness(1) -> big.


-spec part(binary(), non_neg_integer(), integer()) -> binary().
part(Subject, Start, Length) ->
    binary_part(Subject, ?WORD(Start), ?WORD(Length)).

-spec split(binary(), non_neg_integer()) -> {binary(), binary()}.
split(Subject, Length) ->
    split_binary(Subject, ?WORD(Length)).

-spec word(integer()) -> integer().
word(Int) ->
    Int div ?WORD(1).


-spec bin_to_word(binary(), non_neg_integer(), integer(), endianness())
                 -> non_neg_integer().
bin_to_word(Subject, Start, Length, Endianness) ->
    baseline_binary:decode_unsigned(Subject, ?WORD(Start), ?WORD(Length), Endianness).

-spec bin_to_words(binary(), non_neg_integer(), non_neg_integer(), integer(), endianness())
                  -> [non_neg_integer()].
bin_to_words(Subject, Start, Size, Incr, Endianness) ->
    baseline_binary:decode_unsigned(Subject, ?WORD(Start), ?WORD(Size), ?WORD(Incr), Endianness).


%%pec word_to_bin(non_neg_integer(), endianness()) -> binary().
word_to_bin(Word, Endianness) ->
    baseline_binary:encode_unsigned(Word, ?WORD(1), Endianness).

-spec words_to_bin([non_neg_integer()], endianness()) -> binary().
words_to_bin(Words, Endianness) ->
    list_to_binary(lists:map(fun(E) -> word_to_bin(E, Endianness) end, Words)).


-spec checksum([integer()]) -> integer().
checksum(Words) ->
    mgmepi_util:checksum(Words).

-spec checksum(binary(), non_neg_integer(), non_neg_integer(), integer(), endianness())
              -> non_neg_integer().
checksum(Binary, Start, Size, Incr, Endianness) ->
    mgmepi_util:checksum(Binary, ?WORD(Start), ?WORD(Size), ?WORD(Incr), Endianness).
