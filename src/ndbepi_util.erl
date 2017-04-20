-module(ndbepi_util).

-include("internal.hrl").

%% -- private --
-export([endianness/1]).
-export([part/2, part/3, split/2]).
-export([bin_to_word/4, bin_to_words/5]).
-export([words_to_bin/2]).
-export([checksum/1, checksum/5]).

%% == private ==

-spec endianness(0|1) -> little|big.
endianness(0) -> little;
endianness(1) -> big.


-spec part(binary(), non_neg_integer()) -> binary().
part(Subject, Start) ->
    binary_part(Subject, ?BYTE(Start), size(Subject) - ?BYTE(Start)).

-spec part(binary(), non_neg_integer(), integer()) -> binary().
part(Subject, Start, Length) ->
    binary_part(Subject, ?BYTE(Start), ?BYTE(Length)).

-spec split(binary(), non_neg_integer()) -> {binary(), binary()}.
split(Subject, Length) ->
    split_binary(Subject, ?BYTE(Length)).


-spec bin_to_word(binary(), non_neg_integer(), integer(), endianness())
                 -> non_neg_integer().
bin_to_word(Subject, Start, Length, Endianness) ->
    baseline_binary:decode_unsigned(Subject, ?BYTE(Start), ?BYTE(Length), Endianness).

-spec bin_to_words(binary(), non_neg_integer(), non_neg_integer(), integer(), endianness())
                  -> [non_neg_integer()].
bin_to_words(Subject, Start, Size, Incr, Endianness) ->
    baseline_binary:decode_unsigned(Subject, ?BYTE(Start), ?BYTE(Size), ?BYTE(Incr), Endianness).


%%pec word_to_bin(non_neg_integer(), endianness()) -> binary().
word_to_bin(Word, Endianness) ->
    baseline_binary:encode_unsigned(Word, ?BYTE(1), Endianness).

-spec words_to_bin([non_neg_integer()], endianness()) -> binary().
words_to_bin(Words, Endianness) ->
    list_to_binary(lists:map(fun(E) -> word_to_bin(E, Endianness) end, Words)).


-spec checksum([non_neg_integer()]) -> non_neg_integer().
checksum(Words) ->
    mgmepi_util:checksum(Words).

-spec checksum(binary(), non_neg_integer(), non_neg_integer(), integer(), endianness())
              -> non_neg_integer().
checksum(Binary, Start, Size, Incr, Endianness) ->
    mgmepi_util:checksum(Binary, ?BYTE(Start), ?BYTE(Size), ?BYTE(Incr), Endianness).
