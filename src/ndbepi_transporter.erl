-module(ndbepi_transporter).

-include("internal.hrl").

-import(ndbepi_util, [endianness/1,
                      byte/1, part/3, split/2, word/1,
                      bin_to_word/4, bin_to_words/5,
                      words_to_bin/2,
                      checksum/1, checksum/5
                     ]).

%% -- private --
-export([start_link/2]).
-export([cast/3]).
-export([deliver/3]).

-behaviour(gen_server).
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2]).

%% -- internal --

%% -- ~/include/kernel/ndb_limits.h --
-define(NDB_SECTION_SEGMENT_SZ, 60).

%% -- ~/include/transporter/TransporterDefinitions.hpp --
-define(MAX_RECV_MESSAGE_BYTESIZE, 32768).
-define(MAX_SEND_MESSAGE_BYTESIZE, 32768).

%% enum TransporterType
-define(TCP_TRANSPORTER, $1).

%% -- ~/include/transporter/TransporterFacade.cpp --
-define(CHUNK_SZ, ((?MAX_SEND_MESSAGE_BYTESIZE bsl 2) div ?NDB_SECTION_SEGMENT_SZ - 2 ) * ?NDB_SECTION_SEGMENT_SZ).

%% -- ~/src/common/transporter/TransporterInternalDefinitions.hpp --

%%  b : Byte order        ,  1 * 3
%%  c : Checksum included ,  1
%%  d : Signal data length,  5
%%  f : FragmentInfo1     ,  1
%%  g : GSN               , 16
%%  h : FragmentInfo2     ,  1
%%  i : Signal id included,  1
%%  m : Message length    , 16
%%  n : No of segments    ,  2
%%  p : Prio              ,  2
%%  r : Recievers block no, 16
%%  s : Senders block no  , 16
%%  t : Trace             ,  6
%%  v : Version id        ,  4
%% (z): Compression       ,  1
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

%% -- other --
-define(GET(Word, Shift, Mask), ((Word band Mask) bsr Shift)).


-record(state, {
          local       :: node_id(),
          remote      :: node_id(),
          interval    :: non_neg_integer(),
          ets         :: undefined|pid(),
          tab         :: undefined|ets:tab(),
          socket      :: undefined|gen_tcp:socket(),
          regreq      :: undefined|binary(),
          rest = <<>> :: binary()
         }).

%% == private ==

-spec start_link([term()], [term()]) -> {ok, pid()}|{error, _}.
start_link(Args, Options) ->
    gen_server:start_link(?MODULE, Args, Options).


-spec cast(pid(), signal(), [term()]) -> ok.
cast(Pid, #signal{byte_order=B}=S, Sections) ->
    E = endianness(B),
    {L, D} = sections_to_words(Sections, E, [], []),
    cast(Pid, S, L, D, byte(lists:sum(L)), E).

cast(Pid, Signal, Length, Data, Size, Endianness) % TODO
  when Size =< ?CHUNK_SZ ->
    gen_server:cast(Pid, {send, pack(Signal, Length, Data, Endianness)}).


-spec deliver(pid(), binary(), signal()) -> {signal(), binary()}.
deliver(Pid, Binary, #signal{byte_order=B}=S) ->
    deliver(Pid, Binary, endianness(B), S).

deliver(Pid, Binary, Endianness, #signal{signal_id_included=0}=S) ->
    deliver(Pid, Binary, 0, Endianness, S);
deliver(Pid, Binary, Endianness, #signal{signal_id_included=1}=S) ->
    deliver(Pid, Binary, 1, Endianness,
            S#signal{signal_id = bin_to_word(Binary, 0, 1, Endianness)}).

deliver(Pid, Binary, Start, Endianness, #signal{signal_data_length=L}=S) ->
    {B, R} = split(Binary, Start + L),
    Pid ! {S#signal{signal_data = bin_to_words(B, Start, L, 1, Endianness)}, R}.

%% -- behaviour: gen_server --

init(Args) ->
    setup(Args).

terminate(_Reason, State) ->
    cleanup(State).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call(_Request, _From, State) ->
    {stop, enosys, State}.

handle_cast({send, Packet}, State) ->
    ready(Packet, State).

handle_info({tcp, S, Data}, #state{socket=S}=X) ->
    R = X#state.rest,
    received(<<R/binary, Data/binary>>, X#state{rest = <<>>});
handle_info(timeout, #state{}=X) ->
    interrupted(X);
handle_info(timeout, Args) ->
    initialized(Args);
handle_info({Reason, S}, #state{socket=S}=X) ->
    {stop, Reason, X#state{socket = undefined}};
handle_info({'EXIT', S, Reason}, #state{socket=S}=X) ->
    {stop, Reason, X#state{socket = undefined}};
handle_info({'EXIT', _Pid, Reason}, State) ->
    {stop, Reason, State}.

%% == internal ==

%%
%% ~/src/transporter/TCP_Transporter.cpp: TCP_Transporter::*
%%

cleanup(#state{socket=S}=X)
  when S =/= undefined ->
    ok = gen_tcp:close(S),
    cleanup(X#state{socket = undefined});
cleanup(#state{ets=E}=X)
  when E =/= undefined ->
    catch true = baseline_ets:delete(E, X#state.remote),
    cleanup(X#state{ets = undefined});
cleanup(_) ->
    baseline:flush().

setup(Args) ->
    false = process_flag(trap_exit, true),
    {ok, Args, 300}.


initialized([Local, Remote, Interval, Default, Args]) ->
    case baseline_app:find(ndbepi_sup, ndbepi_ets, 100, 10) of
        undefined ->
            {stop, not_found, undefined};
        Pid ->
            found(Default, Args, #state{local = Local, remote = Remote,
                                        interval = Interval, ets = Pid})
    end.

found(Default, Args, #state{remote=R, ets=E}=X) ->
    case baseline_ets:insert_new(E, {R, self(), Default}) of
        true ->
            registered(Default, Args, X#state{tab = baseline_ets:tab(E)});
        false ->
            {stop, ebusy, X}
    end.

registered(Default, Args, #state{}=X) ->
    case apply(gen_tcp, connect, Args) of
        {ok, Socket} ->
            connected(Default, binary:compile_pattern(<<?LS>>), X#state{socket = Socket});
        {error, Reason} ->
            {stop, Reason, X}
    end.

connected(Default, Pattern, #state{socket=S}=X) ->
    %%
    %% ~/src/common/transporter/TransporterRegistry.cpp: TransporterRegistry::start_service/1
    %% ~/src/common/util/SocketAuthenticator.cpp: SocketAuthSimple::client_authenticate/1
    %%
    case call(S,
              <<"ndbd", ?LS, "ndbd passwd", ?LS>>,
              <<"ok">>,
              Pattern, 5000) of
        ok ->
            authorized(Default, Pattern, X);
        {error, Reason} ->
            {stop, Reason, X}
    end.

authorized(Default, Pattern, #state{local=L, remote=R, socket=S}=X) ->
    case call(S,
              <<(integer_to_binary(L))/binary, " ", ?TCP_TRANSPORTER, ?LS>>,
              <<(integer_to_binary(R))/binary, " ", ?TCP_TRANSPORTER>>,
              Pattern, 5000) of
        ok ->
            opened(X#state{regreq = regreq(L, Default)});
        {error, Reason} ->
            {stop, Reason, X}
    end.

opened(#state{socket=S}=X) ->
    ok = inet:setopts(S, [{active, true}]),
    {noreply, X, 0}. % -> HB


ready(Packet, #state{interval=I, socket=S}=X) ->
    case gen_tcp:send(S, Packet) of
        ok ->
            {noreply, X, I};
        {error, Reason} ->
            {stop, Reason, X}
    end.


interrupted(#state{interval=I, socket=S, regreq=R}=X) ->
    case gen_tcp:send(S, R) of
        ok ->
            {noreply, X, I};
        {error, Reason} ->
            {stop, Reason, X}
    end.


received(Binary, State) ->
    received(Binary, word(size(Binary)), State).

received(Binary, Size, State)
  when Size >= 3 ->
    received(Binary, Size, unpack(Binary), State);
received(Binary, _Size, #state{interval=I}=X) ->
    {noreply, X#state{rest = Binary}, I}.

received(Binary, Size, #signal{message_length=M}=S, State)
  when M > 0, Size >= M ->
    {B, R} = split(Binary, M),
    accepted(B, S, State#state{rest = R});
received(Binary, _Size, #signal{message_length=M}, #state{interval=I}=X) ->
    case M =:= 0 orelse M > word(?MAX_RECV_MESSAGE_BYTESIZE) of
        false ->
            {noreply, X#state{rest = Binary}, I};
        true ->
            {stop, ebadmsg, X}
    end.

accepted(Binary, #signal{checksum_included=0, message_length=M}=S, State) ->
    checked(part(Binary, 3, M - 3), S, State);
accepted(Binary, #signal{checksum_included=1, message_length=M}=S, State) ->
    C = bin_to_word(Binary, M - 1, 1, native),
    case checksum(Binary, 0, M - 1, 1, native) of
        C ->
            checked(part(Binary, 3, M - 4), S, State);
        _ ->
            {stop, ebadmsg, State}
    end.

checked(Binary, #signal{recv_block_no=N}=S, #state{tab=T, rest=R}=X) ->
    _ = spawn(?MODULE, deliver, [ets:lookup_element(T, N, 2), Binary, S]),
    received(R, X#state{rest = <<>>}).


call(Socket, Req, Cnf, Pattern, Timeout) ->
    case gen_tcp:send(Socket, Req) of % default: active=false
        ok ->
            case gen_tcp:recv(Socket, 0, Timeout) of
                {ok, Binary} ->
                    check(Binary, Pattern, Cnf);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

check(Binary, Pattern, Expected) ->
    case baseline_binary:split(Binary, Pattern) of
        {[Expected], <<>>} ->
            ok;
        {[<<"BYE">>], <<>>} ->
            {error, econnreset}
    end.

regreq(Local, #signal{byte_order=B}=S) ->
    %%
    %% ~/include/kernel/signaldata/ApiRegSignalData.hpp: ApiRegReq
    %% ~/src/ndbapi/ClusterMgr.cpp: ClusterMgr::execAPI_REGREQ/2
    %%
    pack(S#signal{
           gsn = ?GSN_API_REGREQ,
           send_block_no = ?API_CLUSTERMGR,
           recv_block_no = ?QMGR,
           signal_data_length = 3,
           signal_data = [
                          ?NUMBER_TO_REF(?API_CLUSTERMGR, Local),
                          ?NDB_VERSION_ID,
                          ?MYSQL_VERSION_ID
                         ]
          }, [], [], endianness(B)).


sections_to_words([], _Endianness, List1, List2) ->
    {lists:reverse(List1), lists:reverse(List2)};
sections_to_words([H|T], Endianness, List1, List2) ->
    L = case H of
            H when is_binary(H) ->
                B = <<H/binary, 0, 0, 0, 0>>,
                bin_to_words(B, 0, word(size(B)), 1, Endianness)
        end,
    sections_to_words(T, Endianness, [length(L)|List1], [L|List2]).

word(Tuple, List) ->
    lists:foldl(fun({N, S, M}, A) -> A bor ((element(N, Tuple) bsl S) band M) end, 0, List).

pack(#signal{checksum_included=C, signal_id_included=I, signal_data_length=D,
             sections_length=L}=S, Length, Data, Endianness) ->

    {SS, SL, SD} = case L of 0 -> {0, [], []}; _ -> {lists:sum(Length), Length, Data} end,

    X = S#signal{message_length = 3 + C + I + D + L + SS},

    W = lists:flatten([
                       %% header
                       word(X, [
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
                       word(X, [
                                {#signal.gsn,
                                 ?WORD2_SHIFT_GSN, ?WORD2_MASK_GSN},
                                {#signal.version_id,
                                 ?WORD2_SHIFT_VERSION_ID, ?WORD2_MASK_VERSION_ID},
                                {#signal.trace,
                                 ?WORD2_SHIFT_TRACE, ?WORD2_MASK_TRACE},
                                {#signal.sections_length,
                                 ?WORD2_SHIFT_NO_OF_SECTIONS, ?WORD2_MASK_NO_OF_SECTIONS}
                               ]),
                       word(X, [
                                {#signal.send_block_no,
                                 ?WORD3_SHIFT_SEND_BLOCK_NO, ?WORD3_MASK_SEND_BLOCK_NO},
                                {#signal.recv_block_no,
                                 ?WORD3_SHIFT_RECV_BLOCK_NO, ?WORD3_MASK_RECV_BLOCK_NO}
                               ]),
                       %% signal_id
                       case I of 0 -> []; 1 -> X#signal.signal_id end,
                       %% signal_data
                       case D of 0 -> []; _ -> X#signal.signal_data end,
                       %% sections
                       SL, SD
                      ]),

    %% checksum
    words_to_bin(case C of 0 -> W; 1 -> W ++ [checksum(W)] end, Endianness).

unpack(Binary) ->

    B = ?GET(bin_to_word(Binary, 0, 1, native),
             ?WORD1_SHIFT_BYTE_ORDER_1, ?WORD1_MASK_BYTE_ORDER_1), % 1 or 3

    E = endianness(B),

    W1 = bin_to_word(Binary, 0, 1, E),
    W2 = bin_to_word(Binary, 1, 1, E),
    W3 = bin_to_word(Binary, 2, 1, E),

    #signal {
       gsn =
           ?GET(W2, ?WORD2_SHIFT_GSN, ?WORD2_MASK_GSN),
       send_block_no =
           ?GET(W3, ?WORD3_SHIFT_SEND_BLOCK_NO, ?WORD3_MASK_SEND_BLOCK_NO),
       recv_block_no =
           ?GET(W3, ?WORD3_SHIFT_RECV_BLOCK_NO, ?WORD3_MASK_RECV_BLOCK_NO),
       byte_order =
           B,
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
