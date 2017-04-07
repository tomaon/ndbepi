-module(ndbepi_transporter).

-include("internal.hrl").

-import(ndbepi_util, [binary_to_word/3, binary_to_words/4,
                      pack/2, unpack/2]).

%%
%% ~/src/transporter/TCP_Transporter.cpp: TCP_Transporter::*
%%
%% TODO: PACKED, SECTION, TRACE, MAX_(SEND|RECV)_MESSAGE_BYTESIZE
%%

%% -- private --
-export([start_link/2]).

-behaviour(gen_server).
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2]).

-export([default/2]).
-export([call/4, cast/3]).
-export([deliver/3]).

%% -- internal --
-define(TCP_TRANSPORTER, $1).

-record(state, {
          interval    :: non_neg_integer(),
          default     :: signal(),
          tab         :: ets:tab(),
          socket      :: undefined|gen_tcp:socket(),
          regreq      :: undefined|binary(),
          rest = <<>> :: binary()
         }).

%% == private ==

-spec start_link([term()], [term()]) -> {ok, pid()}|{error, _}.
start_link(Args, Options) ->
    gen_server:start_link(?MODULE, Args, Options).


-spec default(pid(), timeout()) -> signal().
default(Pid, Timeout) ->
    gen_server:call(Pid, default, Timeout).


-spec call(pid(), signal(), [term()], timeout()) -> {ok, signal()}|{error, _}.
call(Pid, Signal, Sections, Timeout) ->
    case monitor(process, Pid) of
        MonitorRef ->
            ok = cast(Pid, Signal, Sections),
            try
                receive
                    #signal{}=S ->
                        {ok, S};
                    {'DOWN', _, _, _, Reason} ->
                        {error, Reason}
                after
                    Timeout ->
                        {error, timeout}
                end
            after
                true = demonitor(MonitorRef)
            end
    end.

-spec cast(pid(), signal(), [term()]) -> ok.
cast(Pid, Signal, Sections) ->
    gen_server:cast(Pid, {send, pack(Signal, Sections)}).


-spec deliver(pid(), binary(), signal()) -> signal().
deliver(Pid, Binary, #signal{byte_order=B, signal_id_included=1}=S) ->
    deliver(Pid, Binary, ?WORD(1), S#signal{signal_id = binary_to_word(Binary, 0, B)});
deliver(Pid, Binary, Signal) ->
    deliver(Pid, Binary, 0, Signal).

deliver(Pid, Binary, Start, #signal{byte_order=B, signal_data_length=L}=S) ->
    {D, R} = split_binary(Binary, Start + ?WORD(L)),
    Pid ! S#signal{signal_data = binary_to_words(D, Start, ?WORD(L), B), rest = R}.

%% -- behaviour: gen_server --

init(Args) ->
    setup(Args).

terminate(_Reason, State) ->
    cleanup(State).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call(default, _From, #state{default=D}=X) ->
    {reply, default(D), X}.

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

cleanup(#state{socket=S}=X)
  when S =/= undefined ->
    ok = gen_tcp:close(S),
    cleanup(X#state{socket = undefined});
cleanup(_) ->
    baseline:flush().

setup(Args) ->
    false = process_flag(trap_exit, true),
    {ok, Args, 300}.


initialized([Interval, Default, Args]) ->
    case baseline_app:find(ndbepi_sup, ndbepi_block_mgr, 100, 10) of
        undefined ->
            {stop, not_found, undefined};
        Pid ->
            try baseline_ets:tab(Pid) of
                Tab ->
                    found(Args, #state{interval = Interval, default = Default, tab = Tab})
            catch
                error:Reason ->
                    {stop, Reason, undefined}
            end
    end.

found(Args, #state{socket=undefined}=X) ->
    case apply(gen_tcp, connect, Args) of
        {ok, Socket} ->
            connected(binary:compile_pattern(<<?LS>>), X#state{socket = Socket});
        {error, Reason} ->
            {stop, Reason, X}
    end.

connected(Pattern, #state{socket=S}=X) ->
    %%
    %% ~/src/common/transporter/TransporterRegistry.cpp: TransporterRegistry::start_service/1
    %% ~/src/common/util/SocketAuthenticator.cpp: SocketAuthSimple::client_authenticate/1
    %%
    case call(S,
              <<"ndbd", ?LS, "ndbd passwd", ?LS>>,
              <<"ok">>,
              Pattern, 3000) of
        ok ->
            authorized(Pattern, X);
        {error, Reason} ->
            {stop, Reason, X}
    end.

authorized(Pattern, #state{default=D, socket=S}=X) -> % default:'recv' -> 'send'
    case call(S,
              <<(integer_to_binary(D#signal.recv_node_id))/binary, " ", ?TCP_TRANSPORTER, ?LS>>,
              <<(integer_to_binary(D#signal.send_node_id))/binary, " ", ?TCP_TRANSPORTER>>,
              Pattern, 3000) of
        ok ->
            ok = inet:setopts(S, [{active, true}]),
            {noreply, X#state{regreq = regreq(default(D))}, 0};
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


received(Binary, #state{default=D}=X)
  when size(Binary) >= ?WORD(3) ->
    received(Binary, unpack(binary_part(Binary, 0, ?WORD(3)), D), X);
received(Binary, #state{interval=I}=X) ->
    {noreply, X#state{rest = Binary}, I}.

received(Binary, #signal{message_length=M}=S, #state{}=X)
  when size(Binary) >= ?WORD(M) ->
    {B, R} = split_binary(Binary, ?WORD(M)),
    accepted(B, S, X#state{rest = R});
received(Binary, _Signal, #state{interval=I}=X) ->
    {noreply, X#state{rest = Binary}, I}.

accepted(Binary, #signal{checksum_included=0, message_length=M}=S, State) ->
    checked(binary_part(Binary, {?WORD(3), ?WORD(M - 3)}), S, State);
accepted(Binary, #signal{checksum_included=1, message_length=M}=S, State) ->
    {B, <<C:?WORD(1)/big-unit:8>>} = split_binary(Binary, ?WORD(M - 1)),
    case mgmepi_util:checksum(B, ?WORD(M - 1), ?WORD(1)) of
        C ->
            checked(binary_part(B, {?WORD(3), ?WORD(M - 4)}), S, State)
    end.

checked(Binary, #signal{recv_block_no=B}=S, #state{tab=T, rest=R}=X) ->
    try ets:lookup_element(T, B, 2) of
        Pid ->
            _ = spawn(?MODULE, deliver, [Pid, Binary, S]),
            received(R, X#state{rest = <<>>})
    catch
        error:Reason ->
            {stop, {Reason, S}, X}
    end.


ready(Packet, #state{socket=S}=X) ->
    case gen_tcp:send(S, Packet) of
        ok ->
            {noreply, X};
        {error, Reason} ->
            {stop, Reason, X}
    end.


call(Socket, Req, Cnf, Pattern, Timeout) ->
    case gen_tcp:send(Socket, Req) of
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

default(#signal{send_node_id=S, recv_node_id=R}=X) -> % 'recv' -> 'send'
    X#signal{send_node_id = R, recv_node_id = S}.

regreq(#signal{send_node_id=N}=S) ->
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
                          ?NUMBER_TO_REF(?API_CLUSTERMGR, N),
                          ?NDB_VERSION_ID,
                          ?MYSQL_VERSION_ID
                         ]
          }, []).
