-module(ndbepi_gen_block).

-include("internal.hrl").

-import(ndbepi_util, [part/2]).

%% -- private --
-export([start_link/4]).
-export([call/2, call/3]).

-behaviour(gen_statem).
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
-export([loaded/3, found/3, configured/3, ready/3]).


-callback init(Args :: [term()]) ->
    {ok, Data :: term()}|
    {stop, Reason :: term()}.

-callback terminate(Reason :: term(), Data :: term()) ->
    term().

-callback code_change(OldVsn :: term(), Data :: term(), Extra :: term()) ->
    {ok, NewData :: term()}|
    {error, Reason :: term()}.

-callback handle_call(Request :: term(), Signal :: signal(), Data :: term()) ->
    {noreply, Args :: [term()], NewData :: term()}|
    {stop, Reason :: term(), NewData :: term()}.

-callback handle_info(Signal :: signal(), Binary :: binary(), Data :: term()) ->
    {reply, Reply :: term(), NewData :: term()}|
    {noreply, NewData :: term()}|
    {stop, Reason :: term(), NewData :: term()}.

%% -- internal --
-type(from() :: {pid(), term()}).
-type(request() :: {atom(), [term()], node_id()}).

-record(data, {
          module    :: module(),
          block_no  :: block_no(),
          ets       :: undefined|pid(),
          tab       :: undefined|ets:tab(),
          internal  :: undefined|term(),
          fragments :: undefined|map(),
          from      :: undefined|from()
         }).

%% == private ==

-spec start_link(module(), block_no(), [term()], [term()]) -> {ok, pid()}|{error, _}.
start_link(Module, BlockNo, Args, Options) ->
    gen_statem:start_link(?MODULE, [Module, BlockNo, Args], Options).


-spec call(pid(), request()) -> term().
call(Pid, Request) ->
    gen_statem:call(Pid, Request).

-spec call(pid(), request(), timeout()) -> term().
call(Pid, Request, Timeout) ->
    gen_statem:call(Pid, Request, Timeout).

%% -- behaviour: gen_statem --

callback_mode() ->
    state_functions.

init([Module, BlockNo, Args]) ->
    false = process_flag(trap_exit, true),
    {ok, loaded, #data{module = Module, block_no = BlockNo}, {next_event, internal, Args}}.

terminate(Reason, State, #data{internal=I}=D)
  when I =/= undefined ->
    _ = apply(D#data.module, terminate, [Reason, I]),
    terminate(Reason, State, D#data{internal = undefined});
terminate(Reason, State, #data{ets=E}=D)
  when E =/= undefined ->
    catch true = baseline_ets:delete(E, D#data.block_no),
    terminate(Reason, State, D#data{ets = undefined});
terminate(_, _, _) ->
    baseline:flush().

code_change(OldVsn, OldState, #data{module=M, internal=I}=D, Extra) ->
    case M:code_change(OldVsn, I, Extra) of
        {ok, Internal} ->
            {ok, OldState, D#data{internal = Internal}};
        {error, Reason} ->
            {error, Reason}
    end.


loaded(internal, Args, #data{ets=undefined}=D) ->
    case baseline_app:find(ndbepi_sup, ndbepi_ets, 100, 1) of
        undefined ->
            {stop, not_found};
        Pid ->
            {next_state, found, D#data{ets = Pid}, {next_event, internal, Args}}
    end.

found(internal, Args, #data{block_no=B, ets=E}=D) ->
    case baseline_ets:insert_new(E, {B, self()}) of
        true ->
            {next_state, configured, D#data{tab = baseline_ets:tab(E)}, {next_event, internal, Args}};
        false ->
            {stop, ebusy} % -> retry
    end.

configured(internal, Args, #data{module=M, internal=undefined}=D) ->
    case M:init(Args) of
        {ok, Internal} ->
            {next_state, ready, D#data{internal = Internal, fragments = maps:new()}};
        {stop, Reason} ->
            {stop, Reason}
    end.

ready(info, {#signal{fragment_info=0}=S, Binary}, Data) ->
    received(S, Binary, Data);
ready(info, {#signal{fragment_info=3}=S, Binary}, Data) ->
    {L, M} = maps:take(key(S), Data#data.fragments),
    received(S, list_to_binary(lists:reverse([part(Binary, 1)|L])), Data#data{fragments = M});
ready(info, {#signal{fragment_info=2}=S, Binary}, Data) ->
    M = maps:update_with(key(S), fun(L) -> [part(Binary, 1)|L] end, Data#data.fragments),
    {keep_state, Data#data{fragments = M}};
ready(info, {#signal{fragment_info=1}=S, Binary}, Data) ->
    M = maps:put(key(S), [part(Binary, 1)], Data#data.fragments),
    {keep_state, Data#data{fragments = M}};
ready(info, {'EXIT', _Pid, Reason}, Data) ->
    {stop, Reason, Data};
ready({call, From}, {A, L, N}, #data{module=M, block_no=B, tab=T, internal=I}=D) ->
    try select(T, B, N) of
        {_, P, S} ->
            case M:handle_call({A, L}, S, I) of
                {noreply, Args, Internal} ->
                    ok = apply(ndbepi_transporter, cast, [P|Args]),
                    {keep_state, D#data{internal = Internal, from = From}};
                {error, Reason, Internal} ->
                    {stop, Reason, D#data{internal = Internal}}
            end
    catch
        error:Reason ->
            {stop, Reason, D}
    end.

%% == internal ==

key(#signal{signal_data_length=L, signal_data=D}) ->
    lists:nth(L, D).

received(Signal, Binary, #data{module=M, internal=I, from=F}=D) ->
    case M:handle_info(Signal, Binary, I) of
        {reply, Reply, Internal} when F =/= undefined ->
            _ = gen_server:reply(F, Reply),
            {next_state, ready, D#data{internal = Internal, from = undefined}};
        {noreply, Internal} ->
            {next_state, ready, D#data{internal = Internal, from = undefined}};
        {stop, Reason, Internal} ->
            {stop, Reason, D#data{internal = Internal, from = undefined}}
    end.

select(Tab, Key, 0) ->
    case ets:select(Tab, [{{'$1', '_', '_'}, [], ['$_']}]) of
        [] ->
            error(badarg);
        List ->
            lists:nth(1 + Key rem length(List), List)
    end;
select(Tab, Key, Param) ->
    case ets:select(Tab, [{{Param, '_', '_'}, [], ['$_']}]) of
        [] ->
            select(Tab, Key, 0);
        List ->
            hd(List)
    end.
