-module(ndbepi_gen_block1).

-include("internal.hrl").

-import(ndbepi_util, [part/2]).

%% -- private --
-export([start_link/4]).
-export([call/2, call/3, cast/2]).

-behaviour(gen_server).
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2]).

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
-record(state, {
          module    :: module(),
          block_no  :: block_no(),
          fragments :: map(),
          ets       :: undefined|pid(),
          tab       :: undefined|ets:tab(),
          data      :: undefined|term(),
          from      :: undefined|{pid(), term()}
         }).

-type(request() :: {atom(), [term()], undefined|node_id()}).

%% == private ==

-spec start_link(module(), block_no(), [term()], [term()]) -> {ok, pid()}|{error, _}.
start_link(Module, BlockNo, Args, Options) ->
    gen_server:start_link(?MODULE, [Module, BlockNo, Args], Options).


-spec call(pid(), request()) -> term().
call(Pid, Request) ->
    gen_server:call(Pid, Request).

-spec call(pid(), request(), timeout()) -> term().
call(Pid, Request, Timeout) ->
    gen_server:call(Pid, Request, Timeout).

-spec cast(pid(), request()) -> ok.
cast(Pid, Request) ->
    gen_server:cast(Pid, Request).

%% -- behaviour: gen_server --

init(Args) ->
    setup(Args).

terminate(Reason, State) ->
    cleanup(Reason, State).

code_change(OldVsn, State, Extra) ->
    change(OldVsn, State, Extra).

handle_call(Request, From, State) ->
    ready(Request, From, State).

handle_cast(Request, State) ->
    ready(Request, undefined, State).

handle_info({#signal{fragment_info=0}=S, Binary}, State) ->
    received(S, Binary, State);
handle_info({#signal{fragment_info=3}=S, Binary}, State) ->
    {L, M} = maps:take(key(S), State#state.fragments),
    received(S, list_to_binary(lists:reverse([part(Binary, 1)|L])), State#state{fragments = M});
handle_info({#signal{fragment_info=2}=S, Binary}, State) ->
    M = maps:update_with(key(S), fun(L) -> [part(Binary, 1)|L] end, State#state.fragments),
    {noreply, State#state{fragments = M}};
handle_info({#signal{fragment_info=1}=S, Binary}, State) ->
    M = maps:put(key(S), [part(Binary, 1)], State#state.fragments),
    {noreply, State#state{fragments = M}};
handle_info(timeout, Args) ->
    initialized(Args);
handle_info({'EXIT', _Pid, Reason}, State) ->
    {stop, Reason, State}.

%% == internal ==

change(OldVsn, #state{module=M, data=D}=X, Extra) ->
    case M:code_change(OldVsn, D, Extra) of
        {ok, Data} ->
            {ok, X#state{data = Data}};
        {error, Reason} ->
            {error, Reason}
    end.

cleanup(Reason, #state{data=D}=X)
  when D =/= undefined ->
    _ = apply(X#state.module, terminate, [Reason, D]),
    cleanup(Reason, X#state{data = undefined});
cleanup(Reason, #state{ets=E}=X)
  when E =/= undefined ->
    catch true = baseline_ets:delete(E, X#state.block_no),
    cleanup(Reason, X#state{ets = undefined});
cleanup(_, _) ->
    baseline:flush().

setup(Args) ->
    false = process_flag(trap_exit, true),
    {ok, Args, 0}.


initialized([Module, BlockNo, Args]) ->
    case baseline_app:find(ndbepi_sup, ndbepi_ets, 100, 10) of
        undefined ->
            {stop, not_found, undefined};
        Pid ->
            found(Args, #state{module = Module, block_no = BlockNo,
                               fragments = maps:new(), ets = Pid})
    end.

found(Args, #state{block_no=B, ets=E}=X) ->
    case baseline_ets:insert_new(E, {B, self()}) of
        true ->
            configured(Args, X#state{tab = baseline_ets:tab(E)});
        false ->
            {stop, ebusy, X}
    end.

configured(Args, #state{module=M}=X) ->
    case M:init(Args) of
        {ok, Data} ->
            {noreply, X#state{data = Data}};
        {stop, Reason} ->
            {stop, Reason, X}
    end.


ready({A, L, N}, From, #state{module=M, block_no=B, tab=T, data=D}=X) ->
    try select(T, B, N) of
        {_, P, S} ->
            case M:handle_call({A, L}, S, D) of
                {noreply, Args, Data} ->
                    ok = apply(ndbepi_transporter, cast, [P|Args]),
                    {noreply, X#state{data = Data, from = From}};
                {error, Reason, Data} ->
                    {stop, Reason, X#state{data = Data}}
            end
    catch
        error:Reason ->
            {stop, Reason, X}
    end.


received(Signal, Binary, #state{module=M, data=D, from=F}=X) ->
    case M:handle_info(Signal, Binary, D) of
        {reply, Reply, Data} when F =/= undefined ->
            _ = gen_server:reply(F, Reply),
            {noreply, X#state{data = Data, from = undefined}};
        {noreply, Data} ->
            {noreply, X#state{data = Data, from = undefined}};
        {stop, Reason, Data} ->
            {stop, Reason, X#state{data = Data, from = undefined}}
    end.


key(#signal{signal_data_length=L, signal_data=D}) ->
    lists:nth(L, D).

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
