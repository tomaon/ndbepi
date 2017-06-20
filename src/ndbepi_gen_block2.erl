-module(ndbepi_gen_block2).

-include("internal.hrl").

-import(ndbepi_util, [part/2]).

%% -- private --
-export([start_link/4]).
-export([call/2, call/3, cast/2]).

-behaviour(gen_server).
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2]).

-callback init() ->
    {ok, Data :: term()}|
    {stop, Reason :: term()}.

-callback terminate(Reason :: term(), Data :: term()) ->
    term().

-callback code_change(OldVsn :: term(), Data :: term(), Extra :: term()) ->
    {ok, NewData :: term()}|
    {error, Reason :: term()}.

-callback handle_call(Request :: term(), NodeId :: node_id(), BlockNo :: block_no(),
                      Signal :: signal(), Data :: term()) ->
    {noreply, Args :: [term()], NewData :: term()}|
    {stop, Reason :: term(), NewData :: term()}.

-callback handle_info(Signal :: signal(), Binary :: binary(), Data :: term()) ->
    {reply, Reply :: term(), NewData :: term()}|
    {noreply, NewData :: term()}|
    {stop, Reason :: term(), NewData :: term()}.

%% -- internal --
-record(state, {
          module    :: module(),
          node_id   :: node_id(),
          block_no  :: block_no(),
          fragments :: map(),
          ets       :: undefined|pid(),
          tab       :: undefined|ets:tab(),
          data      :: undefined|term(),
          from      :: undefined|{pid(), term()}
         }).

-type(request() :: {atom(), [term()], undefined|node_id()}).

%% == private ==

-spec start_link(module(), node_id(), block_no(), [term()]) -> {ok, pid()}|{error, _}.
start_link(Module, NodeId, BlockNo, Options) ->
    gen_server:start_link(?MODULE, [Module, NodeId, BlockNo], Options).


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
    loaded(Args).


loaded([Module, NodeId, BlockNo]) ->
    case baseline_app:find(ndbepi_sup, ndbepi_ets, 100, 1) of
        undefined ->
            {stop, not_found};
        Pid ->
            found(#state{module = Module, node_id = NodeId, block_no = BlockNo,
                         fragments = maps:new(), ets = Pid})
    end.

found(#state{block_no=B, ets=E}=X) ->
    case baseline_ets:insert_new(E, {B, self()}) of
        true ->
            configured(X#state{tab = baseline_ets:tab(E)});
        false ->
            {stop, ebusy} % -> retry
    end.

configured(#state{module=M}=X) ->
    case M:init() of
        {ok, Data} ->
            {ok, X#state{data = Data}};
        {stop, Reason} ->
            {stop, Reason}
    end.


ready({A, L, K}, From, #state{module=M, node_id=N, block_no=B, tab=T, data=D}=X) ->
    try select(T, K, B) of
        {_, P, S} ->
            case M:handle_call({A, L}, N, B, S, D) of
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


received(Signal, Binary, #state{module=M, data=D}=X) ->
    case M:handle_info(Signal, Binary, D) of
        {reply, Reply, Data} ->
            _ = gen_server:reply(X#state.from, Reply),
            {noreply, X#state{data = Data, from = undefined}};
        {noreply, Data} ->
            {noreply, X#state{data = Data, from = undefined}};
        {stop, Reason, Data} ->
            {stop, Reason, X#state{data = Data, from = undefined}}
    end.


key(#signal{signal_data_length=L, signal_data=D}) ->
    lists:nth(L, D).

select(Tab, 0, BlockNo) ->
    case ets:select(Tab, [{{'$1', '_', '_'}, [], ['$_']}]) of
        [] ->
            error(badarg);
        List ->
            lists:nth(1 + BlockNo rem length(List), List)
    end;
select(Tab, NodeId, BlockNo) ->
    case ets:select(Tab, [{{'$1', '_', '_'}, [{'=', '$1', NodeId}], ['$_']}]) of
        [] ->
            select(Tab, undefined, BlockNo);
        List ->
            hd(List)
    end.
