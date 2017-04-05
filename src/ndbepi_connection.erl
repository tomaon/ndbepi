-module(ndbepi_connection).

-include("internal.hrl").

-import(ndbepi_util, [find/3]).

%% -- private --
-export([start_link/1]).

-behaviour(gen_server).
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2]).

%% -- internal --
-record(state, {
          block_no  :: pos_integer(),
          block_mgr :: undefined|pid()
         }).

%% == public ==

-spec start_link(pos_integer()) -> {ok, pid()}|{error, _}.
start_link(BlockNo) ->
    gen_server:start_link(?MODULE, [BlockNo], []).

%% -- behaviour: gen_server --

init(Args) ->
    setup(Args).

terminate(_Reason, State) ->
    cleanup(State).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call(_Request, _From, State) ->
    {stop, enosys, State}.

handle_cast(_Request, State) ->
    {stop, enosys, State}.

%% handle_info(#signal{}=S, State) ->
%%     received(S, State);
%% handle_info(timeout, Args) ->
%%     initialized(Args);
handle_info({'EXIT', _Pid, Reason}, State) ->
    {stop, Reason, State}.

%% == internal ==

cleanup(#state{block_no=N, block_mgr=M}=X)
  when M =/= undefined ->
    catch true = baseline_ets:delete(M, N),
    cleanup(X#state{block_mgr = undefined});
cleanup(_) ->
    baseline:flush().

setup([BlockNo]) ->
    false = process_flag(trap_exit, true),
    loaded(#state{block_no = BlockNo}).


loaded(#state{block_no=N}=X) ->
    case find(ndbepi_block_mgr, 100, 10) of
        {ok, Pid} ->
            try baseline_ets:insert_new(Pid, {N, self()}) of
                true ->
                    initialized(X#state{block_mgr = Pid});
                false ->
                    {stop, ebusy}
            catch
                error:Reason ->
                    {stop, Reason}
            end;
        {error, Reason} ->
            {stop, Reason}
    end.

initialized(#state{}=X) ->
    {ok, X}.
