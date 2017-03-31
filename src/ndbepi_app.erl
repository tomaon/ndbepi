-module(ndbepi_app).

-include("internal.hrl").

%% -- public --
-behaviour(application).
-export([start/2, stop/1]).

%% == public ==

%% -- behaviour: application --

start(StartType, []) ->
    case mgmepi:checkout() of
        {ok, Mgmepi} ->
            try baseline_app:start(StartType, [
                                               {sup, [
                                                      {local, ndbepi_sup},
                                                      {
                                                        {rest_for_one, 10, timer:seconds(1)},
                                                        get_childspecs(Mgmepi)
                                                      }
                                                     ]}
                                              ])
            after
                ok = mgmepi:checkin(Mgmepi)
            end;
        {error, Reason} ->
            {error, Reason}
    end.

stop(State) ->
    baseline_app:stop(State).

%% == internal ==

get_childspecs(Mgmepi) ->
    [
     {
       ndbepi_block_mgr,
       {
         baseline_ets,
         start_link,
         [
          ndbepi_block_mgr,
          [
           {read_concurrency, true}
          ]
         ]
       },
       permanent,
       timer:seconds(5),
       worker,
       []
     },
     {
       ndbepi_cluster_mgr,
       {
         ndbepi_cluster_mgr,
         start_link,
         [
         ]
       },
       transient,
       timer:seconds(5),
       worker,
       []
     },
     {
       ndbepi_transporters,
       {
         supervisor,
         start_link,
         [
          baseline_app,
          {
            {one_for_one, 10, timer:seconds(1)},
            get_childspecs(Mgmepi, baseline_app:get_all_env())
          }
         ]
       },
       transient,
       timer:seconds(5),
       supervisor,
       []
     }
    ].

get_childspecs(Mgmepi, Env) ->
    case alloc_node_id(Mgmepi, Env) of
        {ok, NodeId} ->
            case get_config(Mgmepi, NodeId) of
                {ok, Config} ->
                    ByteOrder = get_byte_order(),
                    [ get_childspec(Env, ByteOrder, get_node_config(Config, E), E) ||
                        E <- get_connection_config(Config, NodeId) ];
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

get_childspec(Env, ByteOrder, Nodes, Connections) ->
    Local  = get_value(?CFG_CONNECTION_NODE_1, Connections, 0),
    Remote = get_value(?CFG_CONNECTION_NODE_2, Connections, 0),
    SndBuf = get_value(?CFG_TCP_SEND_BUFFER_SIZE, Connections, 2097152),
    RecBuf = get_value(?CFG_TCP_RECEIVE_BUFFER_SIZE, Connections, 2097152),
    {
      Remote,
      {
        ndbepi_transporter,
        start_link,
        [
         [
          %% CFG_MGMD_MGMD_HEARTBEAT_INTERVAL?
          get_value(?CFG_DB_API_HEARTBEAT_INTERVAL, Nodes, 1500),
          #signal{ % for 'recv'
             send_node_id       = Remote,
             recv_node_id       = Local,
             byte_order         = ByteOrder,
             signal_id_included = get_value(?CFG_CONNECTION_SEND_SIGNAL_ID, Connections, 0),
             checksum_included  = get_value(?CFG_CONNECTION_CHECKSUM, Connections, 0)
            },
          [
           get_value(?CFG_CONNECTION_HOSTNAME_2, Connections, "localhost"),
           abs(get_value(?CFG_CONNECTION_SERVER_PORT, Connections, 0)), % < 0, dynamic
           [
            {active, false},
            {buffer, SndBuf + RecBuf},
            {keepalive, true},
            {mode, binary},
            {packet, raw},
            {recbuf, RecBuf},
            {sndbuf, SndBuf}
           ],
           get_value(timeout, Env, 3000)
          ]
         ]
        ]
      },
      transient,
      timer:seconds(5),
      worker,
      []
    }.


alloc_node_id(Mgmepi, Env) ->
    mgmepi:alloc_node_id(Mgmepi,
                         get_value(name, Env, <<"ndbepi">>),
                         get_value(node_id, Env, 0)).

get_byte_order() ->
    case baseline_app:endianness() of little -> 0; big -> 1 end.

get_config(Mgmepi, NodeId) ->
    mgmepi_config:get_config(Mgmepi, NodeId).

get_connection_config(Config, NodeId) ->
    mgmepi_config:get_connection_config(Config, NodeId).

get_node_config(Config, Connections) ->
    hd(mgmepi_config:get_node_config(Config, get_value(?CFG_CONNECTION_NODE_2, Connections, 0))).

get_value(Key, List, Default)
  when is_list(Default) ->
    baseline_lists:get_value_as_list(Key, List, Default);
get_value(Key, List, Default) ->
    baseline_lists:get_value(Key, List, Default).

%% {ok,M} = mgmepi:checkout().
%% {ok, L} = mgmepi_config:get_config(M).
%% [ io:format("~p", [E]) || E <- mgmepi_config:debug_node_config(L, 1) ].
%% mgmepi_config:debug_connection_config(L, 201).
