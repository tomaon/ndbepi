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
                                                        {rest_for_one, 2, 10},
                                                        get_childspecs(Mgmepi)
                                                      }
                                                     ]}
                                              ])
            catch
                Reason ->
                    {error, Reason}
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
    get_childspecs(Mgmepi, baseline_app:get_all_env()).

get_childspecs(Mgmepi, Env) ->
    case mgmepi:alloc_node_id(Mgmepi,
                              get_value(name, Env, <<"ndbepi">>),
                              get_value(node_id, Env, 0)) of
        {ok, NodeId} ->
            case mgmepi_config:get_config(Mgmepi, NodeId) of
                {ok, Config} ->
                    get_childspecs(Env, NodeId, Config);
                {error, Reason} ->
                    throw(Reason)
            end;
        {error, Reason} ->
            throw(Reason)
    end.

get_childspecs(Env, NodeId, Config) ->
    [
     {
       ndbepi_ets,
       {
         baseline_ets,
         start_link,
         [
          ndbepi_ets,
          [
           {read_concurrency, true}
          ]
         ]
       },
       permanent,
       5000,
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
            {one_for_all, 1, 5},
            get_childspecs(Env, NodeId, Config, get_byte_order())
          }
         ]
       },
       transient,
       5000,
       supervisor,
       []
     },
     {
       ndbepi_cluster_mgr,
       {
         ndbepi_cluster_mgr,
         start_link,
         [
          NodeId,
          ?API_CLUSTERMGR
         ]
       },
       transient,
       5000,
       worker,
       []
     },
     {
       ndbepi_connections,
       {
         supervisor,
         start_link,
         [
          baseline_app,
          {
            {simple_one_for_one, 1, 5},
            [
             {
               undefined,
               {
                 ndbepi_connection,
                 start_link,
                 [
                  NodeId
                 ]
               },
               temporary,
               5000,
               worker,
               []
             }
            ]
          }
         ]
       },
       transient,
       5000,
       supervisor,
       []
     }
    ].

get_childspecs(Env, NodeId, Config, ByteOrder) ->
    [ get_childspec(NodeId, Env, ByteOrder, get_node_config(Config, NodeId), E) ||
        E <- get_connection_config(Config, NodeId) ].


get_childspec(Local, Env, ByteOrder, Nodes, Connections) ->
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
          Local,
          Remote,
          get_value(?CFG_DB_API_HEARTBEAT_INTERVAL, Nodes, 1500),
          #signal{
             byte_order         = ByteOrder,
             signal_id_included = get_value(?CFG_CONNECTION_SEND_SIGNAL_ID, Connections, 0),
             checksum_included  = get_value(?CFG_CONNECTION_CHECKSUM, Connections, 0)
            },
          [
           get_value_as_list(?CFG_CONNECTION_HOSTNAME_2, Connections, "localhost"),
           abs(get_value(?CFG_CONNECTION_SERVER_PORT, Connections, 0)), % < 0, dynamic
           [
            {active, false},
            {buffer, SndBuf + RecBuf},
            {linger, {false, 0}},
            {mode, binary},
            {packet, raw},
            {recbuf, RecBuf},
            {sndbuf, SndBuf}
           ],
           get_value(timeout, Env, 3000)
          ]
         ],
         [
          {spawn_opt, get_value(spawn_opt, Env, [])}
         ]
        ]
      },
      temporary,
      5000,
      worker,
      []
    }.


get_byte_order() ->
    case baseline_app:endianness() of little -> 0; big -> 1 end.

get_connection_config(Config, NodeId) ->
    mgmepi_config:get_connection_config(Config, NodeId).

get_node_config(Config, NodeId) ->
    hd(mgmepi_config:get_node_config(Config, NodeId)).

get_value(Key, List, Default) ->
    baseline_lists:get_value(Key, List, Default).

get_value_as_list(Key, List, Default) ->
    baseline_lists:get_value_as_list(Key, List, Default).
