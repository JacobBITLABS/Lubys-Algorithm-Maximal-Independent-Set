%%----------------------------------------------------------------------------
%% @doc The module provides facilities for simulating algorithms and systems 
%% with a fixed overlay topology.
%% @author peressotti@imada.sdu.dk
%% @end
%%----------------------------------------------------------------------------
-module(simulator).
-export([
  start/2,start/3,
  start_ring/2,start_ring/3,
  start_complete/2,start_complete/3,
  stop/1,kill/2,pid_of/2
]).
-export_type([simulation/1]).

-import_all(simulator_util).

-define(API_TIMEOUT,1000).

-opaque( simulation(_X) :: pid() 
                         | reference() 
                         | port() 
                         | (RegName :: atom()) 
                         | {RegName :: atom(), Node :: node()} ).

-type( graph(X) :: simulator_util:graph(X) ).
%% A directed graph given as adjacency map. The adjacency map is a 
%% list of pairs where the first component represents the vertex (e.g. an 
%% integer) and the second component is the list of vertices in its 
%% neighbourhood.


-type( behaviour(X) :: fun( (X,[X],[{X,pid()}]) -> any() )).
%% A function to be evaluated at each vertex in the simulation. The function 
%% takes the vertex it is evaluating the function, the list of its neighbours, 
%% and a list of vertices and their corresponding pid.


%%----------------------------------------------------------------------------
%% @doc Starts a simulation over the given graph with a process of each vertex.
%%   Processes are connected according to the graph and each evaluates the given
%%   function.
%% @param G is a directed graph.
%% @param B is function to be evaluated at each vertex. 
%% @end
%%----------------------------------------------------------------------------
-spec( start( graph(X), behaviour(X) ) -> simulation(X) ).
start(G,B) -> 
  spawn(fun() -> 
    process_flag(trap_exit, true),
    % starts a process for each vertex in the graph and build a map associating vertexes to pids
    VP = [ { V, spawn_link(fun() -> simulation(V,B) end) } || { V, _ } <- G ],
    % send to each provess its neighbourhood and the vertex-pid map
    lists:foreach( fun( { V, N } ) -> 
      {V,P} = lists:keyfind( V,1,VP),
      P ! { setup, N, VP }
    end, G),
    lists:foreach( fun( { _, P } ) -> 
      P ! start
    end, VP),
    % monitor loop
    monitor_loop(VP)
  end).

%%----------------------------------------------------------------------------
%% @doc Starts a simulation over the given graph with a process of each vertex.
%%   Processes are connected according to the graph and each evaluates the given
%%   function. Registers the name RegName with the simulation manager.
%% @param G is a directed graph.
%% @param B is function to be evaluated at each vertex. 
%% @param Regname the name to register.
%% @see start/2
%% @end
%%----------------------------------------------------------------------------
-spec( start_register( graph(X), behaviour(X), atom() ) -> simulation(X) ).
start_register(G,B,RegName) ->
  Pid = start(G,B),
  register(RegName,Pid),
  Pid.

%%----------------------------------------------------------------------------
%% @doc Starts a simulation over the given graph with a process of each vertex.
%%   Processes are connected according to the graph and each evaluates the given
%%   function.
%% @param G is a directed graph.
%% @param M is a module.
%% @param B is a function in M to be evaluated at each vertex.
%% @see start/2
%% @end
%%----------------------------------------------------------------------------
-spec( start( graph(X), module(), atom() ) -> simulation(X) ).
start(G, M, B) ->
  start(G,fun(V,Ns,VPs) -> apply(M,B,[V,Ns,VPs]) end).

%%----------------------------------------------------------------------------
%% @doc Starts a simulation over the given graph with a process of each vertex.
%%   Processes are connected according to the graph and each evaluates the given
%%   function.
%% @param G is a directed graph.
%% @param M is a module.
%% @param B is a function in M to be evaluated at each vertex.
%% @param Regname the name to register.
%% @see start/3
%% @end
%%----------------------------------------------------------------------------
-spec( start_register( graph(X), module(), atom(), atom() ) -> simulation(X) ).
start_register(G, M, B, RegName) ->
  start_register(G,fun(V,Ns,VPs) -> apply(M,B,[V,Ns,VPs]) end, RegName).


%%----------------------------------------------------------------------------
%% @doc Starts a simulation over a ring 
%% @see start/2.
%% @param N the number of vertices in the graph.
%% @end
%%----------------------------------------------------------------------------
-spec( start_ring( integer(), behaviour(integer()) ) -> simulation( integer() ) ).
start_ring(N, B) ->
  start(simulator_util:ring(N),B).

%%----------------------------------------------------------------------------
%% @doc Starts a simulation over a ring 
%% @see start/3.
%% @param N the number of vertices in the graph.
%% @end
%%----------------------------------------------------------------------------
-spec( start_ring( integer(), module(), atom() ) -> simulation(integer()) ).
start_ring(N, M, B) ->
  start(simulator_util:ring(N),M,B).

%%----------------------------------------------------------------------------
%% @doc Starts a simulation over a complete graph 
%% @see start/2.
%% @param N the number of vertices in the graph.
%% @end
%%----------------------------------------------------------------------------
-spec( start_complete( integer(), behaviour(integer()) ) -> simulation( integer() ) ).
start_complete(N, B) ->
  start(simulator_util:complete_graph(N),B).

%%----------------------------------------------------------------------------
%% @doc Starts a simulation over a complete graph 
%% @see start/3.
%% @param N the number of vertices in the graph.
%% @end
%%----------------------------------------------------------------------------
-spec( start_complete( integer(), module(), atom() ) -> simulation( integer() ) ).
start_complete(N, M, B) ->
  start(simulator_util:complete_graph(N),M,B).

%%----------------------------------------------------------------------------
%% @doc Stops the simulation.
%% @end
%%----------------------------------------------------------------------------
-spec( stop(simulation(_)) -> ok ).
stop(S) -> S ! stop, ok.

%%----------------------------------------------------------------------------
%% @doc Termiantes the process associated to the given vertex.
%% @end
%%----------------------------------------------------------------------------
-spec( kill(X,simulation(X)) -> ok ).
kill(V,S) -> S ! { kill, V }, ok.

%%----------------------------------------------------------------------------
%% @doc returns the process associated to the given vertex.
%% @end
%%----------------------------------------------------------------------------
-spec( pid_of(X,simulation(X)) -> { ok, pid() } | { error, _ } ).
pid_of(V,S) -> 
  Ref = erlang:make_ref(),
  S ! { pid_of_req, Ref, V, self() },
  receive
    { pid_of_resp, Ref, Result } -> Result
  after ?API_TIMEOUT ->
    { error, timeout }
  end.
  
simulation(V, B) -> 
  receive
    { setup, Ns, Ps } -> 
      io:format("~p: neighbours: ~p~n",[V,Ns]),
      receive
        start -> 
          io:format("~p: ready!~n",[V]),
          timer:sleep(100),
          R = B(V, Ns, Ps),
          exit({ result, R })
      end
  end.

monitor_loop([]) ->
  io:format("simulation stopped: no nodes running.~n", []),
  exit(ok);
monitor_loop(VP) -> 
  receive
    { kill,V} ->
      case lists:keyfind(V,1,VP) of
        { V, Pid } -> exit(Pid,kill);
        false -> ok
      end,
      monitor_loop(VP);
    { pid_of_req, Ref, V, From } -> 
      Resp = case lists:keyfind(V,1,VP) of
        { _, Pid } -> {ok, Pid};
        false -> {error, no_pid}
      end,   
      From ! { pid_of_resp, Ref, Resp },   
      monitor_loop(VP);
    { 'EXIT', Pid, Reason } ->
      { V, Pid } = lists:keyfind(Pid,2,VP),
      case Reason of
        killed -> 
          io:format("~p: killed~n", [V]);
        normal -> 
          io:format("~p: terminated~n", [V]);
        { result, R } -> 
          io:format("~p: terminated with result: ~p~n", [V,R]);
        _ -> 
          io:format("~p: stopped with reason: ~p~n", [V,Reason]),
          stop(self())
      end,
      monitor_loop(lists:keydelete(V,1,VP));
    stop ->
      io:format("simulation stopped: request~n"),
      exit(stop)
  end.