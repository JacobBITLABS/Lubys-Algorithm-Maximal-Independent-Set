%%----------------------------------------------------------------------------
%% @doc The module provides functions for creating graphs that represent 
%% common types of overlay topologies. Graphs are directed and represented 
%% using adjacency lists.
%% @author peressotti@imada.sdu.dk
%% @end
%%----------------------------------------------------------------------------
-module(simulator_util).
-export([complete_graph/1,complete_biparted/2,ring/1,uring/1,grid/2,toroidal_grid/2,write_to_file/2,read_from_file/1]).

-type( graph(V) :: [{ V, [V]}]).
%% A directed graph given as adjacency map. The adjacency map is a 
%% list of pairs where the first component represents the vertex (e.g. an 
%% integer) and the second component is the list of vertices in its 
%% neighbourhood.

%%----------------------------------------------------------------------------
%% @doc Creates a complete directed graph with vertices [1,...,N].
%% @end
%%----------------------------------------------------------------------------
-spec(complete_graph(integer()) -> graph(integer())).
complete_graph(N) ->
  Vs = lists:seq(1,N),
  [ { V, [ X || X <- Vs, X /= V ] } || V <- Vs ].

%%----------------------------------------------------------------------------
%% @doc Creates a complete biparted directed graph with vertices 
%% [{a,1},...,{a,Na},{b,1},...,{b,Nb}].
%% @param Na number of 'a'-vertices
%% @param Nb number of 'b'-vertices
%% @end
%%----------------------------------------------------------------------------
-spec(complete_biparted(integer(),integer()) -> graph({ a | b ,integer()})).
complete_biparted(Na,Nb) ->
  As = [ { a, V } || V <- lists:seq(1,Na) ],
  Bs = [ { b, V } || V <- lists:seq(1,Nb) ],
  [ { A, Bs } || A <- As ] ++ [ { B, As } || B <- Bs ].

%%----------------------------------------------------------------------------
%% @doc Creates a directed ring with vertices [1,...,N].
%% @end
%%----------------------------------------------------------------------------
-spec(ring(integer()) -> graph(integer())).
ring(N) ->
  [ { V, [ V rem N + 1 ] } || V <- lists:seq(1,N) ].

%%----------------------------------------------------------------------------
%% @doc Creates an undirected ring with vertices [1,...,N].
%% @end
%%----------------------------------------------------------------------------
-spec(uring(integer()) -> graph(integer())).
uring(N) ->
  [ { V, [ V rem N + 1, rem1(V - 1,N)] } || V <- lists:seq(1,N) ].

%%----------------------------------------------------------------------------
%% @doc Creates a N by M grid with vertices [{1,1},...,{N,M}].
%% @end
%%----------------------------------------------------------------------------
-spec(grid(integer(),integer()) -> graph({integer(),integer()})).
grid(N,M) -> 
  Vs = [ {X,Y} || X <- lists:seq(1,N), Y <- lists:seq(1,M)],
  Neighbours = fun(X,Y) ->
      All = [  
        {   X, Y+1 }, % north
        {   X, Y-1 }, % south
        { X+1,   Y }, % east
        { X-1,   Y }  % west
      ],
      lists:filter( fun( {A,B} ) -> 
        (0 < A) and (A =< N) and (0 < B) and (B =< M) 
      end, All)
    end,
  [ { {X,Y}, Neighbours( X, Y ) } || {X,Y} <- Vs ].

%%----------------------------------------------------------------------------
%% @doc Creates a N by M toroidal grid with vertices [{1,1},...,{N,M}].
%% @end
%%----------------------------------------------------------------------------
-spec(toroidal_grid(integer(),integer()) -> graph({integer(),integer()})).
toroidal_grid(N,M) -> 
  Vs = [ {X,Y} || X <- lists:seq(1,N), Y <- lists:seq(1,M)],
  [ { {X,Y}, [ 
                {          X, rem1(Y+1,M) }, % north
                {          X, rem1(Y-1,M) }, % south
                { rem1(X+1,N),          Y }, % east
                { rem1(X-1,N),          Y }  % west
              ] } || {X,Y} <- Vs ].

% alias for the type of errors returned by file:write_file and file:read_file
-type(file_error() :: file:posix() | badarg | terminated | system_limit).

%%----------------------------------------------------------------------------
%% @doc Writes the graph G to Filename formatted as an Erlang term.
%% @end
%%----------------------------------------------------------------------------
-spec(write_to_file(file:name_all(),graph(_)) -> ok | {error, file_error()}).
write_to_file(Filename,G) -> 
  file:write_file(Filename, io_lib:format("~p.~n", [G])).

%%----------------------------------------------------------------------------
%% @doc Reads an term represenging a graph from Filename.
%% @end
%%----------------------------------------------------------------------------
-spec(read_from_file(file:name_all()) -> { ok, graph(_) } | {error, file_error()}).
read_from_file(Filename) -> 
  case file:consult(Filename) of
    {ok, [G]} -> G;
    E -> E
  end.


%% @hidden
% a `one-off` modulo function
rem1(A,B) -> 
  if
    A > B   -> 1;
    A =:= 0 -> B;
    true    -> A
  end.