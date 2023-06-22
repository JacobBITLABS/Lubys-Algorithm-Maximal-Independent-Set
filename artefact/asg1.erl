-module(asg1).                                          % define module
-import(simulator_util, [complete_graph/1]).            % import function from module   
-import(simulator, [start/2]).                          % import function from module   
-export([start/0]).                                     % export start function

% state for the event loop running at each vertex
-record(state, {
    vertex,        
    random,        
    neighbours,
    nodes,
    randoms
    }).
% -type state(X) :: #state{ vertex :: X }.

%%-----------------------------------------------------------------------------------
%% @doc Update process state by keep track of elminated neighbours
%% @param S state of the process 
%% @param Count atom to track the number of recieved msg's. On call initate to 0.
%% @end 
%%----------------------------------------------------------------------------------
get_eliminated_loop( S, Count ) ->
    if  
        length(S#state.neighbours)-1 > Count ->
            % await neighbours 
            receive 
                {eliminated, Vertex, true} ->          
                    % rermove vertex J/Vertex form list of neighbours
                    New_neighbour_lst = lists:delete( Vertex, S#state.neighbours),
                    % recurse
                    get_eliminated_loop( S#state{ neighbours = New_neighbour_lst}, Count+1);

                {eliminated, _, false} ->    
                    % Vertex messaged, not eliminated
                    get_eliminated_loop( S, Count+1 )      
            end;
        true ->
            % all neighbours answered, return state to caller
            S
    end.

%%-------------------------------------------------------------------------------------
%% @doc Awaits the selected messaages from neighbours
%% @param S state of the process 
%% @param Count atom to track the number of recieved msg's. On call initate to 0.
%% @param Selected_list contains the vertex id for processes sending false-msg. init: []
%% @param J_true represents if a neighbour send a {selected, true} msg. init: false
%% @end 
%%-------------------------------------------------------------------------------------
get_selected_loop( S , Count, Selected_list, J_True) ->
    if
        length(S#state.neighbours) > Count ->
            % await selected from each neighbour
            receive
                % snippet 1.m SELECTED true
                { selected, _, true } -> % then
                
               % NewLst = lists:append(Selected_list, [{Vertex, true}]), 
                % get the other calls
                get_selected_loop( S , Count+1, Selected_list, true);
                
                % else / snippet 1.q
                { selected, Vertex, false } ->
                    NewLst = lists:append([Selected_list, [Vertex]]), 
                    % recusrse with updated arguments
                    get_selected_loop( S , Count+1, NewLst, J_True) % J_true default false
            end; 
        true ->
            % return tuple to caller
            {S, Selected_list, J_True}
    end.


%%-------------------------------------------------------------------------------------
%% @doc Awaits the random messaages from neighbours
%% @param S state of the process 
%% @param Count atom to track the number of recieved msg's. On call initate to 0.
%% @end 
%%-------------------------------------------------------------------------------------
get_random_loop( S , Count) ->
    % not all neigbours has answered  -> AWAIT ALL neigbours to answer
    if
        length(S#state.neighbours) > Count ->
            % io:fwrite("Number of neighbours bigger than count \n"),
            receive
                { random, Random_j } -> %
                        get_random_loop( S#state { 
                            randoms = lists:append([S#state.randoms, [Random_j]]) 
                            }, % append to list
                            Count+1
                        )
            end;
        true -> 
            % All neighbours has answered
            % filter list S#state.random > N
            Predicate = fun(E) -> E < S#state.random end,
            Is_smaller_than_r_i = lists:any(Predicate,  S#state.randoms ), % true if at least one E is <
            
            if 
                Is_smaller_than_r_i ->
                    % random_i is not smallets / else -> another random i sammler 
                    % Send SELECTED False to all neighbours. 
                    % Snippet 1.k
                    lists:foreach( fun( N ) ->
                        { N, P } = lists:keyfind( N, 1, S#state.nodes), % find Pid for node
                        % 1.k
                        P ! { selected, S#state.vertex , false } % send message of type SELECRED to process P
                        end,  S#state.neighbours),

                    % Then we need to get new state
                    {New_State, Selected_list, J_True} = get_selected_loop( S , 0, [], false),

                    if 
                        J_True -> 
                            % there was a SELECTED True from some Neighbour
                            % look in Selected_list, ELIMINATED TRUE, to all that send false
                            % % 1.n-1.o
                
                            lists:foreach(fun( N ) ->
                                 { N, P } = lists:keyfind( N, 1, S#state.nodes),
                                 P ! { eliminated, S#state.vertex , true } % send back to j
                            end, Selected_list),

                            % eliminated true 
                            io:fwrite("Vertex: ~w NOT in MIS\n", [S#state.vertex]);
                
                        % else 1.q
                        true ->
                            % No SELECTED True from any Neighbour
                           lists:foreach( fun( N ) -> 
                                { N, P } = lists:keyfind( N, 1, S#state.nodes),
                                P ! { eliminated, S#state.vertex, false}                % message to each neighbour
                            end, S#state.neighbours ),
                            
                            New_State = get_eliminated_loop( S, 0),

                            % recursion 
                            get_random_loop(New_State, Count+1)
                    end;
                % else 
                true ->
                    % random_i is smallets all is greater
                    io:fwrite("Vertex: ~w in MIS\n", [S#state.vertex]),

                    % Send SELECTED TRUE to all neighbours 
                    lists:foreach( fun( N ) ->
                        { N, P } = lists:keyfind( N, 1, S#state.nodes ), % find Pid for node
                        P ! { selected, S#state.vertex , true } % send message of type SELECRED to process P
                        end,  S#state.neighbours)
        end
    end.


%%-------------------------------------------------------
%% @doc Runs the algorithm itself
%% @param S state of the process 
%% @end 
%%-------------------------------------------------------
mis( S ) ->
    if 
        length(S#state.neighbours) == 0 ->
            % change the state of processj
            io:fwrite("Only one vertex in graph: ~w\n", [S#state.vertex]);
            ok;
        true ->
            % get generated random real
            Random_i = S#state.random,

            % Send random to all neighbours 
            lists:foreach( fun( N ) ->
                { N, P } = lists:keyfind( N, 1, S#state.nodes), % find Pid for node
                P ! { random, Random_i } % send message of type RANDOM to process P
                %io:fwrite("P ~w, ~w \n ", [P,N])
                end, S#state.neighbours),
                    
            % go to loop
            get_random_loop(S , 0) % get random value from all neighbours

            % % run forever in systems that need to monitor
            %mis( New_State )
    end.

%%-------------------------------------------------------
%% @doc Constructs graph, initial state and call MIS 
%% @param None 
%% @end 
%%-------------------------------------------------------
-spec(start() -> simulator:simulation()).
start() ->
    % Create/ Construct Graph
    % Test 1.a
    % Graph = simulator_util:complete_graph(1),
    
    % Test 1.b
    % Graph = simulator_util:complete_graph(2),
    
    % Test 1.c
    Graph = simulator_util:complete_graph(5),

    % Test 1.d
    %Graph = simulator_util:uring(5),

    % Test 1.e
   % Graph = simulator_util:ring(5),
    
    % Start simulation
    simulator:start(Graph, fun(Vertex, Neighbours, Nodes ) ->
        % Create initial process state state
        S = #state{
            vertex = Vertex,
            random = rand:uniform(1000), % random num for each process
            neighbours = Neighbours, 
            nodes = Nodes,
            randoms = []
        },
        
        % to randomise the simulation, random sleep
        %timer:sleep(100 + rand:uniform(1000)), 
        
        % call MIS function 
        mis( S )
    end).