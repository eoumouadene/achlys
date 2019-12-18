%%%-------------------------------------------------------------------
%%% @author Elias Oumouadene and Gregory Creupelandt
%%% Created : 17 December 2019
%%%-------------------------------------------------------------------
-module(achlys_app).
-author("Igor Kopestenski <igor.kopestenski@uclouvain.be>, Elias Oumouadene <elias.oumouadene@student.uclouvain.be>, Gregory Crepeulandt <gregory.creupelandt@student.uclouvain.be>").

-behaviour(application).

%% Application callbacks
-export([start/2 ,
         stop/1,
         daemon/5,
         declare_loop/3,
         mean_compute/9,
         timestamp/0,
         get_GlobalMean/0]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started.
%% It will init some variables, declare the lasp distributed variables
%% then launch the daemon.
%% @end
%%--------------------------------------------------------------------
-spec(start(StartType :: normal | {takeover , node()} | {failover , node()} ,
            StartArgs :: term()) ->
               {ok , pid()} |
               {error , Reason :: term()}).
start(_StartType , _StartArgs) ->
    case achlys_sup:start_link() of
        {ok , Pid} ->
            io:format("Starting ~n"),
            {ok, _} = application:ensure_all_started(grisp),
            Name = erlang:node(),
            NumberOfNodes = 10, %This is the maximum number of GRISP boards
				% (or emulations) you can launch (limited to avoid declaring too many useless variables)
				%If you want to use this application in a bigger context, you can put a higher value

            Id = lists:nth(2,string:split(lists:nth(1,string:split(atom_to_list(Name),"@")), "s")),
            io:format("My Id is : ~p ~n", [Id]),            
            io:format("Init ~n"),
            GMType = {state_pair, [state_lwwregister, state_lwwregister]},
            GMName = "GlobalMean",
            GMSet = {GMName, GMType},
            {ok, {GlobalMean, _, _, _}} = lasp:declare(GMSet, GMType), %GlobalMean that will only be updated by the CHEF node
            Buffer = declare_loop([], NumberOfNodes, 1),
            io:format("Launching the daemon ~n"),
            daemon(5000, Id, Buffer, NumberOfNodes, GlobalMean),
            {ok , Pid};
        Error ->
            {error, Error}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(stop(State :: term()) -> ok).
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================


daemon(Sleep, Id, Buffer, NumberOfNodes, GlobalMean) -> 
%% This daemon will run during the entire lifetime of the node, measuring temperature
%% and computing averages on a regular time basis.
	
    io:format("~n__New round__~n"),
    
    Temperature = rand:uniform(7)+15, %This represents the measured temperature
				      %Going from 15 to 22 degrees for representative reasons
    TemperatureStr = integer_to_list(Temperature),
    MyNode = lists:nth(list_to_integer(Id),Buffer), %We know what variable we have to update based on our Id
    {ok, {TT, _, _, _}} = lasp:update(MyNode, {fst, {set, timestamp(), TemperatureStr}}, self()),
    lasp:update(TT, {snd, {set, timestamp(), timestamp()}}, self()),
    io:format("  Local Temperature: ~p ~n", [Temperature]), 

    FirstSleep =  round(Sleep/3),
    timer:sleep(FirstSleep), 
    % After a small sleep, computes the temperature mean based on all the valid nodes
    Mean = mean_compute(Sleep, Id, GlobalMean, Buffer, NumberOfNodes, 0, 1, 0, 0),
    io:format("  Average temperature I computed : ~p ~n", [Mean]),

    SecondSleep = 2 * round(Sleep/3),
    timer:sleep(SecondSleep), % After a longer sleep, the daemon is looping
    daemon(Sleep, Id, Buffer, NumberOfNodes, GlobalMean).


%% Simply return the timestamp in a conveniant format (ms)
timestamp() -> 
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
    (MegaSecs*1000000000 + Secs*1000 + round(MicroSecs/1000)).


%% Declare all distributed variable (T1, T2, T3,...) representing Temperatures from other nodes
declare_loop(L, NumberOfNodesToDeclare, Counter) when NumberOfNodesToDeclare == 0 ->
    L;
declare_loop(L, NumberOfNodesToDeclare, Counter) when NumberOfNodesToDeclare > 0 ->
    Type = {state_pair, [state_lwwregister, state_lwwregister]},
    Name = "T"++integer_to_list(Counter),
    Set = {Name, Type},
    {ok, {T, _, _, _}} = lasp:declare(Set, Type),
    declare_loop(erlang:append(L,[T]), NumberOfNodesToDeclare-1, Counter+1).


%% Compute the mean based on the Temperatures from other valid nodes. 
%% Crashed or not-yet-initiated nodes will not be taken in consideration for the mean computation.
%% Only the current leader (chef) will push its computed average to the distributed GlobalMean variable.
%% If the chef do not push recent values (for 3 "sleep periods"), a new leader will be elected.
mean_compute(Sleep, Id, GlobalMean, Buffer, NumberOfNodes, Mean, Counter, CounterValid, Chef) when Counter > NumberOfNodes ->
    % All the Nodes have been taken in count, we can compute the mean.
    io:format("*** The Number of Valid node is : ~p~n", [CounterValid]),
    Return = round(Mean/CounterValid),
    ReturnStr = integer_to_list(Return),
    IdInt = list_to_integer(Id),
    if (Chef == IdInt ) or (Chef == 0) ->
    % If I am the chef or no chef exist (all the nodes were considered not valid), I push my mean as the current GlobalMean
    io:format("*** I AM THE CHEF ~n"),
    {ok, {Global, _, _, _}} = lasp:update(GlobalMean, {fst, {set, timestamp(), ReturnStr}}, self()),
    lasp:update(Global, {snd, {set, timestamp(), timestamp()}}, self()),
    io:format("*** My mean as a chef is : ~p ~n", [Return]),
    Return;
    true ->
        Return
    end;
mean_compute(Sleep, Id, GlobalMean, Buffer, NumberOfNodes, Mean, Counter, CounterValid, Chef) when Counter =< NumberOfNodes ->
    % Gradually add all the temperatures from the other valid nodes (that has pushed a temperature recently).
    % If a node has not been initialized yet, it will not be taken in consideration
    % If no chef has been elected, a valid node that has recently pushed a temperature is elected (based on smaller Id).
    Node = lists:nth(Counter,Buffer),
    {ok, Tuple} = lasp:query(Node),

    case erlang:element(1,Tuple) of 
	undefined -> 
        %io:format("    Warning: I got an undefined temperature from the query ~p probably not initialized ~n", [erlang:element(1,Node)]),
        NewCounterValid = CounterValid,
        NewChef = Chef, % chef does not change (this node will not be elected)
        NewMean = Mean; % mean does not change (this node is not taken in consideration for the mean)
	Value ->
        Time_diff = timestamp() - erlang:element(2,Tuple), % We take the elasped time since last update from this node
        if Time_diff < (Sleep*3) -> % The queried node has recently pushed a temperature, it will be taken in consideration
				    % for mean computation and potentially for leader election      
            
            if Chef == 0 -> % If no chef yet and this node was recently active, it becomes the chef (leader)
                NewChef = list_to_integer(lists:nth(2,string:split(erlang:element(1,Node),"T"))),
                io:format("    We found the leader :  ~p ~n", [NewChef]);
            true ->
                NewChef = Chef
            end,
            RemoteTemperature = Value,
            NewCounterValid = CounterValid + 1, %This node was initialized and had a temperature
            io:format("    I got the temperature ~p from the query ", [RemoteTemperature]),
            io:format("~p ~n", [erlang:element(1,Node)]),
            NewMean = Mean+(list_to_integer(RemoteTemperature)); %Add temperatures from nodes
        true ->
            io:format("    Warning: I got a too old temperature from the query ~p ~n", [erlang:element(1,Node)]),
            NewCounterValid = CounterValid,
            NewChef = Chef, % chef does not change (this node will not be elected)
            NewMean = Mean % mean does not change (this node is not taken in consideration for the mean)
        end
    end,
    mean_compute(Sleep, Id, GlobalMean, Buffer, NumberOfNodes, NewMean, Counter+1, NewCounterValid, NewChef).


get_GlobalMean() ->
    GMType = {state_pair, [state_lwwregister, state_lwwregister]},
    GMName = "GlobalMean",
    GMSet = {GMName, GMType},
    {ok, Tuple} = lasp:query(GMSet),
    erlang:element(1,Tuple).