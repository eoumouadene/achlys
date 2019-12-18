%%%-------------------------------------------------------------------
%%% @author Igor Kopestenski <igor.kopestenski@uclouvain.be>
%%%     [https://github.com/achlysproject/achlys]
%%% @doc
%%% The Achlys OTP application module
%%% @end
%%% Created : 06. Nov 2018 20:16
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
         timestamp/0]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start(StartType :: normal | {takeover , node()} | {failover , node()} ,
            StartArgs :: term()) ->
               {ok , pid()} |
               {error , Reason :: term()}).
start(_StartType , _StartArgs) ->
    case achlys_sup:start_link() of
        {ok , Pid} ->
            % For test purposes, grisp_app allows calls to emulated pmod_nav
            % in an Erlang shell when the "drivers" configuration parameter specifies
            % only elements with the "_emu" suffix for each slot.
            % Once the LEDs have turned red,
            % the supervisor has been initialized.
            io:format("Starting ~n"),
            {ok, _} = application:ensure_all_started(grisp),
            %LEDs = [1, 2],
            Name = erlang:node(),
            NumberOfNodes = 2,
            Id = lists:nth(2,string:split(lists:nth(1,string:split(atom_to_list(Name),"@")), "s")),
            
            io:format("Init ~n"),
            GMType = {state_pair, [state_lwwregister, state_lwwregister]},
            GMName = "GlobalMean",
            GMSet = {GMName, GMType},
            {ok, {GlobalMean, _, _, _}} = lasp:declare(GMSet, GMType),
            Buffer = declare_loop([], NumberOfNodes, 1),

            %[grisp_led:color(L, red) || L <- LEDs],

	    io:format("My Id is : ~p ~n", [Id]),
	    io:format("Buffer of distributed variables is : ~p ~n", [Buffer]),
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
    io:format("__New round__~n"),
    
    Temperature = rand:uniform(7)+15, %Mesure de la temperature
    TemperatureStr = integer_to_list(Temperature),
    MyNode = lists:nth(list_to_integer(Id),Buffer),
    %io:format("La valeur  ~p ~n", [Value]),
    {ok, {TT, _, _, _}} = lasp:update(MyNode, {fst, {set, timestamp(), TemperatureStr}}, self()),
    lasp:update(TT, {snd, {set, timestamp(), timestamp()}}, self()),
    io:format("Temperature I measured : ~p ~n", [Temperature]),

    FirstSleep = round(Sleep/3),
    timer:sleep(FirstSleep), 
    Mean = mean_compute(Sleep, Id, GlobalMean, Buffer, NumberOfNodes, 0, 1, 0, 0),
    io:format("---Average temperature I computed : ~p ---~n", [Mean]),

    timer:sleep(Sleep),
    daemon(Sleep, Id, Buffer, NumberOfNodes, GlobalMean).


timestamp() ->
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
    (MegaSecs*1000000000 + Secs*1000 + round(MicroSecs/1000)).


declare_loop(L, NumberOfNodesToDeclare, Counter) when NumberOfNodesToDeclare == 0 ->
    io:format("Ended with ~p ~n", [L]),
    L;
declare_loop(L, NumberOfNodesToDeclare, Counter) when NumberOfNodesToDeclare > 0 ->
    Type = {state_pair, [state_lwwregister, state_lwwregister]},
    Name = "T"++integer_to_list(Counter),
    Set = {Name, Type},
    {ok, {T, _, _, _}} = lasp:declare(Set, Type),
    declare_loop(erlang:append(L,[T]), NumberOfNodesToDeclare-1, Counter+1).


mean_compute(Sleep, Id, GlobalMean, Buffer, NumberOfNodes, Mean, Counter, CounterValid, Chef) when Counter > NumberOfNodes ->
    io:format("The Number of Valid node is : ~p ~n", [CounterValid]),
    Return = round(Mean/CounterValid),
    ReturnStr = integer_to_list(Return),
    if (Chef == Id) or (Chef == 0) ->
    io:format("I AM THE CHEF"),
    {ok, {Global, _, _, _}} = lasp:update(GlobalMean, {fst, {set, timestamp(), ReturnStr}}, self()),
    lasp:update(Global, {snd, {set, timestamp(), timestamp()}}, self()),
    Return;
    true ->
        Return
    end;
mean_compute(Sleep, Id, GlobalMean, Buffer, NumberOfNodes, Mean, Counter, CounterValid, Chef) when Counter =< NumberOfNodes ->
    Node = lists:nth(Counter,Buffer),
    io:format("Lasp variable I want to query : ~p ~n", [erlang:element(1,Node)]),
    {ok, Tuple} = lasp:query(Node),
    case erlang:element(1,Tuple) of 
	undefined -> 
        io:format("--- ERROR: I got an undefined temperature from the query ~n"),
        NewCounterValid = CounterValid,
        NewChef = Chef,
        NewMean = Mean;
	Value ->
        Time_diff = timestamp() - erlang:element(2,Tuple),
        io:format("UnixTime : ~p ~n", [Time_diff]),
        if (Chef == 0) and (Time_diff < (Sleep*3)) ->
            NewChef = list_to_integer(lists:nth(2,string:split(erlang:element(1,Node),"T"))),
            io:format("We found the chef :  ~p ~n ", [NewChef]);
        true ->
            NewChef = Chef
        end,
        RemoteTemperature = Value,
        NewCounterValid = CounterValid + 1,
		io:format("I got the temperature ~p from the query ~n", [RemoteTemperature]),
		NewMean = Mean+(list_to_integer(RemoteTemperature)) 
    end,
    %Value = erlang:element(1,Tuple),
    %io:format("Temperature I got from this query : ~p ~n", [Value]),
    %NewMean = Mean+(list_to_integer(Value)/NumberOfNodes),
    mean_compute(Sleep, Id, GlobalMean, Buffer, NumberOfNodes, NewMean, Counter+1, NewCounterValid, NewChef).
