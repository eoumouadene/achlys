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
         daemon/4,
         declare_loop/3,
         mean_compute/4,
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
            LEDs = [1, 2],
            Name = erlang:node(),
            Number = 2,
            Id = lists:nth(2,string:split(lists:nth(1,string:split(atom_to_list(Name),"@")), "s")),
            
            io:format("Init ~n"),
            Buffer = declare_loop([], Number, 1),

            [grisp_led:color(L, red) || L <- LEDs],
            io:format("Launching the daemon ~n"),
            daemon(10000, Id, Buffer, Number),
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

daemon(Sleep, Id, Buffer, Number) -> 
    io:format("DAAAAEEEMON ~n"),
    
    Temperature = rand:uniform(20)+12, %Mesure de la temperature
    TemperatureStr = integer_to_list(Temperature),
    Node = lists:nth(list_to_integer(Id),Buffer),
    %io:format("La valeur  ~p ~n", [Value]),
    {ok, {TT, _, _, _}} = lasp:update(Node, {fst, {set, timestamp(), TemperatureStr}}, self()),
    {ok, {T, _, _, _}} = lasp:update(TT, {snd, {set, timestamp(), timestamp()}}, self()),
    {ok, TRes} = lasp:query(T),    
    %io:format("Is : ~p ~n", [TRes]), 
    
    timer:sleep(Sleep),
    Mean = mean_compute(Buffer, Number, 0, 1),
    io:format("computed"),

    timer:sleep(Sleep),
    daemon(Sleep, Id, Buffer, Number).


timestamp() ->
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
    (MegaSecs*1000000000 + Secs*1000 + round(MicroSecs/1000)).


declare_loop(L, Number, Counter) when Number == 0 ->
    io:format("Ended with ~p ~n", [L]),
    L;
declare_loop(L, Number, Counter) when Number > 0 ->
    Type = {state_pair, [state_lwwregister, state_lwwregister]},
    Name = "T"++integer_to_list(Counter),
    Set = {Name, Type},
    {ok, {T, _, _, _}} = lasp:declare(Set, Type),
    declare_loop(erlang:append(L,[T]), Number-1, Counter+1).


mean_compute(Buffer, Number, Mean, Counter) when Counter > Number ->
    io:format("The Buffer is : ~p ~n", [Buffer]),
    io:format("The mean is : ~p ~n", [Mean]),
    Mean;
mean_compute(Buffer, Number, Mean, Counter) when Counter =< Number ->
    Node = lists:nth(Counter,Buffer),
    {ok, Tuple} = lasp:query(Node),
    Value = erlang:element(1,Tuple),
    NewMean = Mean+(list_to_integer(Value)/Number),
    mean_compute(Buffer, Number, NewMean, Counter+1).