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
         daemon/5]).

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
            Id = lists:nth(2,string:split(lists:nth(1,string:split(atom_to_list(Name),"@")), "s")),
            io:format("Init ~n"),
	    PairLeftType = state_lwwregister.
	    PairRightType = state_gcounter.


            Type1 = {state_pair, [PairLeftType, PairRightType]},
            Set1 = {<<"T1">>, Type1},
            {ok, {T1Pair, _, _, _}}lasp:declare(Set1, Type1),        

            Type2 = {state_pair, [PairLeftType, PairRightType]},
            Set2 = {<<"T2">>, Type2},
            lasp:declare(Set2, Type2),        

            Type3 = {state_pair, [PairLeftType, PairRightType]},
            Set3 = {<<"T3">>, Type3},
            lasp:declare(Set3, Type3),        


            io:format("Go in Loop ~n"),
            [grisp_led:color(L, red) || L <- LEDs],

            daemon(Id, GMean, 0),
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

daemon(Id, T1Pair, T2Pair, T3Pair, Count) -> 
    io:format("I AM THE DAEMON ~n"),
    Count2 = Count + 1,
    Count2str = integer_to_list(Count2),

    Temperature = rand:uniform(40), %Mesure de la temperature
    TemperatureStr = integer_to_list(Temperature),
    if Id == 1 ->

    	{ok, {T1Pair1, _, _, _}} = lasp:update(T1Pair, {fst, {set, Timestamp(), Count2str}}, self()),

    	{ok, T2Res} = lasp:query(T2Pair),
	{ok, T2Res} = lasp:query(T3Pair),
	Mean = Temperature+T2Res+T3Res /3
    end
    if Id == 2 ->

    	{ok, {T2Pair1, _, _, _}} = lasp:update(T2Pair, {fst, {set, Timestamp(), Count2str}}, self()).

	{ok, T1Res} = lasp:query(T1Pair),
	{ok, T3Res} = lasp:query(T3Pair),
	Mean = T1Res+Temperature+T3Res /3
    end
    if Id == 3 ->

    	{ok, {T3Pair1, _, _, _}} = lasp:update(T1Pair, {fst, {set, Timestamp(), Count2str}}, self()).
	{ok, T1Res} = lasp:query(T1Pair),
	{ok, T2Res} = lasp:query(T2Pair),
	Mean = T1Res+T2Res+Temperature /3

    end

    %Updating my temperature on the distributed variable is done


    %io:format(sets:to_list(T1Res)),
    io:format(T1Res),
    io:format("~n"),
    timer:sleep(500),
    daemon(Id, T1Pair1, T2Pair, T3Pair, Mean, Count2).
    
