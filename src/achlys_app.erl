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
            Id = lists:nth(2,string:split(lists:nth(1,string:split(atom_to_list(Name),"@")), "s")),
            io:format("Init ~n"),
            Type = state_gset,
            Set = {<<"mean">>, Type},
            {ok, {GMean, _, _, _}} = lasp:declare(Set, Type),

            LeftType = state_lwwregister,
	        %RightType = state_gcounter,
            RightType = state_lwwregister,

            Type1 = {state_pair, [LeftType, RightType]},
            Set1 = {<<"T1">>, Type1},
            {ok, {T1, _, _, _}} = lasp:declare(Set1, Type1),


            io:format("Go in Loop ~n"),
            [grisp_led:color(L, red) || L <- LEDs],
            io:format("Launching the daemon ~n"),
            daemon(500, Id, T1, 0),
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

daemon(Sleep, Id, T1, Count) -> 
    Count2 = Count + 1,
    %Count2str = integer_to_list(Count2),

    Temperature = rand:uniform(20)+12, %Mesure de la temperature
    TemperatureStr = integer_to_list(Temperature),

    {ok, {T11, _, _, _}} = lasp:update(T1, {fst, {set, timestamp(), TemperatureStr}}, self()),
    {ok, {T12, _, _, _}} = lasp:update(T11, {snd, {set, timestamp(), timestamp()}}, self()),
    {ok, T1Res} = lasp:query(T12),    
    io:format("~p ~n", [T1Res]),    
    timer:sleep(Sleep),

    

    timer:sleep(Sleep),
    daemon(Sleep, Id, T1, Count2).


timestamp() ->
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
    (MegaSecs*1000000000 + Secs*1000 + round(MicroSecs/1000)).
    
