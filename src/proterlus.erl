%%%-------------------------------------------------------------------
%%% @author podri
%%% @copyright (C) 2019, podri
%%% @doc
%%%
%%% @end
%%% Created : 2019-12-07 04:10:49.617632
%%%-------------------------------------------------------------------
-module(proterlus).

-behaviour(gen_server).

%% API
-export([start_link/0,
				 get_machines/0,
				 configure_machine/2,
				 add_default_machine/1,
				 get_machine_config/1,
				 connect_machine/3,
				 disconnect_machine/3,
				 connections/0,
				 connect/2,
				 new_wire/0
				% add_os/2
				% start_machine/1,
				% stop_machine/1,
				% generate_ethernet_pkg/1,
				% send_ethernet_pkg/1
				]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Configure machines
%%   example:
%%     Config = [{ports, [{eth0, nic, []}, {serial0, serial, []}]},
%%               {os, default_os, []}]
%% @end
%%--------------------------------------------------------------------
configure_machine(Name, Config) ->
  gen_server:call(?MODULE, {configure, Name, Config}).
%%
%%--------------------------------------------------------------------
%% @doc
%% Add machine with default config
%% @end
%%--------------------------------------------------------------------
add_default_machine(Name) ->
	DefConf = [{ports, [{eth0, nic, []}, {serial0, serial, []}]},
             {os, {default_os, []}}],
	?MODULE:configure_machine(Name, DefConf).

%%--------------------------------------------------------------------
%% @doc
%% Get machine names
%% @end
%%--------------------------------------------------------------------
get_machines() ->
  gen_server:call(?MODULE, machines).

%%--------------------------------------------------------------------
%% @doc
%% Get machine config
%% @end
%%--------------------------------------------------------------------
get_machine_config(Name) ->
  gen_server:call(?MODULE, {get_config, Name}).

%%--------------------------------------------------------------------
%% @doc
%% Get new wire
%% @end
%%--------------------------------------------------------------------
new_wire() ->
	{ok, Id} = gen_server:call(proterlus_wires, new_wire),
	Id.

%%--------------------------------------------------------------------
%% @doc
%% Connect a machine to some physical medium (or wire)
%% @end
%%--------------------------------------------------------------------
connect_machine(Name, Port, Wire) ->
  gen_server:call(?MODULE, {connect, Name, Port, Wire}).

%%--------------------------------------------------------------------
%% @doc
%% Connect 2 machines
%% @end
%%--------------------------------------------------------------------
connect({Name1, Port1}, {Name2, Port2}) ->
	%% TODO: check if connected, if connection is possible
	Wire = ?MODULE:new_wire(),
  gen_server:call(?MODULE, {connect, Name1, Port1, Wire}),
  gen_server:call(?MODULE, {connect, Name2, Port2, Wire}).

%%--------------------------------------------------------------------
%% @doc
%% disconnect a machine from some physical medium (or wire)
%% @end
%%--------------------------------------------------------------------
disconnect_machine(Name, Port, Wire) ->
  gen_server:call(?MODULE, {disconnect, Name, Port, Wire}).

%%--------------------------------------------------------------------
%% @doc
%% show connections
%% @end
%%--------------------------------------------------------------------
connections() ->
  gen_server:call(?MODULE, connections).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #{machines => []}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({configure, MName, Config}, _From, #{machines := Ms} = State) ->
	%% TODO: check name clash and config
  proterlus_machine:start_link(MName),
	add_ports(MName, Config),
	add_os(MName, Config),
	MsNew = [MName|Ms],
	{reply, ok, State#{machines => MsNew}};
handle_call(machines, _From, #{machines := Ms} = State) ->
	{reply, Ms, State};
handle_call({get_config, MName}, _From, #{machines := Ms} = State) ->
	case lists:member(MName, Ms) of
		true ->
			MConf = gen_server:call(MName, get_config),
			{reply, MConf, State};
		_ ->
			{reply, {error, {not_exist, MName}}, State}
	end;
handle_call({connect, MName, Port, Wire}, _From, State) ->
	%% TODO: unplug if proterlus_wires fails
	Res = case gen_server:call(proterlus_wires, {plug, Wire, {MName, Port}}) of
					ok ->
						gen_server:call(MName, {plug, Port, Wire});
					Err ->
						Err
				end,
	{reply, Res, State};
handle_call({disconnect, MName, Port, Wire}, _From, State) ->
  gen_server:call(proterlus_wires, {unplug, Wire, {MName, Port}}),
  gen_server:call(MName, {unplug, Port}),
	{reply, ok, State};
handle_call(connections, _From, State) ->
	Res = gen_server:call(proterlus_wires, connections),
	{reply, Res, State};
handle_call(_Request, _From, State) ->
	{reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

add_ports(MName, Config) ->
	PortConfigs = proplists:get_value(ports, Config),
	[gen_server:call(MName, {add_port, Name, Type, Options}) || {Name, Type, Options} <- PortConfigs],
	ok.

add_os(MName, Config) ->
	{OS, Options} = proplists:get_value(os, Config),
	gen_server:call(MName, {add_os, OS, Options}),
	ok.
