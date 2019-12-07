%%%-------------------------------------------------------------------
%%% @author podri
%%% @copyright (C) 2019, podri
%%% @doc
%%%
%%% @end
%%% Created : 2019-12-01 19:25:27.936904
%%%-------------------------------------------------------------------
-module(proterlus_machine).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

%-record(state, {name, phys_link, nic, eth_mod}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server and registers it with Name
%%
%% @spec start_link(Name) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Name) ->
	gen_server:start_link({local, Name}, ?MODULE, [Name], []).

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
init([Name]) ->
	{ok, #{name => Name,
				 ports => [],
				 plugged => [],
				 os => {default_os, []}}}.

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
handle_call({add_port, PortName, Type, Options}, _From, #{ports := Ports} = State)
	when Type == nic; Type == serial ->
	PortsNew = [{PortName, {Type, Options}} | Ports],
	{reply, ok, State#{ports => PortsNew}};
handle_call({plug, PortName, Wire}, _From, #{plugged := Plugs, ports := Ports} = State) ->
	case proplists:lookup(PortName, Ports) of
    none ->
			{reply, {error, {not_exists, PortName}}, State};
		{PortName, _} ->
			PlugsNew = [{PortName, Wire} | Plugs],
			{reply, ok, State#{plugged => PlugsNew}}
	end;
handle_call({unplug, PortName}, _From, #{plugged := Plugs} = State) ->
	PlugsNew = proplists:delete(PortName, Plugs),
	{reply, ok, State#{plugged => PlugsNew}};
handle_call(get_config, _From, State) ->
	{reply, State, State};
handle_call({add_os, Name, Options}, _From, State) ->
	{reply, ok, State#{os => {Name, Options}}};
handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

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
handle_cast({put_pkg, PortName, Package}, #{name := MName, plugged := PluggedPorts} = State) ->
	case proplists:lookup(PortName, PluggedPorts) of
		{PortName, Wire} ->
      gen_server:cast(proterlus_wires, {spark, {MName, PortName}, Wire, Package});
		_ ->
			ok
	end,
	{noreply, State};
handle_cast({pick_pkg, PortName, Package}, #{ports := Ports, os := OS} = State) ->
	case proplists:lookup(PortName, Ports) of
		{PortName, {Type, Options}} ->
			gen_event:notify(OS, {pkg_on_port, PortName, Type, Options, Package});
    _ ->
			skip
	end,
	{noreply, State};
	
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




