%%%-------------------------------------------------------------------
%%% @author podri
%%% @copyright (C) 2019, podri
%%% @doc
%%%
%%% @end
%%% Created : 2019-12-03 20:47:46.079193
%%%-------------------------------------------------------------------
-module(proterlus_wires).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

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
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
    {ok, #{wires => [], plugs => []}}.

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
handle_call(new_wire, _From, #{wires := Wires, plugs := Plugs} = State) ->
	Id = length(Wires) + 1,
  WiresNew = [{Id, ok} | Wires],
	PlugsNew = [{Id, []} | Plugs],
  {reply, {ok, Id}, State#{wires => WiresNew, plugs => PlugsNew}};
handle_call({cut_wire, Id}, _From, #{wires := Wires} = State) when Id =< length(Wires) ->
  WiresNew = lists:keyreplace(Id, 1, Wires, {Id, fail}),
  {reply, ok, State#{wires => WiresNew}};
handle_call({cut_wire, Id}, _From, State) ->
  {reply, {error, {not_exist, Id}}, State};
handle_call({replace_wire, Id}, _From, #{wires := Wires} = State) when Id =< length(Wires) ->
  WiresNew = lists:keyreplace(Id, 1, Wires, {Id, ok}),
  {reply, ok, State#{wires => WiresNew}};
handle_call({replace_wire, Id}, _From, State) ->
  {reply, {error, {not_exist, Id}}, State};
handle_call({plug, Id, Port}, _From, #{wires := Wires, plugs := Plugs} = State) when Id =< length(Wires) ->
	{Id, IdPlugs} = lists:keyfind(Id, 1, Plugs),
	PlugsNew = lists:keyreplace(Id, 1, Plugs, {Id, [Port|IdPlugs]}),
  {reply, ok, State#{plugs => PlugsNew}};
handle_call({unplug, Id, Port}, _From, #{wires := Wires, plugs := Plugs} = State) when Id =< length(Wires) ->
	{Id, IdPlugs} = lists:keyfind(Id, 1, Plugs),
	IdPlugsNew = lists:delete(Port, IdPlugs),
	PlugsNew = lists:keyreplace(Id, 1, Plugs, {Id, IdPlugsNew}),
  {reply, ok, State#{plugs => PlugsNew}};
handle_call({plug, Id, _Port}, _From, State) ->
  {reply, {error, {not_exist, Id}}, State};
handle_call(connections, _From, #{plugs := Plugs} = State) ->
	{reply, Plugs, State};
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
handle_cast({spark, Port, Id, Package}, #{plugs := Plugs} = State) ->
  IdPlugs = lists:keyfind(Id, 1, Plugs),
	[gen_server:cast(MName, {pick_pkg, PName, Package}) || P = {MName, PName} <- IdPlugs, P =/= Port],
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




