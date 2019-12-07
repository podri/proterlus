%%%-------------------------------------------------------------------
%% @doc proterlus top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(proterlus_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 5,
                 period => 1},
		ProterlusSpec = #{id => proterlus,
											start => {proterlus, start_link, []},
											restart => permanent,
											shutdown => 5000,
											type => worker,
											modules => [proterlus]},
		ProterlusWiresSpec = #{id => proterlus_wires,
													 start => {proterlus_wires, start_link, []},
													 restart => permanent,
													 shutdown => 5000,
													 type => worker,
													 modules => [proterlus_wires]},
    ChildSpecs = [ProterlusSpec, ProterlusWiresSpec],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
