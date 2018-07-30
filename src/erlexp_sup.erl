-module(erlexp_sup).

% supervisor is here
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

% @doc start root supervisor
-spec start_link() -> Result when
    Result :: 'ignore' | {'error',_} | {'ok',pid()}.

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

% @doc init callbacks
-spec init([]) -> Result when
    Result :: {ok, {SupFlags :: supervisor:sup_flags(), [ChildSpec :: supervisor:child_spec()]}}.

init([]) ->
    RestartStrategy = {one_for_one, 4, 3600},

    ErlExp = {
        erlexp,
        {erlexp, start_link, []},
        permanent,
        5000,
        worker,
        [erlexp]
    },

    Childrens = [ErlExp],

    {ok, {RestartStrategy, Childrens}}.
