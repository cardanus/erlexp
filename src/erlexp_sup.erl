-module(erlexp_sup).

% supervisor is here
-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

% @doc start root supervisor
-spec start_link(Options) -> Result when
    Options :: erlexp:start_options(),
    Result  :: 'ignore' | {'error',_} | {'ok',pid()}.

start_link(Options) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Options).

% @doc init callbacks
-spec init(Options) -> Result when
    Options :: erlexp:start_options(),
    Result  :: {ok, {SupFlags :: supervisor:sup_flags(), [ChildSpec :: supervisor:child_spec()]}}.

init(Options) ->
    RestartStrategy = {one_for_one, 4, 3600},

    ErlExp = {
        erlexp,
        {erlexp, start, Options},
        permanent,
        5000,
        worker,
        [erlexp]
    },

    Childrens = [ErlExp],

    {ok, {RestartStrategy, Childrens}}.
