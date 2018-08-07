-module(erlexp_app).

% Application is here
-behaviour(application).

%% API
-export([start/2]).
-export([stop/1]).

% @doc start application
-spec start(Type, Args) -> Result when
    Type        :: application:start_type(),
    Args        :: term(),
    Result      :: {'ok', pid()} | {'ok', pid(), State :: term()} | {'error', Reason :: term()}.

start(normal, StartArgs) ->
    erlexp_sup:start_link().

% @doc stop application
-spec stop(State) -> Result when
    State       :: term(),
    Result      :: ok.

stop(_State) -> ok.
