-module(erlexp).

-define(NOTEST, true).
-ifdef(TEST).
    -compile(export_all).
-endif.

-include("erlexp.hrl").
%-include("deps/teaser/include/utils.hrl").

% gen server is here
-behaviour(gen_server).

% gen_server api
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% public api
-export([
        start/1, stop/1, stop/2
    ]).

-export_type([start_options/0]).

% @doc start api
-spec start(Options) -> Result when
    Options     :: start_options(),
    Result      :: {ok, Pid} | 'ignore' | {'error', Error},
    Pid         :: pid(),
    Error       :: {already_started, Pid} | term().

start(#{'register' := Register} = Options) ->
    gen_server:start_link(Register, ?MODULE, Options, []);

% non-registered gen_server here mostly used for tests
start(Options) ->
    gen_server:start_link(?MODULE, Options, []).

% @doc API for stop gen_server. Default is sync call.
-spec stop(Server) -> Result when
    Server      :: server(),
    Result      :: term().

stop(Server) ->
    stop('sync', Server).

% @doc API for stop gen_server. We support async casts and sync calls aswell.
-spec stop(SyncAsync, Server) -> Result when
    SyncAsync   :: 'sync' | 'async',
    Server      :: server(),
    Result      :: term().

stop('sync', Server) ->
    gen_server:stop(Server);
stop('async', Server) ->
    gen_server:cast(Server, stop).

% ============================ gen_server part =================================

init(Options) ->
    AllocationHeartbeatFreq = maps:get('seed_freq', Options, 1000),
    AllocationQty = maps:get('seed_qty', Options, 1000),

    {ok, #erlexp_state{}}.

%============ handle_call =================

% @doc callbacks for gen_server handle_call.
-spec handle_call(Message, From, State) -> Result when
    Message     :: accepted_messages(),
    From        :: {pid(), Tag},
    Tag         :: term(),
    State       :: erlexp_state(),
    Result      :: {reply, term(), State}.

% handle_call for all other thigs
handle_call(Msg, _From, State) ->
    ?undefined(Msg),
    {reply, ok, State}.

%-----------end of handle_call-------------

%============ handle_cast =================

% @doc callbacks for gen_server handle_cast.
-spec handle_cast(Message, State) -> Result when
    Message :: accepted_messages(),
    State   :: erlexp_state(),
    Result  :: {noreply, State} | {stop, normal, State}.

% handle_cast for stop
handle_cast(stop, State) ->
    {stop, normal, State};

%% handle_info for all other thigs
handle_cast(Msg, State) ->
    ?undefined(Msg),
    {noreply, State}.

%-----------end of handle_cast-------------

%============ handle_info =================

% @doc callbacks for gen_server handle_info.
-spec handle_info(Message, State) -> Result when
    Message :: term(),
    State   :: erlexp_state(),
    Result  :: {noreply, State}.

% handle_info for all other thigs
handle_info(Msg, State) ->
    ?undefined(Msg),
    {noreply, State}.

%-----------end of handle_info-------------


% @doc call back for gen_server terminate
-spec terminate(Reason, State) -> term() when
    Reason      :: 'normal' | 'shutdown' | {'shutdown',term()} | term(),
    State       :: erlexp_state().

terminate(Reason, State) ->
    {noreply, Reason, State}.

% @doc call back for gen_server code_change
-spec code_change(OldVsn, State, Extra) -> Result when
    OldVsn      :: Vsn | {down, Vsn},
    Vsn         :: term(),
    State       :: erlexp_state(),
    Extra       :: term(),
    Result      :: {ok, NewState},
    NewState    :: erlexp_state().

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% --------------------------- end of gen_server part ---------------------------

% ================================= internals ==================================

-spec seed(Qty) -> Result when
    Qty       :: pos_integer(),
    Result    :: [variant()].

seed(Qty) ->
    [crypto:rand_uniform(0, 2) || _ <- lists:seq(1, Qty)].

% ------------------------------- end of internals -----------------------------

