-module(erlexp).

-define(NOTEST, true).
-ifdef(TEST).
    -compile(export_all).
-endif.

-define(ETSNAME, ?MODULE).

-include("erlexp.hrl").
%-include("deps/teaser/include/utils.hrl").

% gen server is here
-behaviour(gen_server).

% gen_server api
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% public api
-export([
        start/1, stop/1, stop/2,
        seed/2,
        is_active/2
    ]).

-export_type([start_options/0]).

% @doc start api
-spec start(Options) -> Result when
    Options     :: start_options(),
    Result      :: {ok, Pid} | 'ignore' | {'error', Error},
    Pid         :: pid(),
    Error       :: {already_started, Pid} | term().

start(Options) ->
    gen_server:start_link(?MODULE, ?MODULE, Options, []);

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

-spec is_active(StickId, ExpId) -> Result when
    StickId     :: stick_id(),
    ExpId       :: experiment_id(),
    Result      :: boolean().

is_active(StickId, ExpId) ->
    is_active(
      ets:lookup(?ETSNAME, alloc_key(StickId, ExpId)),
      StickId,
      ExpId
     ).

is_active([], StickId, ExpId) ->
    gen_server:call(?SERVER, {'allocate', StickId, ExpId}).

% ============================ gen_server part =================================

init(Options) ->
    SeedFreq = maps:get('seed_freq', Options, 1000),
    SeedQty = maps:get('seed_qty', Options, 1000),
    SeedThrUpper = maps:get('seed_threshold_upper', Options, 100000),
    SeedThrLower = maps:get('seed_threshold_lower', Options, 100),
    SelfPid = self(),

    {ok, #erlexp_state{
            seed_freq = SeedFreq,
            seed_qty = SeedQty,
            seed_threshold_upper = SeedThrUpper,
            seed_threshold_lower = SeedThrLower,
            seed_tref = timer:apply_interval(SeedFreq, ?MODULE, seed, [SeedQty, SelfPid]),
            self_pid = SelfPid,
            ets = ets:new(?ETSNAME, [set, ?ETSOPT, {keypos, #allocations.alloc_key}, named_table])
        }
    }.

%============ handle_call =================

% @doc callbacks for gen_server handle_call.
-spec handle_call(Message, From, State) -> Result when
    Message     :: accepted_messages(),
    From        :: {pid(), Tag},
    Tag         :: term(),
    State       :: erlexp_state(),
    Result      :: {reply, term(), State}.

handle_call({'allocate', StickId, ExpId}, _From, State#erlexp_state{ets = Ets, variants = [Variant | T]}) ->
    {reply, Variant, State#erlexp_state{variants = T}};


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

% handle_cast for seed
handle_cast({seed, Variants},
    #erlexp_state{
        variants = OldVariants,
        seed_threshold_upper = Threshold
} = State) when length(OldVariants) < Threshold ->
    {noreply, State#erlexp_state{
        variants = lists:foldl(fun(Variant, Acc) -> [Variant | Acc] end, OldVariants, Variants)
    }};

handle_cast({seed, _Variants},
    #erlexp_state{
       variants = OldVariants,
       seed_threshold_upper = Threshold
} = State) ->
    ?info("Got seed message when upper threshold is ~p, but we still have ~p variants",[Threshold, OldVariants]);

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

-spec seed(Qty, SendTo) -> Result when
    Qty         :: pos_integer(),
    SendTo      :: pid(),
    Result      :: [variant()].

seed(Qty, SendTo) ->
    gen_server:cast(SendTo, {seed, [crypto:rand_uniform(0, 2) || _ <- lists:seq(1, Qty)]}).

-spec alloc_key(StickId, ExpId) -> alloc_key().

alloc_key(StickId, ExpId) -> {StickId, ExpId}.

% ------------------------------- end of internals -----------------------------


