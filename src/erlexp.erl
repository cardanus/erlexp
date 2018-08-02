-module(erlexp).

-define(NOTEST, true).
-ifdef(TEST).
    -compile(export_all).
    -define(ETSOPT, public).
-else.
    -define(ETSOPT, protected).
-endif.

-define(ETS_ALLOC_NAME, erlexp_alloc).
-define(ETS_EXPS_NAME, erlexp_exps).


-define(SERVER, ?MODULE).

-include("erlexp.hrl").
-include("deps/teaser/include/utils.hrl").

% gen server is here
-behaviour(gen_server).

% gen_server api
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% public api
-export([
        start/1, stop/1, stop/2,
        seed/2,
        variant/2
    ]).

-export_type([start_options/0]).

% @doc start api
-spec start(Options) -> Result when
    Options     :: start_options(),
    Result      :: {ok, Pid} | 'ignore' | {'error', Error},
    Pid         :: pid(),
    Error       :: {already_started, Pid} | term().

start(Options) ->
    gen_server:start_link(?MODULE, ?MODULE, Options, []).

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

-spec variant(UId, ExpId) -> Result when
    UId         :: uid(),
    ExpId       :: experiment_id(),
    Result      :: variant().

variant(UId, ExpId) ->
    variant(
      ets:lookup(?ETSNAME, alloc_key(UId, ExpId)),
      UId,
      ExpId
     ).

variant([], UId, ExpId) ->
    gen_server:call(?SERVER, {'allocate', UId, ExpId}).

% ============================ gen_server part =================================

init(Options) ->
    State = #erlexp_state{
            seed_freq = maps:get('seed_freq', Options, 1000),
            seed_qty = maps:get('seed_qty', Options, 1000),
            seed_threshold_upper = maps:get('seed_threshold_upper', Options, 100000),
            seed_threshold_lower = maps:get('seed_threshold_lower', Options, 500),
            self_pid = self(),
            alloc_ets = ets:new(?ETS_ALLOC_NAME, [set, ?ETSOPT, {keypos, #allocations.alloc_key}, named_table]),
            exps_ets = ets:new(?ETS_EXPS_NAME, [set, ?ETSOPT, {keypos, #experiments.id}, named_table])
        }
    {ok, State#erlexp_state{
            seed_tref = timer:apply_interval(SeedFreq, ?MODULE, seed, [State]),
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

handle_call({'allocate', UId, ExpId}, _From, #erlexp_state{ets = Ets, variants = [Variant | T]} = State) ->
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
handle_cast({seed, ExpId, Variants},#erlexp_state{exps_ets = Ets} = State) ->
    NewVariants = lists:foldl(fun
        (Variant, #experiments{variants = Acc}) -> [Variant | Acc];
        (Variant, Acc) -> [Variant | Acc]
    end, ets:lookup(Ets, ExpId), Variants),
    {noreply, State};

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


seed(#erlexp_state{seed_threshold_upper = ThrshUp, exps_ets = Ets} = State) ->
    MS = [{#experiments{current_v_qty = '$1', _ = '_'},
           {'andalso',
                {'<', '$1', ThrshUp}
           },
        ['$_']
    }],
    seed(ets:select(Ets, MS)

-spec seed(Experiments, State) -> Result when
    Experiments :: [] | [experiment()],
    SendTo      :: erlexp_state(),
    Result      :: term().


seed(Experiments, #erlexp_state{exps_ets = Ets, seed_qty = Qty, self_pid = SelfPid} = State) ->
    lists:map(fun
        (#experiments{id = ExpId, b_probability = Probability]}) ->
            SelfPid ! {
              seed,
              ExpId,
                [case rand:uniform() of Number =< Probability -> b; _ -> a end || _ <- lists:seq(1, Qty)]
             }
        end,
    Experiments).


-spec alloc_key(UId, ExpId) -> Result when
    UId         :: uid(),
    ExpId       :: experiment_id(),
    Result      :: alloc_key().

alloc_key(UId, ExpId) -> {UId, ExpId}.

% ------------------------------- end of internals -----------------------------


