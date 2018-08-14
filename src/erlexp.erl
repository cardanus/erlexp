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

-define(DEF_ALGO, exsp).

-define(SERVER, ?MODULE).
-define(SEED_SERVER, erlexp_seeder).

-include("erlexp.hrl").
-include("logger_macroses.hrl").

% gen server is here
-behaviour(gen_server).

% gen_server api
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% public api
-export([
        start/1, stop/1, stop/2,
        seed/1,
        variant/2,
        set/2
    ]).

-export_type([start_options/0]).

% @doc start api
-spec start(Options) -> Result when
    Options     :: start_options(),
    Result      :: {ok, Pid} | 'ignore' | {'error', Error},
    Pid         :: pid(),
    Error       :: {already_started, Pid} | term().

start(Options) ->
    gen_server:start_link({'local', ?SERVER}, ?MODULE, Options, []).

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
    variant(UId, ExpId, undefined).

-spec set(ExpId, ExpSettings) -> Result when
    ExpId       :: experiment_id(),
    ExpSettings :: exp_settings(),
    Result      :: ok.

set(ExpId, ExpSetting) ->
    gen_server:call(?SERVER, {'set', ExpId, ExpSetting}).

% ============================ gen_server part =================================

-spec init(Options) -> Result when
    Options :: start_options(),
    Result  :: {'ok', erlexp_state()}.

init(Options) ->
    SeedFreq = maps:get('seed_freq', Options, 1000),
    State = #erlexp_state{
            auto_discover = maps:get(
                'auto_discover', Options,
                application:get_env(?MODULE, auto_discover, true)
            ),
            seed_freq = SeedFreq,
            seed_qty = maps:get(
                'seed_qty', Options,
                application:get_env(?MODULE, seed_qty, 1000)
            ),
            seed_threshold_upper = maps:get(
                'seed_threshold_upper', Options,
                application:get_env(?MODULE, seed_threshold_upper, 10000)
            ),
            seed_threshold_lower = maps:get(
                'seed_threshold_lower', Options,
                application:get_env(?MODULE, 'seed_threshold_lower', 500)
            ),
            transport_module = maps:get(
                'transport_module', Options,
                application:get_env(?MODULE, 'transport_module', ?MODULE)
            ),
            self_pid = self(),
            alloc_ets = ets:new(?ETS_ALLOC_NAME, [set, ?ETSOPT, {keypos, #allocations.alloc_key}, named_table]),
            exps_ets = ets:new(?ETS_EXPS_NAME, [set, ?ETSOPT, {keypos, #experiments.id}, named_table])
        },
    {ok, Tref} = timer:apply_interval(SeedFreq, ?MODULE, seed, [State]),
    {ok, State#erlexp_state{
            seed_tref = Tref
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

handle_call({'set', ExpId, ExpSetting}, _From, State) ->
    _ = set(ExpId, ExpSetting, State),
    {reply, ok, State};

handle_call({'allocate', UId, ExpId}, _From, State) ->
    {reply, variant(UId, ExpId, State), State};

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

% handle_cast for seed.
% we ignoring varints if seed_state already changed
handle_cast({seed, ExpId, {NewSeedState, Variants}, OldSeedState},#erlexp_state{exps_ets = Ets} = State) ->
    _ = case ets:lookup(Ets, ExpId) of
        [#experiments{last_seed_state = OldSeedState} = Experiment] ->
            NewVariants = lists:foldl(fun(Variant, Acc) -> [Variant | Acc] end, Experiment#experiments.variants, Variants),
            ets:insert(Ets,
                Experiment#experiments{
                  variants = NewVariants,
                  last_seed_state = NewSeedState,
                  current_v_qty = length(NewVariants)
            });
        _ ->
            false
    end,
    {noreply, State};

% when we support auto_discovering we set the experiment
handle_cast({auto_discover, ExpId}, #erlexp_state{auto_discover = true} = State) ->
    _ = set(ExpId, #{}, State),
    {noreply, State};

% when we don't support auto_discovering for tests, we just ignore message
handle_cast({auto_discover, _ExpId}, #erlexp_state{auto_discover = false} = State) ->
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

terminate(Reason, #erlexp_state{seed_tref = SeedTref}= State) ->
    _ = timer:cancel(SeedTref),
    {noreply, Reason, State#erlexp_state{seed_tref = undefined}}.

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

% @doc Seed experiments variants.
% We traversing experiments ETS-table and finding experiments where amount of variants less than seed_threshold_upper,
% then passing list this experiments to the seed/2.
%
% We skipping variants seeding here for 0% and 100% probability
% @end

% it is possible that we try to seed new data when before previus seeding completed.
% so, we avoiding collision here just by registering process.
-spec seed(State) -> Result when
    State       :: erlexp_state(),
    Result      :: [] | [ok].

seed(#erlexp_state{seed_threshold_upper = ThrshUp, exps_ets = Ets} = State) ->
    try
        true = register(?SEED_SERVER, self()),  % fail here if we already running
        MS = [{#experiments{current_v_qty = '$1', 'b_probability' = '$2', status = active, _ = '_'},
               [{'andalso',
                    {'<', '$1', ThrshUp},
                    {'orelse',
                        {'=/=', '$2', '0'},
                        {'=/=', '$2', '1'}
                    }
               }],
            ['$_']
        }],
        seed(ets:select(Ets, MS), State)
    catch
        Error ->
            ?error(Error)
    end.


-spec seed(Experiments, State) -> Result when
    Experiments :: [] | [experiments()],
    State       :: erlexp_state(),
    Result      :: [] | [ok].

seed(Experiments, #erlexp_state{seed_qty = Qty, self_pid = SelfPid}) ->
    lists:map(fun
        (#experiments{id = ExpId, b_probability = Probability, last_seed_state = SeedState}) ->
            gen_server:cast(SelfPid, {seed, ExpId, seed_with_state(SeedState, Probability, Qty, []), SeedState})
        end,
    Experiments).

% @doc Generate Qty amount of variants between a and b.
% To allow us using different probability rate for different experiments we using here uniform distribution
% for random numbers.
% To perform uniform distribution with acceptance level of granularity we have to deal with the seed state.
% @end

-spec seed_with_state(SeedStateOrNumber, Probability, Qty, Acc) -> Result when
    SeedStateOrNumber   :: undefined | rand:state() | {float(), rand:state()},
    Probability         :: alloc_rate(),
    Qty                 :: non_neg_integer(),
    Acc                 :: [] | [a | b],
    Result              :: {rand:state(), [variant()] | []}.

seed_with_state(SeedState, Probability, Qty, _Acc) when Probability == 1 ->
    {SeedState, [b || _ <- lists:seq(1, Qty)]}; % don't use rand for 100% probability
seed_with_state(SeedState, Probability, Qty, _Acc) when Probability == 0 ->
    {SeedState, [a || _ <- lists:seq(1, Qty)]}; % don't use rand for 0% probability

seed_with_state({Number, {#{}, _} = NewSeedState}, Probability, Qty, Acc) when Number =< Probability ->
    seed_with_state(NewSeedState, Probability, Qty-1, [b | Acc]);
seed_with_state({_Number, {#{}, _} = NewSeedState}, Probability, Qty, Acc) ->
    seed_with_state(NewSeedState, Probability, Qty-1, [a | Acc]);

seed_with_state(undefined, Probability, Qty, Acc) ->
    seed_with_state(rand:seed(?DEF_ALGO), Probability, Qty, Acc);

seed_with_state(NewSeedState, _Probability, 0, Acc) -> {NewSeedState, Acc};

seed_with_state(State, Probability, Qty, Acc) ->
    seed_with_state(rand:uniform_real_s(State), Probability, Qty, Acc).

% @doc set settings for experiment
-spec set(ExpId, ExpSettings, State) -> Result when
    ExpId       :: experiment_id(),
    ExpSettings :: exp_settings(),
    State       :: erlexp_state(),
    Result      :: true.

set(ExpId, ExpSettings, #erlexp_state{exps_ets = Ets} = State) ->
    Experiment = ets:lookup(Ets, ExpId),
    ets:insert(Ets,
        should_update_variants(
            Experiment,
            gen_or_update_exp(ExpId, ExpSettings, Experiment),
            State
        )
    ).

% @doc generate new or update experiment
-spec gen_or_update_exp(ExpId, ExpSettings, ExpFromEts) -> Result when
    ExpId       :: experiment_id(),
    ExpSettings :: exp_settings(),
    ExpFromEts  :: [] | [experiments()],
    Result      :: experiments().

gen_or_update_exp(ExpId, ExpSettings, []) ->
    #experiments{
       id = ExpId,
       status = maps:get(status, ExpSettings, active),
       b_probability = maps:get(b_probability, ExpSettings, 0.5)
    };

gen_or_update_exp(_ExpId, ExpSettings, [Exp]) ->
    Exp#experiments{
       status = maps:get(status, ExpSettings, Exp#experiments.status),
       b_probability = maps:get(b_probability, ExpSettings, Exp#experiments.b_probability)
    }.

% @doc we have to reset variants if we are going to change probability rate
-spec should_update_variants(OldExperiment, NewExperiment, State) -> Result when
    OldExperiment   :: [] | [experiments()],
    NewExperiment   :: experiments(),
    State           :: erlexp_state(),
    Result          :: experiments().

should_update_variants([], Experiment, _State) -> Experiment;
should_update_variants(
    [#experiments{b_probability = SameBP}],
    #experiments{b_probability = SameBP} = Experiment,
    _State
) ->
    Experiment;

should_update_variants(
    _OldExperiment,
    #experiments{b_probability = Probability, last_seed_state = SeedState} = Experiment,
    #erlexp_state{seed_qty = Qty}
) ->
    {NewSeedState, Variants} = seed_with_state(SeedState, Probability, Qty, []),
    Experiment#experiments{last_seed_state = NewSeedState, variants = Variants}.

%% @doc function for adapt number to probability rate.
%% The main idea that we can define probability in percent (eg 10%). It should
%% automatically convert to the 0 < N < 1 format.
%% In case if number passed correctly, we returning "as-is".
%% We also filtering here numbers which not in our range.
%% @end
%
%-spec verify_probability(Number) -> Result when
%      Number    :: integer() | float(),
%      Result    :: float() | {error, term}.
%
%verify_probability(Number) when Number < 0 ->
%    {error,
%        {
%            probability_less_than_0,
%            io_lib:format("Given number ~p is less than 0.",[Number])
%        }
%    };
%verify_probability(Number) when Number > 100 ->
%    {error,
%        {
%            probability_more_than_100,
%            io_lib:format("Given number ~p is less than 0.",[Number])
%        }
%    };
%verify_probability(Number) when Number =< 1 -> Number;
%
%verify_probability(Number) ->
%   verify_probability(Number / 100).

% @doc generate allocation key
-spec alloc_key(UId, ExpId) -> Result when
    UId         :: uid(),
    ExpId       :: experiment_id(),
    Result      :: alloc_key().

alloc_key(UId, ExpId) -> {UId, ExpId}.


% @doc get variant
-spec variant(UId, ExpId, State) -> Result when
    UId         :: uid(),
    ExpId       :: experiment_id(),
    State       :: undefined | erlexp_state(),
    Result      :: variant().

variant(UId, ExpId, State) ->
    variant(
      ets:lookup(?ETS_ALLOC_NAME, alloc_key(UId, ExpId)),
      ets:lookup(?ETS_EXPS_NAME, ExpId),
      UId,
      ExpId,
      State
     ).

-spec variant(Allocations, Experiment, UId, ExpId, State) -> Result when
    Allocations :: [allocations()],
    Experiment  :: [experiments()],
    UId         :: uid(),
    ExpId       :: experiment_id(),
    State       :: undefined | erlexp_state(),
    Result      :: variant().

% when experiment is not yet active (disabled) we always returning "a"
variant(_Allocation, [#experiments{status = disabled}], _UId, _ExpId, _State) -> a;

% when experiment "canceled" we always force "a" even if it was allocated before as "b"
variant(_Allocation, [#experiments{status = canceled}], _UId, _ExpId, _State) -> a;

% The difference here between "disabled" and "canceled" is that when we perorm statistical calculation we have to
% exclude trafic with this experiment for experiments with the same goal because it may affect each other.

% when experiment integrated we always force "b" even if it was allocated before as "a"
variant(_Allocation, [#experiments{status = integrated}], _UId, _ExpId, _State) -> b;

% if allocation happend before and experiment not canceled, we returning allocated variant
variant([#allocations{variant = Variant}], _Experiment, _UId, _ExpId, _State) -> Variant;

% if allocation not yet happend and experiment not found, we returning "a" but try to perform auto_discovering
variant([], [], _UId, ExpId, undefined) ->
    gen_server:cast(?SERVER, {auto_discover, ExpId}),
    a;

% if experiment found but not yet allocated to user, we perform allocation
variant([], [#experiments{}], UId, ExpId, undefined) ->
    gen_server:call(?SERVER, {'allocate', UId, ExpId});

% if experiment found with b_probability 0% we stick to "a"
variant([], [#experiments{status = active, b_probability = 0}], UId, ExpId, State) ->
    stick(UId, ExpId, a, State);

% if experiment found with b_probability 100% we stick to "b"
variant([], [#experiments{status = active, b_probability = 1}], UId, ExpId, State) ->
    stick(UId, ExpId, b, State);

% if experiment found but variants still empty we returning "a"
variant([], [#experiments{variants = [], b_probability = Probability}], _UId, ExpId, _State) ->
    ?warning("Don't have variants for experiment ~p with probability ~p",[ExpId, Probability]),
    a;

variant([], [#experiments{variants = [Variant|T], current_v_qty = Qty} = Experiment], UId, ExpId, State) ->
    ets:insert(State#erlexp_state.exps_ets, Experiment#experiments{variants = T, current_v_qty = Qty-1}),
    stick(UId, ExpId, Variant, State).

% @doc stick experiment to the customer
-spec stick(UId, ExpId, Variant, State) -> Result when
    UId         :: uid(),
    ExpId       :: experiment_id(),
    Variant     :: variant(),
    State       :: erlexp_state(),
    Result      :: variant().

stick(UId, ExpId, Variant, #erlexp_state{alloc_ets = Ets, transport_module = TransportModule}) ->
    ToStick = #allocations{alloc_key = alloc_key(UId, ExpId), variant = Variant},
    ets:insert_new(Ets, ToStick),
    TransportModule,
%    erlang:spawn(TransportModule, ?FUNCTION_NAME, [ToStick]),
    Variant.

% ------------------------------- end of internals -----------------------------


