% uid is with what experiment variant will be linked (sticked).
% In common is kind of user_id or session_id or event_id
-type uid()             :: binary().
-type experiment_id()   :: binary().
-type alloc_key()       :: {uid(), experiment_id()}.
-type server()          :: atom() | pid().

-type seed_msg()        :: {seed, experiment_id(), {rand:state(), [variant()]}}.

-type variant()         :: a | b.
-type alloc_rate()      :: float() | 0 | 1.

-type match_spec()      :: '$1' | '$2' | '_'.

-type start_options()   :: #{
    'transport_module'              => module(),
    'seed_freq'                     => pos_integer(),
    'seed_qty'                      => pos_integer(),
    'seed_threshold_upper'          => pos_integer(),
    'seed_threshold_lower'          => pos_integer()
}.

-record(erlexp_state, {
        'seed_freq' = 1000                      :: pos_integer(),
        'seed_qty' = 1000                       :: pos_integer(),
        'seed_threshold_upper' = 100000         :: pos_integer(),
        'seed_threshold_lower' = 500            :: pos_integer(),
        'seed_tref'                             :: undefined | timer:tref(),
        'self_pid'                              :: pid(),
        'alloc_ets'                             :: ets:tid(),
        'exps_ets'                              :: ets:tid(),
        'transport_module'                      :: module()
    }).

-type erlexp_state()                :: #erlexp_state{}.

% record for alloc_ets ets table
-record(allocations, {
        alloc_key                   :: alloc_key(),
        variant                     :: variant()
    }).
-type allocations()                 :: #allocations{}.

% record for keeping experiments
-record(experiments, {
        id                          :: experiment_id()          | match_spec(),
        active                      :: boolean()                | match_spec(),
        b_probability               :: alloc_rate()             | match_spec(),
        last_seed_state = undefined :: undefined | rand:state() | match_spec(),
        current_v_qty               :: non_neg_integer()        | match_spec(),
        variants                    :: [variant()]              | match_spec()
    }).
-type experiments()                 :: #experiments{}.

-type accepted_messages()           :: term().
