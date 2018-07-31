% stick_id is with what experiment variant will be linked (sticked).
% In common is kind of user_id or session_id or event_id
-type stick_id()        :: binary().
-type experiment_id()   :: binary().
-type allo_key()        :: {stick_id(), experiment_id()}.

-type variant()         :: 0 | 1.

-type start_options()   :: #{
    'transport_module'              => module(),
    'seed_freq'                     => pos_integer(),
    'seed_qty'                      => pos_integer(),
    'seed_threshold_upper'          => pos_integer(),
    'seed_threshold_lower'          => pos_integer(),
%    'seed_freq_auto_adjusting' => boolean(), % todo
    'registered'                    => atom()
}.

-record(erlexp_state, {
        'seed_freq' = 1000                      :: pos_integer(),
        'seed_qty' = 1000                       :: pos_integer(),
        'seed_threshold_upper' = 100000         :: pos_integer(),
        'seed_threshold_lower' = 100            :: pos_integer(),
        'seed_tref'                             :: timer:tref(),
        'seld_pid'                              :: pid(),
%        'seed_freq_auto_adjusting' = false :: boolean(), % todo
        'variants'                              :: [variant()],
        'ets'                                   :: ets:tid(),
        'transport_module'                      :: module()
    }).

-record(allocations, {
        stickid_and_exp             :: allo_key(),
        variant                     :: variant()
    }).
-type erlexp_state()                :: #erlexp_state{}.

-type accepted_messages()           :: term().
