% stick_id is with what experiment variant will be linked (sticked).
% In common is kind of user_id or session_id or event_id
-type stick_id()        :: binary().

-type variant()         :: 0 | 1.

-type start_options()   :: #{
    'transport_module'              => module(),
    'seed_freq'                     => pos_integer(),
    'seed_qty'                      => pos_integer(),
    'seed_threshold_upper'          => pos_integer(),
    'seed_threshold_lower'          => pos_integer(),
%    'seed_threshold_auto_adjusting' => boolean(), % todo
    'registered'                    => atom()
}.

-record(erlexp_state, {
        'seed_freq' = 1000                      :: pos_integer(),
        'seed_qty' = 1000                       :: pos_integer(),
        'seed_threshold_upper' = 100            :: pos_integer(),
        'seed_threshold_lower' = 100000         :: pos_integer(),
%        'seed_threshold_auto_adjusting' = false :: boolean(), % todo
        'variants'                              :: [variant()],
        'ets'                                   :: ets:tid(),
        'transport_module'                      :: module()
    }).

-record(allocations, {
        stickid_and_exp             :: {stick_id(), experiment_id()},
        variant                     :: variant()
    }).
-type erlexp_state()                :: #erlexp_state{}.

-type accepted_messages()           :: term().
