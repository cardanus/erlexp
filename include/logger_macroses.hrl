% debug macros (development only, will not available once compiled in release mode)
% todo: rid macros from the code with parse transform for release mode
-ifndef(release).
    -define(here, error_logger:info_msg("(~p)~p: we are here", [?LINE,?MODULE]), true).
    -define(dump_to_file(Data, Filename),
        ok = file:write_file(Filename, io_lib:fwrite("~s~n",[Data]))
    ).
    -define(debug(Msg),
        error_logger:info_msg("(~p)~p: ~p is ~p", [?LINE,?MODULE,??Msg,Msg]), true
    ).
    -define(debug(Msg,Arg),
        error_logger:info_msg(lists:concat(["(~p)~p: ", Msg]), lists:append([?LINE,?MODULE], Arg)), true
    ).
-else.
    -define(here, true).
    -define(dump_to_file(Data, Filename), true).
    -define(debug(Msg), true).
    -define(debug(Msg,Arg), true).
-endif.

% warning macros (will be available both in development and release mode)
-define(warning(Msg),
        error_logger:warning_msg("(~p)~p: ~s", [?LINE,?MODULE,Msg]), true
    ).
-define(warning(Msg,Arg),
        error_logger:warning_msg(lists:concat(["(~p)~p: ", Msg]), lists:append([?LINE,?MODULE], Arg)), true
    ).

% info macros (will be available both in development and release mode)
-define(info(Msg),
        error_logger:info_msg("(~p)~p: ~s", [?LINE,?MODULE,Msg]), true
    ).
-define(info(Msg,Arg),
        error_logger:info_msg(lists:concat(["(~p)~p: ", Msg]), lists:append([?LINE,?MODULE], Arg)), true
    ).

% error macros (will be available both in development and release mode)
-define(error(Msg),
        error_logger:error_msg("(~p)~p: ~s", [?LINE,?MODULE,Msg]), true
    ).
-define(error(Msg,Arg),
        error_logger:error_msg(lists:concat(["(~p)~p: ", Msg]), lists:append([?LINE,?MODULE], Arg)), true
    ).

% got undefined message with standart format
-define(undefined(Arg),
        {current_function, {M, F, A}} = process_info(self(), current_function),
        RegisteredName = case process_info(self(), registered_name) of
            [] -> unregistered;
            {registered_name, Name} -> Name
        end,
        error_logger:warning_msg("(~p)~p:~p/~p (~p, ~p) something undefined:~n~p", [?LINE,M,F,A,self(),RegisteredName,Arg]),
        true
    ).
% got undefined message with custom format
-define(undefined(Msg,Arg),
        {current_function, {M, F, A}} = process_info(self(), current_function),
        RegisteredName = case process_info(self(), registered_name) of
            [] -> unregistered;
            {registered_name, Name} -> Name
        end,
        error_logger:warning_msg(lists:concat(["(~p)~p:~p/~p (~p, ~p)", Msg]), [?LINE,M,F,A,self(),RegisteredName,Arg]),
        true
    ).
