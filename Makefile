PROJECT = erlexp
# --------------------------------------------------------------------
# Defining OTP version for this project which uses by kerl
# --------------------------------------------------------------------
ERLANG_OTP = OTP-21.0

# --------------------------------------------------------------------
# Compilation.
# --------------------------------------------------------------------

# global compile options (about more warnings see erl_lint.erl)
ERLC_GLOBAL_OPTS = +'{parse_transform, lager_transform}'

# default mode
ERLC_OPTS = $(ERLC_GLOBAL_OPTS)
ERLC_OPTS += +warn_export_all +warn_export_vars +warn_unused_import +warn_untyped_record +warn_missing_spec_all
ERLC_OPTS += -Werror
ERLC_OPTS += +debug_info

# tests mode
TEST_ERLC_OPTS += $(ERLC_GLOBAL_OPTS)
TEST_ERLC_OPTS += +debug_info

# --------------------------------------------------------------------
#  # Dependencies.
# --------------------------------------------------------------------

# 3rd party
dep_lager = git https://github.com/erlang-lager/lager				master
# 3rd party dev only
dep_sync = git https://github.com/rustyio/sync						master
dep_teaser = git https://github.com/spylik/teaser                   develop

DEPS = lager

SHELL_DEPS = sync teaser

# --------------------------------------------------------------------
# Development enviroment ("make shell" to run it).
# --------------------------------------------------------------------

SHELL_OPTS = -kernel shell_history enabled -args_file vm.args.development -config sys.config -pa ebin/ test/ -eval 'lager:start(), mlibs:discover()' -env ERL_LIBS deps -run mlibs autotest_on_compile

# --------------------------------------------------------------------
# Configuration for RELX
# --------------------------------------------------------------------

RELX_REPLACE_OS_VARS = true

# --------------------------------------------------------------------
# We using erlang.mk 
# --------------------------------------------------------------------

include erlang.mk
