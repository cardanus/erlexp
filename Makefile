PROJECT = erlexp
# --------------------------------------------------------------------
# Defining OTP version for this project which uses by kerl
# --------------------------------------------------------------------
ERLANG_OTP = OTP-21.0

# --------------------------------------------------------------------
# Compilation.
# --------------------------------------------------------------------

# default mode
ERLC_OPTS += +warn_export_all +warn_export_vars +warn_unused_import +warn_untyped_record +warn_missing_spec_all
ERLC_OPTS += -Werror

# tests mode
TEST_ERLC_OPTS += +debug_info

# --------------------------------------------------------------------
#  # Dependencies.
# --------------------------------------------------------------------

# 3rd party dev only
dep_teaser = git https://github.com/spylik/teaser                   develop
dep_sync = git https://github.com/rustyio/sync						master

SHELL_DEPS = sync teaser

# --------------------------------------------------------------------
# Development enviroment for TDD ("make shell" to run it).
# --------------------------------------------------------------------

# if we part of deps directory, we using $(CURDIR)../ as DEPS_DIR                                                       
ifeq ($(shell basename $(shell dirname $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST)))))), deps)               
    DEPS_DIR ?= $(shell dirname $(CURDIR))                                                                              
endif                                                                                                                   

SHELL_OPTS = -kernel shell_history enabled -pa ebin/ test/ -eval 'mlibs:discover()' -env ERL_LIBS deps -run mlibs autotest_on_compile

# --------------------------------------------------------------------
# We using erlang.mk 
# --------------------------------------------------------------------

include erlang.mk
