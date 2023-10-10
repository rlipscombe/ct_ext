PROJECT = ct_ext

TEST_DEPS = \
	unite

dep_unite = git https://github.com/eproxus/unite
dep_unite_commit = v0.4.0

EUNIT_OPTS = no_tty, {report, {unite_compact, [profile]}}

CT_HOOKS = ct_ext_summary
CT_OPTS = -ct_hooks $(CT_HOOKS) -erl_args -config test/sys.config

include $(if $(ERLANG_MK_FILENAME),$(ERLANG_MK_FILENAME),erlang.mk)
