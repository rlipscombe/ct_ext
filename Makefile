PROJECT = ct_ext

CT_HOOKS = ct_ext_summary
CT_OPTS = -ct_hooks $(CT_HOOKS) -erl_args -config test/sys.config

TEST_DEPS = \
	unite

dep_unite = git https://github.com/eproxus/unite v0.4.0

EUNIT_OPTS = no_tty, {report, {unite_compact, []}}

include $(if $(ERLANG_MK_FILENAME),$(ERLANG_MK_FILENAME),erlang.mk)
