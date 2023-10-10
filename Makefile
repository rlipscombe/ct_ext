PROJECT = ct_ext

TEST_DEPS = \
	meck \
	unite

dep_unite = git https://github.com/eproxus/unite
dep_unite_commit = v0.4.0

EUNIT_OPTS = no_tty, {report, {unite_compact, []}}

CT_HOOKS = ct_ext_summary
CT_OPTS = -ct_hooks $(CT_HOOKS) -erl_args -config test/sys.config

top: app eunit ct

include $(if $(ERLANG_MK_FILENAME),$(ERLANG_MK_FILENAME),erlang.mk)
