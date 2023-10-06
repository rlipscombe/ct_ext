PROJECT = ct_ext

CT_HOOKS = ct_ext_summary
CT_OPTS = -ct_hooks $(CT_HOOKS) -erl_args -config test/sys.config

include erlang.mk
