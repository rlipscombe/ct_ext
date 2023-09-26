PROJECT = ct_report

CT_HOOKS = ct_summary
CT_OPTS = -ct_hooks $(CT_HOOKS) -erl_args -config test/sys.config

include erlang.mk
