PROJECT = ct_report

CT_HOOKS = ct_summary
CT_OPTS = -ct_hooks $(CT_HOOKS)

include erlang.mk
