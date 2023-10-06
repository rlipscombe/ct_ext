PROJECT = ct_ext

CT_HOOKS = ct_ext_summary
CT_OPTS = -ct_hooks $(CT_HOOKS) -erl_args -config test/sys.config

include $(if $(ERLANG_MK_FILENAME),$(ERLANG_MK_FILENAME),erlang.mk)
