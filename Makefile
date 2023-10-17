PROJECT = ct_ext

CT_HOOKS = ct_ext_summary
CT_HOOKS += and ct_ext_debug
CT_OPTS = -ct_hooks $(CT_HOOKS) -erl_args -config test/sys.config

include $(if $(ERLANG_MK_FILENAME),$(ERLANG_MK_FILENAME),erlang.mk)
