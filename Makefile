PROJECT = ecsv
PROJECT_VERSION = $(shell git describe --tag --abbrev=0)

app:: rebar.config

include $(if $(ERLANG_MK_FILENAME),$(ERLANG_MK_FILENAME), erlang.mk)

ERLC_OPTS += $(ERLC_COMPILE_OPTS)
TEST_ERLC_OPTS += $(ERLC_COMPILE_OPTS)
