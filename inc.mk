# put common definitions in here

export APPS := nlib numeric
export APPFILES := $(addprefix $(NSDIR)/,$(join $(APPS),$(APPS:%=/ebin/%.app)))
export CHILDAPP_TARGETS := $(APPS:%=%%)

export SHELL := /bin/sh
export ECHO := echo
export ERL_LIBS := $(NSDIR)
export PROG_INC_DEPS := $(NSDIR)/bin/incdeps.erl
export PROG_MAKEREL := $(NSDIR)/releases/makerel.erl
export PROG_RUN_EUNIT := $(NSDIR)/bin/run_eunit.erl

export ERLC := erlc
export INCLUDE_FLAGS := -I $(NSDIR)/nlib/include
export ERLC_FLAGS := 

export ERL := erl
export ERL_EMUFLAGS := 
export ERL_FLAGS := -boot_var NSLIB $(CIRDIR)
export ERL_PLAINFLAGS :=

export ctarget = $(subst -, ,$1)

# Generic rules

%.d: %.erl
	@$(ECHO) "  IDEPS" $<; $(PROG_INC_DEPS) $< $@ $(INCLUDE_FLAGS)

ebin/%.app : src/%.app
	@$(ECHO) "  CP" $@; cp $< $@

ebin/%.beam : src/%.erl
	@$(ECHO) "  ERLC" $<; $(ERLC) $(INCLUDE_FLAGS) $(ERLC_FLAGS) $<

ebin/%.beam : tests/%.erl
	@$(ECHO) "  ERLC" $<; $(ERLC) $(INCLUDE_FLAGS) $(ERLC_FLAGS) $<

