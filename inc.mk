# put common definitions in here

# Application directory :
# -----------------------
#   Applications must be named using alphanumeric characters and the
#   sub-directory in the source tree, containing the application code, must
#   also have same name.

# Make sure that $(NSDIR) is defined in your shell. This should point to the
# netscale source tree.
export ERL_LIBS := $(NSDIR)

# List of Applications
export APPS     := ncloud numeric

# List of .app files for each application
export APPFILES := $(addprefix $(NSDIR)/,$(join $(APPS),$(APPS:%=/ebin/%.app)))

# Create PHONY targets for each application in `netscale` of the form,
# 	<appname>-<targetname>
# which will be interpreted as 
# 	make -C <appname> <targetname>
export APPTARGETS := $(APPS:%=%%)

# Command file to generate .d file, dependency file, for each .erl source file.
export PROG_INC_DEPS := $(NSDIR)/bin/incdeps.erl

# Command file to create a netscale release.
export PROG_MAKEREL  := $(NSDIR)/releases/makerel.erl

# Log directory for error_logger application, in sasl.
export LOGDIR := $(NSDIR)/logs/

# Compiler 
export ERLC := erlc
export INCLUDE_FLAGS := -I $(NSDIR)/ncloud/include -I $(CURDIR)/include
export ERLC_FLAGS := -DNOTEST -o ebin/

# Emulator
export ERL := erl
export ERL_EMUFLAGS := 
export ERL_FLAGS := 
export ERL_PLAINFLAGS :=

export SHELL := /bin/sh
export ECHO := echo

# sub-function to convert '<appname>-<targetname>' to '<appname> <targetname>'
export ctarget = $(subst -, ,$1)

# Generic rules
eunit : ERLC_FLAGS += -DTEST

%.d: %.erl
	@$(ECHO) "  IDEPS" $<; $(PROG_INC_DEPS) $< $@ $(INCLUDE_FLAGS)

ebin/%.app : src/%.app
	@$(ECHO) "  CP" $@; cp $< $@

ebin/%.beam : src/%.erl
	@$(ECHO) "  ERLC" $<; $(ERLC) $(INCLUDE_FLAGS) $(ERLC_FLAGS) $<

ebin/%.beam : tests/%.erl
	@$(ECHO) "  ERLC" $<; $(ERLC) $(INCLUDE_FLAGS) $(ERLC_FLAGS) $<
