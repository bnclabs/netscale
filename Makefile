APPS := numeric

export SHELL := /bin/sh
export ECHO := echo
export PROG_INC_DEPS := $(CURDIR)/_releases/incdeps.erl
export ERL_LIBS := $(CURDIR)
export ERLC := erlc
export ERLC_FLAGS :=
export ERL_FLAGS :=
export INCLUDE_FLAGS :=

all :
	@for dir in $(APPS); do $(MAKE) -C $$dir; done

release :
	$(MAKE) -C _releases release

clean :
	rm -rf `find ./ -name erl_crash.dump`
	@for dir in $(APPS); do $(MAKE) -C $$dir clean; done
	$(MAKE) -C _releases clean


cleanall :
	@for dir in $(APPS); do $(MAKE) -C $$dir cleanall; done

