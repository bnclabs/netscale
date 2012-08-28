# ncloud

include ../inc.mk

SRCS = $(wildcard src/*.erl)
OBJS = $(SRCS:src/%.erl=ebin/%.beam)
TESTSRCS = $(wildcard tests/*erl)
TESTOBJS = $(TESTSRCS:tests/%.erl=ebin/%.beam)
APP  = ebin/ncloud.app
SRCDEPS = $(SRCS:.erl=.d)
TESTDEPS = $(TESTSRCS:.erl=.d)

.phony : make-ebin clean

all : make-ebin $(OBJS) $(APP)

eunit : all $(TESTOBJS)

runeunit : eunit
	@erl -boot $(ERL_LIBS)/releases/netscale -noshell \
		 -config $(ERL_LIBS)/_config/test.config -s run_eunit start /a ncloud

make-ebin :
	@mkdir -p ebin

clean :
	rm -rf ebin/*.beam ebin/*.app
	rm -rf src/*.beam
	rm -rf src/*.d

ifneq ($(MAKECMDGOALS),clean)
-include $(SRCDEPS) $(TESTDEPS)
endif

