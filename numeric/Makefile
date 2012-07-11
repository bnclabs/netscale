# numeric

include ../inc.mk

SRCS = $(wildcard src/*.erl)
OBJS = $(SRCS:src/%.erl=ebin/%.beam)
TESTSRCS = $(wildcard tests/*erl)
TESTOBJS = $(TESTSRCS:tests/%.erl=ebin/%.beam)
APP  = ebin/numeric.app
SRCDEPS = $(SRCS:.erl=.d)
TESTDEPS = $(TESTSRCS:.erl=.d)

.phony : make-ebin eunit clean clean-prime-cache clean-factor-cache

all : make-ebin $(OBJS) $(APP)

make-ebin :
	@mkdir -p ebin

eunit : all $(TESTOBJS)

runeunit : eunit
	@erl -noshell -boot $(ERL_LIBS)/releases/netscale -s run_eunit start /a numeric

clean :
	rm -rf ebin/*.beam ebin/*.app
	rm -rf src/*.beam
	rm -rf src/*.d

clean-prime-cache :
	rm -rf priv/primes/prime*

clean-factor-cache :
	rm -rf priv/factors/factor*

ifneq ($(MAKECMDGOALS),clean)
-include $(SRCDEPS) $(TESTDEPS)
endif

