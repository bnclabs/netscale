# numeric

include ../inc.mk

INCLUDE_FLAGS += -I $(CURDIR)/include
ERLC_FLAGS += -o ebin/

SRCS = $(wildcard src/*.erl)
OBJS = $(SRCS:src/%.erl=ebin/%.beam)
TESTSRCS = $(wildcard tests/*erl)
TESTOBJS = $(TESTSRCS:tests/%.erl=ebin/%.beam)
APP  = ebin/numeric.app
DEPS = $(SRCS:.erl=.d)

.phony : make-ebin eunit clean clean-prime-cache clean-factor-cache

all : make-ebin $(OBJS) $(APP)

make-ebin :
	@mkdir -p ebin

eunit : all $(TESTOBJS)
	@$(PROG_RUN_EUNIT) -a numeric

clean :
	rm -rf ebin/*.beam ebin/*.app
	rm -rf src/*.beam
	rm -rf src/*.d

clean-prime-cache :
	rm -rf priv/primes/prime*

clean-factor-cache :
	rm -rf priv/factors/factor*

-include $(DEPS)

