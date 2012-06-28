# nlib

include ../inc.mk

INCLUDE_FLAGS += -I $(CURDIR)/include
ERLC_FLAGS += -o ebin/

SRCS = $(wildcard src/*.erl)
OBJS = $(SRCS:src/%.erl=ebin/%.beam)
TESTSRCS = $(wildcard tests/*erl)
TESTOBJS = $(TESTSRCS:tests/%.erl=ebin/%.beam)
APP  = ebin/nlib.app
SRCDEPS = $(SRCS:.erl=.d)
TESTDEPS = $(TESTSRCS:.erl=.d)

.phony : make-ebin clean

all : make-ebin $(OBJS) $(APP)

make-ebin :
	@mkdir -p ebin

eunit : all $(TESTOBJS)

runeunit : eunit
	@erl -noshell -boot $(ERL_LIBS)/releases/netscale -s run_eunit start /a nlib


clean :
	rm -rf ebin/*.beam ebin/*.app
	rm -rf src/*.beam
	rm -rf src/*.d

-include $(SRCDEPS) $(TESTDEPS)

