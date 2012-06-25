# nlib

include ../inc.mk

INCLUDE_FLAGS += -I $(CURDIR)/include
ERLC_FLAGS += -o ebin/

SRCS = $(wildcard src/*.erl)
OBJS = $(SRCS:src/%.erl=ebin/%.beam)
TESTSRCS = $(wildcard tests/*erl)
TESTOBJS = $(TESTSRCS:tests/%.erl=ebin/%.beam)
APP  = ebin/nlib.app
DEPS = $(SRCS:.erl=.d)

.phony : make-ebin clean

all : make-ebin $(OBJS) $(APP)

make-ebin :
	@mkdir -p ebin

eunit : all $(TESTOBJS)
	@$(PROG_RUN_EUNIT) -a nlib

clean :
	rm -rf ebin/*
	rm -rf src/*.beam
	rm -rf src/*.d

-include $(DEPS)

