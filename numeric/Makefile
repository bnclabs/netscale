# numeric

INCLUDE_FLAGS += -I $(CURDIR)/include
ERLC_FLAGS += -o ebin/

SRCS = $(wildcard src/*.erl)
OBJS = $(SRCS:src/%.erl=ebin/%.beam)
APP  = ebin/numeric.app
DEPS = $(SRCS:.erl=.d)

all : $(OBJS) $(APP)

clean :
	rm -rf ebin/*
	rm -rf src/*.beam
	rm -rf src/*.d

# Generic rules

%.d: %.erl
	@$(ECHO) "  IDEPS" $<; $(PROG_INC_DEPS) $< $@ $(INCLUDE_FLAGS)

ebin/%.app : src/%.app
	@$(ECHO) "  CP" $@; cp $< $@

ebin/%.beam : src/%.erl
	@$(ECHO) "  ERLC" $<; $(ERLC) $(INCLUDE_FLAGS) $(ERLC_FLAGS) $<

-include $(DEPS)

