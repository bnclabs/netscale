include ../inc.mk

ERL_EMUFLAGS += 
ERL_FLAGS +=
ERL_PLAINFLAGS +=

.PHONY : 0.1 netscale-latest check_appspec

0.1 : check_appspec netscale-0.1.script netscale-latest

netscale-0.1.script : $(APPFILES) netscale-0.1.rel
	@$(ECHO) -n "  CODE-CLASH "; $(PROG_MAKEREL) clash
	@$(ECHO)    "  RELEASE" $@; $(PROG_MAKEREL) release netscale-0.1

netscale-latest:
	@$(ECHO) "  Latest netscale.script and netscale.boot available ..." 
	@cp netscale-0.1.script netscale.script
	@cp netscale-0.1.boot netscale.boot

check_appspec :
	@for f in $(APPFILES); do \
		$(ECHO) "  CHECK_APPSPEC" $$f; $(PROG_MAKEREL) childspecs $$f; done

clean :
	rm -rf netscale-0.1.script netscale-0.1.boot netscale.script netscale.boot
