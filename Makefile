export NSDIR := $(CURDIR)

include inc.mk

.PHONY : $(APPS) $(CHILDAPP_TARGETS) eunit release release% clean cleanall 

all :
	@for dir in $(APPS); do $(MAKE) -C $$dir; done

$(APPS) $(CHILDAPP_TARGETS) :
	@$(MAKE) -C $(call ctarget,$@)

eunit :
	@for dir in $(APPS); do $(MAKE) EUNIT=1 -C $$dir eunit; done

runeunit :
	@for dir in $(APPS); do $(MAKE) EUNIT=1 -C $$dir runeunit; done

release release% :
	$(MAKE) -C releases $(word 2,$(call ctarget,$@))

clean :
	rm -rf `find ./ -name erl_crash.dump`
	@for dir in $(APPS); do $(MAKE) -C $$dir clean; done
	$(MAKE) -C releases clean

cleanall :
	@for dir in $(APPS); do $(MAKE) -C $$dir cleanall; done

