export NSDIR := $(CURDIR)

include inc.mk

.PHONY : $(APPS) $(APPTARGETS) eunit release release% 
.PHONY : clean cleanlogs cleanall 

all :
	@for dir in $(APPS); do $(MAKE) -C $$dir; done

$(APPS) $(APPTARGETS) :
	@$(MAKE) -C $(call ctarget,$@)

eunit :
	@for dir in $(APPS); do $(MAKE) EUNIT=1 -C $$dir eunit; done

runeunit :
	@for dir in $(APPS); do $(MAKE) EUNIT=1 -C $$dir runeunit; done

release release% :
	$(MAKE) -C releases $(word 2,$(call ctarget,$@))

push-github:
	# git remote add origin git@github.com:prataprc/netscale.git
	git push -u origin master

clean : cleandumps
	@for dir in $(APPS); do $(MAKE) -C $$dir clean; done
	$(MAKE) -C releases clean

cleandumps :
	rm -rf `find ./ -name erl_crash.dump`

cleanlogs :
	rm -rf $(LOGDIR)/*

cleanall : cleandumps cleanlogs clean

