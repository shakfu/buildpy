
define all-projects
@$(MAKE) -C go/buildpy $1
@$(MAKE) -C hs/buildpy $1
@$(MAKE) -C rs/buildpy $1
@$(MAKE) -C cxx/buildpy $1
@$(MAKE) -C swift/buildpy $1

endef


.PHONY: all build release clean

all: build


build:
	@echo "make $@ for all projects"
	$(call all-projects,"$@")

release:
	@echo "make $@ for all projects"
	$(call all-projects,"$@")
	@cp py/buildpy/buildpy.py ./bin/

clean:
	@echo "make $@ for all projects"
	$(call all-projects,"$@")

