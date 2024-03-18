
define all-projects
@$(MAKE) -C go/buildpy $1
@$(MAKE) -C hs/buildpy $1
@$(MAKE) -C rs/buildpy $1
@$(MAKE) -C swift/buildpy $1
endef


.PHONY: all build release clean

all: build


build:
	@echo "make $@ for all projects"
	$(call all-projects)

release:
	@mkdir -p bin
	@mv go/buildpy/buildpy bin/buildpy-go
	@make -C rs/buildpy release

clean:
	@echo "make $@ for all projects"
	$(call all-projects,"$@")

