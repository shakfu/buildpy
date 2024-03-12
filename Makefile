
define all-projects
@$(MAKE) -C rs/buildpy $1
@$(MAKE) -C go/buildpy $1
@$(MAKE) -C swift/buildpy $1
endef


.PHONY: all build clean

all: build


build:
	@echo "make $@ for all projects"
	$(call all-projects)

clean:
	@echo "make $@ for all projects"
	$(call all-projects,"$@")
