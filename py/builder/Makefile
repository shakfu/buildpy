# ============================================================================
# VARIABLES & CONSTANTS

# architecture of current system
# currently only macOS x86_64 and arm64 supported
ARCH = $(shell uname -m)
PYTHON = python3

# constants
COLOR_BOLD_CYAN = "\033[1;36m"
COLOR_RESET = "\033[m"

# ============================================================================
# FUNCTIONS

# $(call section,string)
section = @echo ${COLOR_BOLD_CYAN}">>> ${1}"${COLOR_RESET}

# $(call pybuild,name)
define call-builder
$(call section,"builder $1 $2 $3 $4 $5 $6")
@$(PYTHON) -m builder $1 $2 $3 $4 $5 $6
endef


# ============================================================================
# TARGETS

.PHONY: all

all: default


# -----------------------------------------------------------------------
# python targets

.PHONY: python-shared python-shared-pkg python-shared-ext python-shared-tiny \
		python-static  python-static-tiny \
		python-framework python-framework-ext python-framework-pkg \
		python-beeware python-cmake clean

python-shared: clean
	$(call call-builder,"python" "shared" "--install")

python-shared-ext: clean
	$(call call-builder,"python" "shared_ext" "--install" "-p" "$(PYTHON_VERSION)")

python-shared-pkg: clean
	$(call call-builder,"python" "shared_pkg" "--install" "-p" "$(PYTHON_VERSION)")

python-static: clean
	$(call call-builder,"python" "static" "--install" "-p" "$(PYTHON_VERSION)")

python-framework: clean
	$(call call-builder,"python" "framework" "--install" "-p" "$(PYTHON_VERSION)")

python-framework-ext: clean
	$(call call-builder,"python" "framework_ext" "--install" "-p" "$(PYTHON_VERSION)")

python-framework-pkg: clean
	$(call call-builder,"python" "framework_pkg" "--install" "-p" "$(PYTHON_VERSION)")

python-relocatable: clean
	$(call call-builder,"python" "relocatable" "--install")

# python-relocatable: clean
# 	$(call call-builder,"python" "relocatable_pkg")

python-static-tiny:
	$(call call-builder,"python" "static_tiny" "--install" "-p" "$(PYTHON_VERSION)")

python-shared-tiny:
	$(call call-builder,"python" "shared_tiny" "--install" "-p" "$(PYTHON_VERSION)")

python-beeware:
	$(call call-builder,"python" "beeware" "--install")

python-cmake: clean
	$(call call-builder,"python" "cmake" "--install" "-p" "3.9.17")

clean:
	@rm -rf build/src/*

reset:
	@rm -rf build
