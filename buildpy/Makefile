STATICPY := build/install/python-static/bin/python3
SHAREDPY := build/install/python-shared/bin/python3

.PHONY: all build repl clean reset


all: build


build: clean
	@python3 buildpy.py 2>&1 | tee -a log.txt


repl:
	@test -f $(STATICPY) && $(STATICPY) || test -f $(SHAREDPY) && $(SHAREDPY)


clean:
	@rm -rf build/src log.txt build/install/python


reset:
	@rm -rf build log.txt

