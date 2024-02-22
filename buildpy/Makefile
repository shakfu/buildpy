

.PHONY: all build repl clean reset


all: build


build: clean
	@python3 buildpy.py 2>&1 | tee -a log.txt


repl:
	@build/install/python/bin/python3


clean:
	@rm -rf build/src log.txt build/install/python


reset:
	@rm -rf build log.txt

