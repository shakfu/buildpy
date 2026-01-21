STATICPY := build/install/python-static/bin/python3
SHAREDPY := build/install/python-shared/bin/python3

.PHONY: all run debug build repl format lint typecheck release \
		check publish publish-test clean reset

all: fullcheck

run: clean
	@COLOR=0 python3 buildpy.py 2>&1 | tee -a log.txt

debug: clean
	@DEBUG=1 COLOR=1 buildpy.py 2>&1 | tee -a log.txt

build:
	@uv build

repl:
	@test -f $(STATICPY) && $(STATICPY) || test -f $(SHAREDPY) && $(SHAREDPY)

format:
	@uv run ruff format buildpy.py

lint:
	@uv run ruff check --fix buildpy.py

typecheck:
	@uv run mypy --strict buildpy.py

release: lint typecheck format build check

check:
	@uv run twine check dist/*

publish:
	@uv run twine upload dist/*

publish-test:
	@uv run twine upload -r testpypi dist/*

clean:
	@rm -rf build/src log.txt

reset: clean
	@rm -rf build


