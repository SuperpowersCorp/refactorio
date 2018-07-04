help:
	@cat Makefile

EXE=refactorio

build:
	stack build

clean:
	stack clean

dist-clean:
	\rm -rf .stack-work

ghcid:
	stack exec -- ghcid -c 'stack ghci' --restart stack.yaml

ghcid-devel:
	stack exec -- ghcid						\
		--command "stack ghci redstring"	\
		--test "DevelMain.update"

hlint:
	stack exec hlint .

longboye-all:
	longboye imports app
	longboye imports src
	longboye imports test
	longboye pragmas app
	longboye pragmas src
	longboye pragmas test

run:
	stack exec $(EXE) $(STACK_ARGS) -- refactorio $(ARGS)

test:
	stack test

watch:
	stack build --fast --file-watch

watch-test:
	stack test --fast --file-watch

b: build
g: ghcid
gd: ghcid-devel
hl: hlint
lba: longboye-all
r: run
w: watch
t: test
wt: watch-test

.PHONY: test
