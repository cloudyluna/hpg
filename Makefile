DATADIR=$(XDG_DATA_HOME)
BINDIR=$(HOME)/.local/bin

all: repl

install: install-data
	cabal install exe:hpg -O2 --install-method=copy  --installdir=$(BINDIR) --overwrite-policy=always
	upx $(BINDIR)/hpg

install-data:
	mkdir -p $(DATADIR)/hpg
	cp -r ./data $(DATADIR)/hpg/data

repl:
	ghciwatch --command "cabal repl lib:ProjectGenerator" \
		--watch src --watch test \
		--before-reload-shell "fourmolu --mode inplace src/ProjectGenerator.hs"

clean:
	cabal clean
