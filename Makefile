DATADIR=$(XDG_DATA_HOME)
BINDIR=$(HOME)/.local/bin
PROJECT_NAME=ProjectGenerator
EXE=hpg

all: repl

install: install-data
	cabal install exe:$(EXE) -O2 --install-method=copy  --installdir=$(BINDIR) --overwrite-policy=always
	upx $(BINDIR)/$(EXE)

install-data:
	mkdir -p $(DATADIR)/$(EXE)
	cp -r ./data $(DATADIR)/$(EXE)

repl:
	ghciwatch --command "cabal repl lib:$(PROJECT_NAME)" \
		--watch src --watch test \
		--before-reload-shell "fourmolu --mode inplace src/$(PROJECT_NAME).hs"

clean:
	cabal clean
