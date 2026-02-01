repl:
	ghciwatch --command "cabal repl lib:ProjectGenerator" \
		--watch src --watch test \
		--before-reload-shell "fourmolu --mode inplace src/ProjectGenerator.hs"
