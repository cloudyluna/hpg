repl:
	ghciwatch --command "cabal repl lib:ProjectGenerator" \
		--watch src --watch test
