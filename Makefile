all: game.native

game.native: game.ml _build/board.cma
	corebuild game.native

_build/board.cma: board.ml
	corebuild board.cma

clean:
	rm -rf _build
	rm game.native
