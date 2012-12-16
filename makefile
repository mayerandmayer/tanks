all: Game.html

Game.html: *.elm
	elm --make -r elm-runtime.js Game.elm
