
build-web:
	elm make src/Main.elm --optimize

build-test-worker:
	elm make src/Worker.elm --optimize --output elm.min.js

test-worker:
	node TestGenerator.js

clean:
	rm -f *.min.js
	rm -rf elm-stuff/
