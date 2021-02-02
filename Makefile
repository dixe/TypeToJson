run-website:
	elm-live src/Main.elm

build-website:
	elm make src/Main.elm --optimize

build-test-worker:
	elm make src/Worker.elm --optimize --output elm.min.js

run-test-worker:
	node TestGenerator.js

clean:
	rm -f *.min.js
	rm -rf elm-stuff/
