.PHONY: test
test:
	./testsBootstrap.sh

all: serve build browserPage

clear:
	./clear.sh

build: copyAssets
	cd src/web && elm-make Index.elm --output dist/index.js
	cd src/web && elm-make Log.elm --output dist/log.js

copyAssets:
	mkdir dist || true
	cp src/web/*.html dist/ || true
	cp src/web/*.css dist/ || true

install:
	elm-package install elm-lang/http

browserPage:
	cd dist/ && python -m http.server 5001 &
	${BROWSER} http://localhost:5001/index.html?env=development

browserProduction:
	${BROWSER} http://thewhatnext.net

deploy: build browserProduction
	./deploy.sh

watch: copyAssets
	make browserPage
	my_watch "make build" .

serve: clear
	source ../config.sh && python webserver.py &
