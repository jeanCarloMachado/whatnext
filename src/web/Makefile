all: build browserPage

build: copyAssets
	elm-make index.elm --output dist/index.js

copyAssets:
	mkdir dist || true
	cp *.html dist/ || true
	cp *.css dist/ || true

install:
	elm-package install elm-lang/http

browserPage:
	cd dist/ && ${BROWSER} index.html

browserProduction:
	${BROWSER} http://jeancarlomachado.com.br:5001

deploy: build browserProduction
	./deploy.sh

watch: copyAssets
	make browserPage
	my_watch "make build" .

serve:
	source ../config.sh && python webserver.py
