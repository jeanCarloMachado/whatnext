all: serve build browserPage

clear:
	./clear.sh

build: copyAssets
	elm-make index.elm --output dist/index.js
	elm-make log.elm --output dist/log.html

copyAssets:
	mkdir dist || true
	cp *.html dist/ || true
	cp *.css dist/ || true

install:
	elm-package install elm-lang/http

browserPage:
	cd dist/ && python -m http.server 5001 &
	${BROWSER} http://localhost:5001/index.html?env=development

browserProduction:
	${BROWSER} http://jeancarlomachado.com.br:5001

deploy: build browserProduction
	./deploy.sh

watch: copyAssets
	make browserPage
	my_watch "make build" .

serve: clear
	source ../config.sh && python webserver.py &
