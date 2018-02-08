.PHONY: test
current_dir = $(shell pwd)

all: serveApi build browserPage

test:
	./testsBootstrap.sh

clear:
	./clear.sh

build: copyAssets
	cd src/web && elm-make Index.elm --output ../../dist/index.js
	cd src/web && elm-make Log.elm --output ../../dist/log.js
	cd src/web && elm-make Add.elm --output ../../dist/add.js

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

deployFrontend:
	scp -r dist/* blog:"/home/ubuntu/whatnext/frontend/"

deployApi:
	./deployApi.sh

deploy: build browserProduction deployFrontend deployApi


watch: copyAssets
	make browserPage
	my_watch "make build" .

serveApi: clear
	source ${current_dir}/src/config.sh && cd ${current_dir}/src/web && python webserver.py &
