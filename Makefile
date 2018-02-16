.PHONY: test
current_dir = $(shell pwd)
dist_dir = ${current_dir}/dist

all: serveApi build browserPage

test:
	./testsBootstrap.sh

clear:
	./clear.sh

build: copyAssets
	cd src/web/frontend && elm-make Scheduler.elm --output ${dist_dir}/scheduler.js
	cd src/web/frontend && elm-make Log.elm --output ${dist_dir}/log.js
	cd src/web/frontend && elm-make Login.elm --output ${dist_dir}/login.js
	cd src/web/frontend && elm-make Signup.elm --output ${dist_dir}/signup.js

copyAssets:
	mkdir dist || true
	cp src/web/frontend/*.html dist/ || true
	cp src/web/frontend/*.css dist/ || true

install:
	elm-package install elm-lang/http

browserPage:
	cd dist/ && python -m http.server 5001 &
	${BROWSER} https://app.thewhatnext.net

browserProduction:
	${BROWSER} https://app.thewhatnext.net

deployFrontend:
	scp -r dist/* blog:"/home/ubuntu/whatnext/frontend/"

deployApi:
	./deployApi.sh

deploy: build browserProduction deployFrontend deployApi


watch: copyAssets
	make browserPage
	my_watch "make build" .

serveApi: clear
	source ${current_dir}/src/config.sh && cd ${current_dir}/src/web/api && python webserver.py &
