.PHONY: test
current_dir = $(shell pwd)
dist_dir = ${current_dir}/dist

all: serveApi servePage 

test:
	./testsBootstrap.sh

clear:
	./clear.sh

build: copyAssets
	cd src/web/frontend && elm-make Scheduler.elm --output ${dist_dir}/scheduler.js
	cd src/web/frontend && elm-make History.elm --output ${dist_dir}/history.js
	cd src/web/frontend && elm-make Login.elm --output ${dist_dir}/login.js

copyAssets:
	mkdir dist || true
	cp src/web/frontend/*.html dist/ || true
	cp src/web/frontend/*.css dist/ || true
	rm -rf dist/images || true
	cp -rf src/web/frontend/images dist/images || true

install:
	elm-package install elm-lang/http

servePage:
	cd dist/ && python -m http.server 5001 &


deployFrontend:
	scp -r dist/* blog:"/home/ubuntu/whatnext/frontend/"

deployApi:
	./deployApi.sh

deploy: build deployFrontend deployApi

watch: copyAssets
	my_watch "make build" .

serveApi: clear
	source ${current_dir}/src/config.sh && cd ${current_dir}/src/web/api && python webserver.py &

buildScheduler:
	ghc --make Scheduler.hs -dynamic && ./Scheduler 
	

