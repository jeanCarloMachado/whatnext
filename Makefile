.PHONY: test
current_dir = $(shell pwd)
dist_dir = ${current_dir}/dist

test:
	./testsBootstrap.sh

#deps

install:
	cd src/frontend && elm-install
	pip install flask
	pip install flask_cors

#frontend

serveFront:
	cd dist/ && python3 -m http.server 80

deployFrontend: build
	scp -r dist/* blog:"/home/ubuntu/whatnext/frontend/"

build: copyAssets
	cd src/frontend/ ; elm-make Scheduler.elm --output ${dist_dir}/scheduler.js
	cd src/frontend/ ; elm-make History.elm --output ${dist_dir}/history.js
	cd src/frontend/ ; elm-make Login.elm --output ${dist_dir}/login.js

copyAssets:
	mkdir dist || true
	cp src/frontend/*.html dist/ || true
	cp src/frontend/*.css dist/ || true
	rm -rf dist/images || true
	cp -rf src/frontend/images dist/images || true

watchFrontend: copyAssets
	my_watch "make build" src/frontend

#backend

serveApi:
	source ${current_dir}/src/api/config.sh && cd ${current_dir}/src/api && python3 webserver.py

deployApi:
	./deployApi.sh


compileBackend:
	ghc --make src/Scheduler.hs
	# ghc --make src/api/Triggers.hs

containerBash:
	docker run -it wn-build-image bash

compileLinux:
	docker run -it -v ${current_dir}:/wn --entrypoint bash wn-build-image -c "cd /wn ; make compileBackend"


