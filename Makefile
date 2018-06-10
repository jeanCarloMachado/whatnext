.PHONY: test
current_dir = $(shell pwd)
dist_dir = ${current_dir}/dist


test:
	./testsBootstrap.sh

#deps

install:
	pip install flask
	pip install flask_cors
	cd src/web/frontend && elm-install
	(cd src/web/frontend/ ; elm-install)

#frontend

serveFront:
	cd dist/ && python3 -m http.server 5001


deployFrontend:
	scp -r dist/* blog:"/home/ubuntu/whatnext/frontend/"

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

watchFrontend: copyAssets
	my_watch "make build" src/web/frontend

#backend

serveApi:
	source ${current_dir}/src/config.sh && cd ${current_dir}/src/web/api && python3 webserver.py


deployApi:
	./deployApi.sh


compileBackend:
	ghc --make src/Scheduler.hs
	# ghc --make src/web/api/Triggers.hs


containerBash:
	docker run -it wn-build-image bash

compileLinux:
	docker run -it -v ${current_dir}:/wn --entrypoint bash wn-build-image -c "cd /wn ; make compileBackend"


