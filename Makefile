.PHONY: test
current_dir = $(shell pwd)
dist_dir = ${current_dir}/dist

servePage:
	cd dist/ && python3 -m http.server 5001

serveApi:
	source ${current_dir}/src/config.sh && cd ${current_dir}/src/web/api && python3 webserver.py

test:
	./testsBootstrap.sh


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
	pip install flask
	pip install flask_cors
	cd src/web/frontend && elm-install
	(cd src/web/frontend/ ; elm-install)


deploy: build deployFrontend deployApi

deployFrontend:
	scp -r dist/* blog:"/home/ubuntu/whatnext/frontend/"

deployApi: buildApi
	./deployApi.sh

watchFrontend: copyAssets
	my_watch "make build" src/web/frontend

buildScheduler:
	ghc --make src/Scheduler.hs

runScheduler:
	(cd src/ && source ./config.sh && ./Scheduler)

buildApi:
	ghc --make src/Scheduler.hs

buildTriggers: buildScheduler
	ghc --make src/web/api/Triggers.hs

watchApi:
	my_watch "make buildApiDev" src/
