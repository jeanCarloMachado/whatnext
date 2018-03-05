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

deploy: build deployFrontend deployApi

deployFrontend:
	scp -r dist/* blog:"/home/ubuntu/whatnext/frontend/"

deployApi: buildApi
	./deployApi.sh

watchFrontend: copyAssets
	my_watch "make build" src/web/frontend

serveApi: clear
	source ${current_dir}/src/config.sh && cd ${current_dir}/src/web/api && python webserver.py &

buildScheduler:
	ghc --make Scheduler.hs -dynamic && ./Scheduler


buildApi:
	docker run -v /home/jean/projects/whatnext:/whatnext -it 77f66f6665b3 bash -c "/opt/ghc/bin/ghc --make /whatnext/src/Scheduler"


buildTriggers:
	docker run -w="$(pwd)" -v /home/jean/.cabal:/home/ubuntu/.cabal -v /home/jean/projects/whatnext:/whatnext -it --entrypoint /opt/ghc/bin/ghc jeancarlomachado/wndev:version1.0 --make /whatnext/src/web/api/Triggers.hs

buildApiDev:
	ghc --make src/Scheduler -dynamic

watchApi:
	my_watch "make buildApiDev" src/
