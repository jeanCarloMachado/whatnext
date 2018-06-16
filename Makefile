.PHONY: test
.PHONY: api
current_dir = $(shell pwd)
dist_dir = ${current_dir}/dist

all: front



test:
	./testsBootstrap.sh

deployFrontend: build
	 scp -r frontend/build/* blog:"/home/ubuntu/whatnext/frontend/"

build:
	(cd frontend ; ELM_APP_API_URL=https://api.thewhatnext.net elm-app build)

install:
	pip install flask
	pip install flask_cors

front:
	cd frontend ; elm-app start

api:
	source ${current_dir}/api/config.sh && cd ${current_dir}/api && WHATNEXT_ENVIROMENT=development python3 webserver.py

buildApi: compileLinux
	./buildApi.sh

deployApi:
	./deployApi.sh

buildAndDeployApi: buildApi deployApi


compileLocal:
	ghc --make api/Scheduler.hs
	# ghc --make api/api/Triggers.hs

containerBash:
	docker run -it wn-build-image bash

compileLinux:
	docker run -it -v ${current_dir}:/wn --entrypoint bash wn-build-image -c "cd /wn ; make compileLocal"

