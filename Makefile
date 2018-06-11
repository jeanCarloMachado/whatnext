.PHONY: test
current_dir = $(shell pwd)
dist_dir = ${current_dir}/dist

test:
	./testsBootstrap.sh


deployFrontend: build
	 scp -r frontend/build/* blog:"/home/ubuntu/whatnext/frontend/"

build:
	(cd frontend ; ELM_APP_API_URL=https://api.thewhatnext.net elm-app build)


#backend

install:
	pip install flask
	pip install flask_cors

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


