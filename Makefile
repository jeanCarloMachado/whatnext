.PHONY: test
.PHONY: api
current_dir = $(shell pwd)
dist_dir = ${current_dir}/dist

all:
	#"Run make front and make api to start developing"
	#to deploy: make deployFront ; make deployApi

test:
	./testsBootstrap.sh

deployFront: buildFront
	ssh -n -f blog 'rm -rf /home/ubuntu/whatnext/frontend/* ; mkdir -p /home/ubuntu/whatnext/frontend/'
	 scp -r frontend/build/* blog:"/home/ubuntu/whatnext/frontend/"

buildFront:
	(cd frontend ; ELM_APP_API_URL=https://api.thewhatnext.net elm-app build)

envSetup: initializeData
	yarn global add create-elm-app
	yarn global add elm-github-install


front:
	cd frontend ; elm-app start

api:
	source ${current_dir}/api/fileStorageGateway/config.sh &&  export WHATNEXT_SRC=${current_dir}/api/fileStorageGateway &&  ${current_dir}/api/api

buildApi: compileContainer
	./buildPackage.sh

deployApi:
	./deployApi.sh

buildAndDeployApi: buildApi deployApi

containerBash:
	docker run -it wn-build-image bash

watch:
	cd api ; stack build --file-watch

compile:
	cd api ;  stack install --allow-different-user
	cp -rf ${HOME}/.local/bin/scheduler api/fileStorageGateway/Scheduler
	cp ${HOME}/.local/bin/api api/api
	# ghc --make api/api/Triggers.hs

compileContainer:
	docker run -it -v ${current_dir}:/wn --entrypoint bash wn-build-image -c "cd /wn ; make compile && cp /root/.local/bin/api /wn/api/api"

copyServerData:
	scp -r 'blog:~/whatnext_data' /tmp/data
	make initializeDir
	cp -rf /tmp/data/* /data/whatnext

cleanupData:
	sudo rm -rf /data || true

initializeDir: cleanupData
	sudo mkdir /data
	sudo chown -R ${USER} /data
	mkdir /data/whatnext

initializeData: initializeDir
	touch /data/whatnext/whatnext_users
	mkdir /data/whatnext/users


deployAll: buildAndDeployApi deployFront

