.PHONY: test
test:
	./testsBootstrap.sh
server:
	python ./src/webserver.py
