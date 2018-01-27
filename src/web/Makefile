all:
	elm-make index.elm --output index.js
	${BROWSER} index.html
install:
	elm-package install elm-lang/http

watch:
	${BROWSER} index.html
	my_watch "elm-make index.elm --output index.js" .

serve:
	source ../config.sh && python webserver.py
