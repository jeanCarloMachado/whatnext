#!/usr/bin/env bash

REMOTE_SERVE_DIR=/home/ubuntu/whatnext/api
scp -r api/* blog:"$REMOTE_SERVE_DIR/"

ssh blog -t '
pkill -f "webserver" ;
'
ssh -n -f blog "bash -c 'cd $REMOTE_SERVE_DIR/api && . config.sh && nohup python3 webserver.py > /dev/null 2>&1 &'"

