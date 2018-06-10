#!/usr/bin/env bash

REMOTE_SERVE_DIR=/home/ubuntu/whatnext/api
scp -r distApi/* blog:"$REMOTE_SERVE_DIR/"

ssh blog -t '
pkill -f "webserver" ;
'

echo "Deploying"
ssh -n -f blog "bash -c 'cd $REMOTE_SERVE_DIR/api && nohup python3 webserver.py > /dev/null 2>&1 &'"

exit 0
