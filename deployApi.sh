#!/usr/bin/env bash

REMOTE_SERVE_DIR=/home/ubuntu/whatnext/api/
rm -rf distApi/*
cp -rf src/* distApi/
rm -rf distApi/web/frontend
scp -r distApi/* blog:"$REMOTE_SERVE_DIR"

ssh blog -t '
pkill -f "SimpleHTTPServer" ;
'

echo "Deploying"
ssh -n -f blog "sh -c 'cd $REMOTE_SERVE_DIR/web && nohup python3 webserver.py  > /dev/null 2>&1 &'"

exit 0
