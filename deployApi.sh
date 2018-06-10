#!/usr/bin/env bash

REMOTE_SERVE_DIR=/home/ubuntu/whatnext/api
mkdir distApi || true
rm -rf distApi/*
cp -rf src/* distApi/
rm -rf distApi/web/frontend
rm -rf distApi/web/mobile
rm -rf distApi/web/landing
scp -r distApi/* blog:"$REMOTE_SERVE_DIR/"

ssh blog -t '
pkill -f "webserver" ;
'

echo "Deploying"
ssh -n -f blog "bash -c 'cd $REMOTE_SERVE_DIR/web/api && nohup python3 webserver.py  > /dev/null 2>&1 &'"

exit 0
