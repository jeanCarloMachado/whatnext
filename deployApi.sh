#!/usr/bin/env bash

BUILD_DIR=/home/ubuntu/build_dir/
buildFile=$(ls builds --sort=time | head -n1)

echo "Deploying file: $buildFile"

scp -r "builds/$buildFile" blog:"$BUILD_DIR/"
ssh -n -f blog "bash -c 'cd $BUILD_DIR && tar -xzf $buildFile '"

# pkill -f "webserver" ;
# ssh -n -f blog "bash -c 'cd /home/ubuntu/whatnext/api && . config.sh && nohup python3 webserver.py > /dev/null 2>&1 &'"
