#!/usr/bin/env bash

export BUILD_DIR=/home/ubuntu/build_dir/
export DEPLOY_DIR=/home/ubuntu/whatnext/
export buildFile=$(ls builds --sort=time | head -n1)

echo "Deploying file: $buildFile"

scp -r "builds/$buildFile" blog:"$BUILD_DIR/"
ssh -n -f blog "bash -c 'cd $DEPLOY_DIR/ ; rm -rf api ;  tar -xzf $BUILD_DIR/$buildFile '"
#kill process
ssh -n -f blog 'kill -9 $(ps -aux | grep "api" | cut -d " " -f 4 | head -n 1)'
ssh -n -f blog "bash -c 'cd $DEPLOY_DIR/api/legacy ;  source config.sh ; nohup ../api &'"

