#!/bin/bash

set -e

rm -rf /tmp/api || true
mkdir -p /tmp/api
cp -rf api/api-exe /tmp/api
cp -rf api/legacy /tmp/api

destinationDir=$(pwd)/builds/
filename=$( date "+%Y-%m-%d_%H-%M-%S")-api.tar.gz

cd /tmp
tar -zcvf "$filename" api

cp -rf "$filename" "$destinationDir"
