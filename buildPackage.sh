#!/bin/bash

set -e

buildDir=/tmp/api

rm -rf $buildDir || true
mkdir -p $buildDir
cp -rf api/api $buildDir
cp -rf api/fileStorageGateway $buildDir

destinationDir=$(pwd)/builds/
filename=$( date "+%Y-%m-%d_%H-%M-%S")-api.tar.gz

cd /tmp
tar -zcvf "$filename" api

cp -rf "$filename" "$destinationDir"
