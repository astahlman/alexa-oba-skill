#!/bin/bash
set -e
lein uberjar
cd target
aws lambda update-function-code --function-name one-bus-away --zip-file fileb://./oba-0.1.0-standalone.jar
