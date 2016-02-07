#!/bin/bash
set -e
LOG_STREAMS=$(aws logs describe-log-streams --log-group-name /aws/lambda/one-bus-away)
LOG_STREAM_NAME=$(echo $LOG_STREAMS | jq '.logStreams | sort_by("lastEventTimestamp") | reverse | .[0] | .logStreamName' | sed 's/"//g')
aws logs get-log-events --log-group-name /aws/lambda/one-bus-away --log-stream-name "$LOG_STREAM_NAME"
