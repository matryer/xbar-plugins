#!/bin/bash
# <bitbar.title>Amazon SQS Queue Status</bitbar.title>
# <bitbar.version>v0.1</bitbar.version>
# <bitbar.author>Brandon Stilson</bitbar.author>
# <bitbar.author.github>bbstilson</bitbar.author.github>
# <bitbar.desc>Shows current queue stats for specified AWS SQS queues. Updates every minute.</bitbar.desc>
# <bitbar.dependencies>awscli,jq</bitbar.dependencies>
# <bitbar.image>https://i.imgur.com/UFhXIL5.png</bitbar.image>

# Dependencies:
#   awscli (https://aws.amazon.com/cli/)
#   jq (https://stedolan.github.io/jq/)

# Hello, User! Be sure to fill out the following areas:
# 1. Your region in the AWS_REGION.
# 2. The names of the SQS queues you want to track in the QUEUES array.

export PATH="$PATH:/usr/local/bin"

AWS_PROFILE="default"
export AWS_PROFILE

AWS_REGION=""
export AWS_REGION

# Add the queues you want to report on here. Do not use quotes or commas!
# For example: QUEUES=(foo-queue foo-queue-deadletter bar-queue bar-queue-deadletter)
QUEUES=()

QUEUECOLOR="white"
NUMCOLOR="red"

monoq() {
  echo "$1 | font=Monaco trim=false color=$QUEUECOLOR href=$2"
}

monon() {
  echo "$1 | font=Monaco trim=false color=$NUMCOLOR"
}

SQS_URL_ROOT="https://console.aws.amazon.com/sqs/home?region=$AWS_REGION#queue-browser:prefix="

echo "SQS"
echo "---"
for idx in ${!QUEUES[*]}
do
    QUEUE_URL=$(aws sqs get-queue-url --queue-name="${QUEUES[idx]}" | jq .QueueUrl | cut -d '"' -f 2)
    RESP=$(aws sqs get-queue-attributes \
        --queue-url "$QUEUE_URL" \
        --attribute-names ApproximateNumberOfMessages ApproximateNumberOfMessagesNotVisible \
        | jq .Attributes)

    DEPTH=$(echo "$RESP" | jq '.ApproximateNumberOfMessages | tonumber')
    INFLIGHT=$(echo "$RESP" | jq '.ApproximateNumberOfMessagesNotVisible | tonumber')

    monoq "Queue     : ${QUEUES[idx]}" "$SQS_URL_ROOT${QUEUES[idx]}"
    monon "Depth     : $DEPTH"
    monon "In-flight : $INFLIGHT"
done
