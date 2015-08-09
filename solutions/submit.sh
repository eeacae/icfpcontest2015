#!/bin/bash -ex

TEAM_ID=276
API_TOKEN="99yl4NyoUwnOmc+iSbPE8C4XxIfjKDRWEu6YHZvcshU="
SOLUTION=$1

curl \
  --user :$API_TOKEN \
  -X POST \
  -H "Content-Type: application/json" \
  -d @$SOLUTION \
  https://davar.icfpcontest.org/teams/$TEAM_ID/solutions
