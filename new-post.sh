#!/bin/bash

if [ $# -eq 0 ]; then
  echo "usage: new-post title"
  exit 1
fi

POST_NAME=$(echo $@ | tr ' ' '-')
CURRENT_DATE=$(date '+%Y-%m-%d')
touch "posts/$CURRENT_DATE-$POST_NAME.md"
