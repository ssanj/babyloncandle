#!/bin/bash

POST_NAME=$(echo $@ | tr ' ' '-')
CURRENT_DATE=$(date '+%Y-%m-%d')
touch "posts/$CURRENT_DATE-$POST_NAME.md"