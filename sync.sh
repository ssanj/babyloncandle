#!/bin/bash
set -e
./site clean && ./site rebuild && ./site deploy
