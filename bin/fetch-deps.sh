#!/usr/bin/env sh

ROOT_DIR="$(dirname "$0")"
INCLUDES_DIR="$ROOT_DIR/../include"
TB_ROOT_DIR="$INCLUDES_DIR/tigerbeetle"
TB_GIT_URL="git@github.com:tigerbeetle/tigerbeetle.git"

echo "Fetching dependencies (tigerbeetle)..."

mkdir -p $INCLUDES_DIR
git clone $TB_GIT_URL $TB_ROOT_DIR
