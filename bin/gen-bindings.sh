#!/usr/bin/env sh

ROOT_DIR="$(dirname "$0")"
INCLUDES_DIR="$ROOT_DIR/../include"
TB_ROOT_DIR="$INCLUDES_DIR/tigerbeetle"
TB_INCLUDE_OPT="-I${TB_ROOT_DIR}/src/clients/c"

echo "Generating bindings..."

c2hs --cppopts=${TB_INCLUDE_OPT} src/Database/TigerBeetle/Internal/TbClient.chs
