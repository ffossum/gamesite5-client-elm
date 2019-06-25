#!/bin/sh

set -e

elm make --optimize --output dist/elm.js src/Main.elm
