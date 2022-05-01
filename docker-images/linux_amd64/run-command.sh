#!/bin/sh

echo "su builder -c 'cd && git clone --depth 1 --branch $RAYLIB_TAG https://github.com/raysan5/raylib && cd raylib/src && make PLATFORM=PLATFORM_DESKTOP RAYLIB_LIBTYPE=SHARED'"
