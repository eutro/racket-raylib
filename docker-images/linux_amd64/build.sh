#!/bin/bash

if [ -z "$RAYLIB_TAG" ]
then echo "RAYLIB_TAG is unset or empty, please set it"
     exit 1
fi

IMG=$(readlink -f "$(dirname "$0")")
cd "$IMG" || exit 1
docker build . -t eutro/racket-raylib:linux-build || exit 1
./run-command.sh | docker run --name racket-raylib_linux-build -i eutro/racket-raylib:linux-build || exit 1
docker cp racket-raylib_linux-build:/home/builder/raylib/libraylib.so.4.0.0 . || exit 1
docker rm racket-raylib_linux-build || exit 1
