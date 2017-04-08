#! /usr/bin/env bash

STACK_IMAGE=${1:-7.8.4}

docker run \
  -v `pwd`/../aeson-streams:/home/gusdev/aeson-streams \
  -v `pwd`:/home/gusdev/hicks \
  images.reesd.com/reesd/stack:$STACK_IMAGE \
  cabal install aeson-streams/aeson-streams.cabal hicks/hicks.cabal
