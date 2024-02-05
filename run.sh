#!/bin/sh

if [ ! -f root.dat ]; then
  mcinit -p $PORT -r $ROOT_TOKEN -c $BLOCKS
fi

exec /usr/bin/mcserver
