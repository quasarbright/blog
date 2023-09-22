#! /usr/bin/env sh

# This script serves the blog with live updates
# from editing scribble files. does not detect new files.

trap "trap - SIGTERM && kill -- -$$" SIGINT SIGTERM EXIT

raco frog -s &
server_pid=$!
echo "server pid: $server_pid"
echo "use CTRL-C to exit"
find . -name '*.scrbl' | entr -s 'raco frog -b'

