#! /usr/bin/env sh

# This script serves the blog with live updates
# from editing scribble files. does not detect new files.

trap "trap - SIGTERM && kill -- -$$" SIGINT SIGTERM EXIT

raco frog -s &
server_pid=$!
echo "blog will be running at http://localhost:3000/blog/"
echo "use CTRL-C to exit"
find . -name '*.scrbl' -o -name '*.md' | entr -s 'raco frog -b'

