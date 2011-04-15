#!/bin/sh

# This script attempts to start several processes and then kill the rest
# when one finishes.  We haven't figured out how to write it, though.
# See scripts/amb.ml for a version that works.

stop=
pids=

trap "stop=please" USR1

for i; do
  sh -c "$i; kill -USR1 $$ 2>/dev/null" &
  pids="$pids $!"
done

echo $pids

trap "
  ps;
  for pid in $pids; do
    kill -INT -\$pid;
  done;
  ps;
  exit 0
" USR1

if [ -n "$stop" ]; then
  kill -USR1 $$
fi

wait
