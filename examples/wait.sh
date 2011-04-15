#!/bin/sh

(
    sleep 1
    echo b 
    sleep 1
) & 
PROC=$!
echo a
wait $PROC
echo c