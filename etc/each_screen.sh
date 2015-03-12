#!/bin/sh


for s in `screen -ls $* | grep Detached | awk '{printf("%s ", $1);}'`; do
    screen -r $s;
done


