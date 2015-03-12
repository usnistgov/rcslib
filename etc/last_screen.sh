#!/bin/sh


screen -r `screen -ls $* | grep Detached | tail -n 1 | awk '{print $1}'`;
