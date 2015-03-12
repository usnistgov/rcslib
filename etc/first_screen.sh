#!/bin/sh


screen -r `screen -ls $* | grep Detached | head -n 1 | awk '{print $1}'`;
