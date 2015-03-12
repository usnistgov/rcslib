#!/bin/sh

umask 000
echo Content type: text/html
cat kill_servers_top.html

echo kill -2 `cat timesvr.pid`
kill -2 `cat timesvr.pid`
echo kill -2 `cat wtimer.pid` 
kill -2 `cat wtimer.pid` 

#\rm -f timesvr.pid >> kill_server.log
#\rm -f wtimer.pid >> kill_server.log

cat kill_servers_bottom.html

