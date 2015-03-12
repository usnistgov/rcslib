#!/bin/csh -f 

echo Content type: text/html
cat kill_servers_top.html

umask 000
echo rm 'ls *.log'
rm *.log
echo rm 'ls *.err'
rm *.err
echo rm 'ls *.pid'
rm *.pid
