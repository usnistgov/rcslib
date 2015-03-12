#!/bin/csh

./start_servers.cgi
echo Content-type: text/html
echo
echo
cat socapplet.html
