#!/bin/csh

umask 000
echo BEGIN  >>start_servers.log
date >> start_servers.log

if ( -f wtimler.run)  exit;
if ( -f timsvr.run)  exit;

\rm -f  wtimer.log
\rm -f timesvr.log
touch wtimer.run
touch timesvr.run
touch wtimer.log
touch timesvr.log
chmod -f a+rw *.{run,log,err}
nice /depot/www-isd/staff/shackleford/socapplet/wtimer >> wtimer.log &
sleep 2
nice /depot/www-isd/staff/shackleford/socapplet/timesvr >> timesvr.log &
sleep 2


ps -ael | grep time >>start_servers.log
ipcs >> start_servers.log
ls -x  *.{log,err,pid,run} >> start_servers.log
echo END >>start_servers.log
