#! /bin/sh

set -x

if [ x${PLAT} = x ] ; then
    PLAT=~/rcslib/etc/platname
    export PLAT
fi

\rm -f *.log
\rm -f *.xml
\rm -f *.xml.~*~

RCS_DEBUG_FLAG=0
export RCS_DEBUG_FLAG
XMLOUT_LOG_FILE=xmloutperf.log
export XMLOUT_LOG_FILE
XMLSCHEMAOUT_LOG_FILE=xmlschemaoutperf.log
export XMLSCHEMAOUT_LOG_FILE
CGTESTSVR1_LOG_FILE=cgtestsvr1perf.log
export CGTESTSVR1_LOG_FILE
CGTESTSVR2_LOG_FILE=cgtestsvr2perf.log
export CGTESTSVR2_LOG_FILE
CGTESTREAD_LOG_FILE=cgtestreadperf.log
export CGTESTREAD_LOG_FILE
CGTESTWRITE_LOG_FILE=cgtestwriteperf.log
export CGTESTWRITE_LOG_FILE
#DEBUG_MEMORY=1
#export DEBUG_MEMORY


killall -INT cgtestsvr1
killall -INT cgtestsvr2
sleep 2
killall -KILL cgtestsvr1
killall -KILL cgtestsvr2

../../../rcslib/plat/${PLAT}/bin/cgtestsvr1 &
jobtokill=$!
../../../rcslib/plat/${PLAT}/bin/cgtestsvr2 &
jobtokill2=$!

sleep 5
../../../rcslib/plat/${PLAT}/bin/cgtestwrite  && \
sleep 2 && \
../../../rcslib/plat/${PLAT}/bin/cgtestread

kill -INT $jobtokill
kill -INT $jobtokill2

sleep 1

kill -KILL $jobtokill
kill -KILL $jobtokill2


killall -INT cgtestsvr1
killall -INT cgtestsvr2
sleep 2
killall -KILL cgtestsvr1
killall -KILL cgtestsvr2



