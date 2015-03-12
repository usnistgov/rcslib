#!/bin/sh

set -x

PLAT=`../../etc/platname`

../../plat/$PLAT/bin/cgtestsvr1 &

sleep 2

../../plat/$PLAT/bin/cgtestsvr2 &

sleep 2


../../plat/$PLAT/bin/cgtestread
java -classpath ../../plat/java/lib/rcs.jar:. exread

../../plat/$PLAT/bin/cgtestwrite

../../plat/$PLAT/bin/cgtestread
java -classpath ../../plat/java/lib/rcs.jar:. exread

java -classpath ../../plat/java/lib/rcs.jar:. exwrite

../../plat/$PLAT/bin/cgtestread
java -classpath ../../plat/java/lib/rcs.jar:. exread


killall -INT cgtestsvr1
killall -INT cgtestsvr2

sleep 2

killall -KILL cgtestsvr1
killall -KILL cgtestsvr2

