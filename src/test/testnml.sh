#!/bin/sh

killall -INT testnml_internal.sh 2>/dev/null >/dev/null
sleep 2
killall -KILL testnml_internal.sh  2>/dev/null >/dev/null


./testnml_internal.sh >/tmp/testnml.log 2>/tmp/testnml.err  || (echo FAILED;exit 255) || exit 255

exit 0
