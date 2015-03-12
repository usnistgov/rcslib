#! /bin/sh

set -x

echo "Test Date:"
date
echo "Current directory:"
pwd
echo "SET"
set
echo "PRINTENV"
printenv


if [ x${PLAT} = x ] ; then
    PLAT=`../../etc/platname`
    export PLAT
fi

make PLAT=${PLAT} -C .. || (echo FAILED;exit 255) || exit 255
make PLAT=${PLAT}  || (echo FAILED;exit 255) || exit 255
make PLAT=${PLAT}  nml_test_server nml_test_write \
    nml_test_read nml_test_blocking_read || (echo FAILED;exit 255) || exit 255


builddir=../../plat/${PLAT}/bin
srcdir=../../


if test -f /tmp/test.nml ; then
    ${builddir}/nmlclean /tmp/test.nml
    \rm -f /tmp/test.nml
fi

echo "______________________________________"
echo "Starting NML TESTS:"

killall -INT nml_test_blocking_read
killall -INT nml_test_read
killall -INT nml_test_write
killall -INT nml_test_server

sleep 2

killall -KILL nml_test_blocking_read
killall -KILL nml_test_read
killall -KILL nml_test_write
killall -KILL nml_test_server

echo "Kill any nml test programs still running."
procstokill=`ps -ae | grep nml_te | awk '{print $1}'`
if test  x != "x${procstokill}" ; then
    kill -INT $procstokill
    sleep 1
    procstokill=`ps -ae | grep nml_te | awk '{print $1}'`
    if test x != "x${procstokill}" ; then
	echo ."$procstokill".
	kill -KILL $procstokill 
	sleep 1
    fi
fi

echo "Testing nmlcfg  . . ."

${builddir}/nmlcfg ${srcdir}/src/test/test.nml2 -o /tmp/test.nml
if test $? -ne 0 ; then
    echo "nmlcfg test failed"
    exit 255
fi

if test ! -f /tmp/test.nml ; then
    echo "nmlcfg test failed"
    exit 255
fi

echo "nmlcfg test Passed"

echo "Starting server . . ."
if test -f nml_test_server.running ; then \rm -f nml_test_server.running; fi
${builddir}/nml_test_server b1 b1s /tmp/test.nml &
sleep 1;
tries=0;
while true ; do
    echo "waiting for nml_test_server..."
    sleep 1;
    let tries++;
    if test ${tries} -gt 40 ; then
	echo " timedout.";
	exit 2;
    fi
    echo -n ".";

    if test -f "nml_test_server.running" ; then
	echo " done.";
	break;
    fi
done;

sleep 1


echo "Testing local write . . . "
${builddir}/nml_test_write b1 lw /tmp/test.nml 7
if test $? -ne 0 ; then 
    echo "Local write test failed."
    echo "Kill any nml test programs still running."
    procstokill=`ps -ae | grep nml_te | awk '{print $1}'`
    if test  x != "x${procstokill}" ; then
	kill -INT $procstokill
	sleep 1
	procstokill=`ps -ae | grep nml_te | awk '{print $1}'`
	if test x != "x${procstokill}" ; then
	    echo ."$procstokill".
	    kill -KILL $procstokill 
	    sleep 1
	fi
    fi
    exit 255
fi
echo "local write test passed."


echo "Testing local read . . . "
${builddir}/nml_test_read b1 lr /tmp/test.nml 7
if test $? -ne 0 ; then 
    echo "Local read test failed."
    echo "Kill any nml test programs still running."
    procstokill=`ps -ae | grep nml_te | awk '{print $1}'`
    if test  x != "x${procstokill}" ; then
	kill -INT $procstokill
	sleep 1
	procstokill=`ps -ae | grep nml_te | awk '{print $1}'`
	if test x != "x${procstokill}" ; then
	    echo ."$procstokill".
	    kill -KILL $procstokill 
	    sleep 1
	f
    fi
    echo "Kill any nml test programs still running."
    procstokill=`ps -ae | grep nml_te | awk '{print $1}'`
    exit 255
fi
echo "local read test passed."


echo "Kill any nml test programs still running."
procstokill=`ps -ae | grep nml_te | awk '{print $1}'`
if test  x != "x${procstokill}" ; then
    kill -INT $procstokill
    sleep 1
    procstokill=`ps -ae | grep nml_te | awk '{print $1}'`
    if test x != "x${procstokill}" ; then
	echo ."$procstokill".
	kill -KILL $procstokill 
	sleep 1
    fi
fi

echo "Starting server . . ."
echo "Starting server . . ."
if test -f nml_test_server.running ; then \rm -f nml_test_server.running; fi
${builddir}/nml_test_server b1 b1s /tmp/test.nml &
sleep 1;
tries=0;
while true ; do
    echo "waiting for nml_test_server..."
    sleep 1;
    let tries++;
    if test ${tries} -gt 40 ; then
	echo " timedout.";
	exit 2;
    fi
    echo -n ".";

    if test -f "nml_test_server.running" ; then
	echo " done.";
	break;
    fi
done;

sleep 1


( sleep 5 ; ${builddir}/nml_test_write b1 lw /tmp/test.nml 8; \
    ${builddir}/nml_test_blocking_read b1 lr /tmp/test.nml 30 8 \
 ) &

echo "Testing local blocking read . . . "
${builddir}/nml_test_blocking_read b1 lr /tmp/test.nml 30 8 &
${builddir}/nml_test_blocking_read b1 lr /tmp/test.nml 30 8 &
${builddir}/nml_test_blocking_read b1 lr /tmp/test.nml 30 8 &
sleep 1
${builddir}/nml_test_blocking_read b1 lr /tmp/test.nml 30 8 &
${builddir}/nml_test_blocking_read b1 lr /tmp/test.nml 30 8
if test $? -ne 0 ; then 
    echo "Local blocking read test failed."
    echo "Kill any nml test programs still running."
    procstokill=`ps -ae | grep nml_te | awk '{print $1}'`
    if test  x != "x${procstokill}" ; then
	kill -INT $procstokill
	sleep 1
	procstokill=`ps -ae | grep nml_te | awk '{print $1}'`
	if test x != "x${procstokill}" ; then
	    echo ."$procstokill".
	    kill -KILL $procstokill 
	    sleep 1
	fi
    fi
    exit 255
fi
echo "local read blocking test passed."


echo "Testing remote write . . . "
${builddir}/nml_test_write b1 rw /tmp/test.nml 7
if test $? -ne 0 ; then 
    echo "Remote write test failed."
    echo "Kill any nml test programs still running."
    procstokill=`ps -ae | grep nml_te | awk '{print $1}'`
    if test  x != "x${procstokill}" ; then
	kill -INT $procstokill
	sleep 1
	procstokill=`ps -ae | grep nml_te | awk '{print $1}'`
	if test x != "x${procstokill}" ; then
	    echo ."$procstokill".
	    kill -KILL $procstokill 
	    sleep 1
	fi
    fi
    exit 255
fi
echo "remote write test passed."


echo "Testing remote read . . . "
${builddir}/nml_test_read b1 rr /tmp/test.nml 7
if test $? -ne 0 ; then 
    echo "Remote write read failed."
    echo "Kill any nml test programs still running."
    procstokill=`ps -ae | grep nml_te | awk '{print $1}'`
    if test  x != "x${procstokill}" ; then
	kill -INT $procstokill
	sleep 1
	procstokill=`ps -ae | grep nml_te | awk '{print $1}'`
	if test x != "x${procstokill}" ; then
	    echo ."$procstokill".
	    kill -KILL $procstokill 
	    sleep 1
	fi
    fi
    exit 255
fi
echo "remote read test passed."


echo "Kill any nml test programs still running."
procstokill=`ps -ae | grep nml_te | awk '{print $1}'`
if test  x != "x${procstokill}" ; then
    kill -INT $procstokill
    sleep 1
    procstokill=`ps -ae | grep nml_te | awk '{print $1}'`
    if test x != "x${procstokill}" ; then
	echo ."$procstokill".
	kill -KILL $procstokill 
	sleep 1
    fi
fi


echo "Starting server . . ."
if test -f nml_test_server.running ; then \rm -f nml_test_server.running; fi
${builddir}/nml_test_server b1 b1s /tmp/test.nml &
sleep 1;
tries=0;
while true ; do
    echo "waiting for nml_test_server..."
    sleep 1;
    let tries++;
    if test ${tries} -gt 40 ; then
	echo " timedout.";
	exit 2;
    fi
    echo -n ".";

    if test -f "nml_test_server.running" ; then
	echo " done.";
	break;
    fi
done;
sleep 1


( sleep 5 ; ${builddir}/nml_test_write b1 rw /tmp/test.nml 8 ; \
    ${builddir}/nml_test_blocking_read b1 lr /tmp/test.nml 30 8 ) &

echo "Testing remote blocking read . . . "
${builddir}/nml_test_blocking_read b1 lr /tmp/test.nml 30 8 &
${builddir}/nml_test_blocking_read b1 rr /tmp/test.nml 30 8 &
${builddir}/nml_test_blocking_read b1 rr /tmp/test.nml 30 8 &
sleep 1
${builddir}/nml_test_blocking_read b1 lr /tmp/test.nml 30 8 &
date
echo '$?=' $?
${builddir}/nml_test_blocking_read b1 rr /tmp/test.nml -1 8
echo '$?=' $?
if test $? -ne 0 ; then 
    date
    echo "Remote blocking read test failed."
    echo "Kill any nml test programs still running."
    procstokill=`ps -ae | grep nml_te | awk '{print $1}'`
    if test  x != "x${procstokill}" ; then
	kill -INT $procstokill
	sleep 1
	procstokill=`ps -ae | grep nml_te | awk '{print $1}'`
	if test x != "x${procstokill}" ; then
	    echo ."$procstokill".
	    kill -KILL $procstokill 
	    sleep 1
	fi
    fi
    exit 255
fi
date
echo "remote read blocking test passed."


echo "Kill any nml test programs still running."
procstokill=`ps -ae | grep nml_te | awk '{print $1}'`
if test  x != "x${procstokill}" ; then
    kill -INT $procstokill
    sleep 1
    procstokill=`ps -ae | grep nml_te | awk '{print $1}'`
    if test x != "x${procstokill}" ; then
	echo ."$procstokill".
	kill -KILL $procstokill 
	sleep 1
    fi
fi

${builddir}/nmlclean /tmp/test.nml
exit 0
