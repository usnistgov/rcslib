#!/bin/bash

if test "x${DEBUG}" != "x" ; then
    set -x;
fi

#pwd
#echo $0 $*

if test $# -lt 3 ; then
    echo "Usage: program_to_kill times_to_wait wait_delay";
    exit 1;
fi

target=${1}

if test "x${4}" != "x" ; then
    if test -f "${4}" ; then
	pid=`grep "pid=" "${4}" | grep -v "ppid=" | head -n 1 | awk '{print $2}'`;
    fi
fi

if test "x${pid}" != "x" ; then

    if  ps -p "${pid}"  >/dev/null ; then
	true;
    else
	if test "x${DEBUG}" = "x1" ; then
	    echo "kill_with_timeout target ${target} not running." 
	fi
	exit 0;
    fi
    kill -INT "${pid}"
    
else

    if  ps -eo cmd | awk '{printf(" %s %s %s \n",$1,$2,$3);}' | grep -v grep | grep -v ssh  | grep -v kill | grep -v stop | grep -iv screen | grep -v start | grep -v run | grep "${target} "  >/dev/null ; then
	true;
    else
	if test "x${DEBUG}" = "x1" ; then
	    echo "kill_with_timeout target ${target} not running." 
	fi
	exit 0;
    fi
    killall -INT "${target}"
fi


max_tries=$2;
delay=$3;

echo "Killing ${target} with -INT  and then waiting ${max_tries} delays of ${delay} second before killing with -KILL".
#echo "Killing ${target} with -INT and then waiting ${max_tries} delays of ${delay} second before killing with -KILL". >&2


tries=0
if test "x${pid}" != "x" ; then

    while test ${tries} -lt ${max_tries} && ps -p "${pid}" >&2 ; do
	let tries++;
	ps -l "${pid}"
	echo  " --  waiting for $1 to die  :  ${tries} of ${max_tries} "
	sleep ${delay};
    done

else

    while test ${tries} -lt ${max_tries} && ps -eo cmd | awk '{printf(" %s %s %s \n",$1,$2,$3);}'  | grep -v grep | grep -v emacs | grep -v ssh  | grep -v kill | grep -v stop | grep -iv screen | grep -v start | grep -v run | grep "${target} " >&2 ; do
	let tries++;
	echo  " --  waiting for $1 to die  : ${tries} of ${max_tries} "
	sleep ${delay};
    done
fi

echo "";

if test "x${pid}" != "x" ; then

    if  test ${tries} -ge ${max_tries} && ps -p "${pid}"  >&2 ; then

	if test -f $1.running; then
	    ./attach_gdb.sh $1.running;
	fi

	echo "${target} would not die after killall -INT ${target} and waiting ${tries} . resorting to killall -KILL ${target}";
	kill -KILL ${pid}
    fi

else 
    if  test ${tries} -ge ${max_tries} && ps -eo cmd | awk '{printf(" %s %s %s \n",$1,$2,$3);}' | grep -v grep | grep -v emacs | grep -v ssh  | grep -v kill | grep -v stop | grep -iv screen | grep -v start | grep -v run | grep "${target} "  >&2 ; then
	echo "${target} would not die after killall -INT ${target} and waiting ${tries} . resorting to killall -KILL ${target}";
	killall -w -KILL ${target}
    fi
fi

echo "";
