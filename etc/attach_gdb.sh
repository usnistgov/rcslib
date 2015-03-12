#!/bin/sh

if test "x${DEBUG}" != "x" ; then
    set -x;
fi

#pwd
#echo $0 $*

if test $# -lt 1 ; then
    echo "Please specify .running file to attach to";
    exit 1;
fi

if test "x${1}" != "x" ; then
    if test -f "${1}.running" ; then
	pid=`grep "pid=" "${1}.running" | awk '{print $2}'`;
	prog=`grep "prog=" "${1}.running" | awk '{print $2}'`;
    elif test "${1%.running}x" != "${1}x" -a -f "${1}" ; then
	pid=`grep "pid=" "${1}" | awk '{print $2}'`;
	prog=`grep "prog=" "${1}" | awk '{print $2}'`;
    fi
fi

echo "pid=${pid}"

if test "x${pid}" = "x" ; then
    echo "No pid found in .running file"
    b=${1:0:13}
    c=${b##*/}
    d=${c%%.*}
    pid=`ps -eo pid,cmd | grep "${d}" | grep -iv "screen" | grep -iv "run_cmd_and_wait" | head -n 1 |  awk '{print $1}'`; 
fi

if test "x${prog}" = "x" ; then
    echo "No prog found in .running file"
    prog=${1%%.*}
fi

uid_p=`ps -o uid ${pid} | tail -n 1 | awk '{print $1}'`
uid_n=`ps -o uid $$ | tail -n 1 | awk '{print $1}'`

if test "x${uid_p}" = "x0" -a "x${uid_n}" != "x0" ; then
    echo "sudo gdb ${prog} ${pid};"
    sudo gdb ${prog} ${pid};
else
    echo "gdb ${prog} ${pid}"
    gdb ${prog} ${pid}
fi


