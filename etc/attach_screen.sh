#!/bin/bash

if test "x${DEBUG}" != "x" ; then
    set -x;
fi

#pwd
#echo $0 $*

if test $# -lt 1; then
    echo "Please specify .running file to attach to";
    exit 1;
fi

if test "x${1}" != "x" ; then
    if test -f "${1}" -a '!' -h "${1}" -a "x${1##*.}" = "running"  ; then
	pid=`grep "pid=" "${1}" | awk '{print $2}'`;
	ppid=`grep "ppid=" "${1}" | awk '{print $2}'`;
    fi
fi


scount=`screen -ls | grep Detach | grep "${1}" | wc -l`
if test "x${scount}" = "x1" ; then 
    if test '!' -f "$1" -o "x${1##*.}" != "running" ; then
	screen -r -d `screen -ls | grep Detach | grep "${1}" | awk '{print $1}'` && exit 0;
    fi
fi

uid=`id -u`;
su_scount=0;

if test "x${scount}" != "x1" -a "x${uid}" = "x0" ; then
    su_scount=`sudo screen -ls | grep tach | grep "${1}" | wc -l`;
    if test "x${scount}" = "x1" ; then 
	if test '!' -f "$1" -o "x${1##*.}" != "running" ; then
	    sudo screen -r -d `sudo screen -ls | grep Detach | grep "${1}" | awk '{print $1}'` && exit 0;
	fi
    fi
fi


if test "x${pid}" = "x" ; then
    echo "No pid found in .running file"
    b=${1:0:13}
    c=${b##*/}
    d=${c%%.*}
    pid=`ps -eo pid,cmd | grep "${d}" | grep -iv "screen" | head -n 1 |  awk '{print $1}'`; 
fi

if test "x${pid}" = "x" ; then
    echo "Can't find pid with ps."
    e=${1:0:13}
    f=${e##*/}
    g=${f%%.*}
    pppid=`screen -ls | grep ${g} | awk '{print $1}' | head -n 1`;
else
    if test "x${ppid}" = "x" ; then
	ppid=`ps -o ppid ${pid} | tail -n 1 | awk '{print $1}'`;
    fi

    pppid=`ps -o ppid ${ppid} | tail -n 1 | awk '{print $1}'`;
fi

uid_p=`ps -o uid ${pid} | tail -n 1 | awk '{print $1}'`
uid_n=`ps -o uid $$ | tail -n 1 | awk '{print $1}'`

pid_to_attach=${pid};


if test "x${uid_p}" = "x0" -a "x${uid_n}" != "x0" ; then
    if sudo screen -ls | grep tach | awk '{print $1}' | grep "${pid}." > /dev/null; then
	pid_to_attach=${pid};
    elif sudo screen -ls | grep tach | awk '{print $1}' | grep "${ppid}." > /dev/null; then
	pid_to_attach=${ppid};
    else
	pid_to_attach=${pppid};
    fi
    sudo screen -r ${pid_to_attach};
else
    if  screen -ls | grep tach | awk '{print $1}' | grep "${pid}." > /dev/null; then
	pid_to_attach=${pid};
    elif  screen -ls | grep tach | awk '{print $1}' | grep "${ppid}." > /dev/null; then
	pid_to_attach=${ppid};
    else
	pid_to_attach=${pppid};
    fi
    screen -r ${pid_to_attach};
fi

