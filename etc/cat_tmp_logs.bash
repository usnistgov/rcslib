#!/bin/bash

#set -x;
ldir=`pwd`;

echo ""
echo "Start Log Directory: ${ldir}"
echo ""
ls -l "${ldir}"
for log in "${ldir}/"*.log ; do
    if test "x${log}" != "x" -a -f "${log}" ; then
        echo ""
        echo "Start Log: ${log}"
        echo ""
        cat "${log}"
        echo ""
        echo "End Log: ${log}"
        echo ""
    fi
 done;
echo ""
echo "End Log Directory: ${ldir}"
echo ""

for ldir in /tmp/tni_* ; do
    echo ""
    echo "Start Log Directory: ${ldir}"
    echo ""
    ls -l "${ldir}"
    for log in "${ldir}/"*.log ; do
        if test "x${log}" != "x" -a -f "${log}" ; then
            echo ""
            echo "Start Log: ${log}"
            echo ""
            cat "${log}"
            echo ""
            echo "End Log: ${log}"
            echo ""
        fi
    done;
    echo ""
    echo "End Log Directory: ${ldir}"
    echo ""
done;
