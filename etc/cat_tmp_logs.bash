#!/bin/bash

for ldir in /tmp/tni_* ; do
    echo ""
    echo "Start Log Directory: ${ldir}"
    echo ""
    ls -l "${ldir}"
    for log in "/tmp/${ldir}/*.log" ; do
        echo ""
        echo "Start Log: ${log}"
        echo ""
        cat "${log}"
        echo ""
        echo "End Log: ${log}"
        echo ""
    done;
    echo ""
    echo "End Log Directory: ${ldir}"
    echo ""
done;
