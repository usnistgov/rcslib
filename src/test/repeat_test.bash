#!/bin/bash

set -x ;

svr_pid=`ps -ae | grep nml | awk '{print $1}'`;

export LD_LIBRARY_PATH=${HOME}/rcslib/lib/;


for i in `seq 1 100` ; do 
    echo "";
    echo $i; 
    ps -lp ${svr_pid}; 
    cat /proc/${svr_pid}/status; 
    cat /proc/${svr_pid}/statm; 
    ./nml_test_write b1 rw dltest.nml $(( $i+100 )); 	
    ./nml_test_read b1 rr dltest.nml $(( $i+100 )); 
    echo ""; 
done;
