#!/bin/sh

set -x;
RCS_DIR=../..;

LD_LIBRARY_PATH="${RCS_DIR}/lib:${LD_LIBRARY_PATH}";
export LD_LIBRARY_PATH;

killall -INT nmlcfgsvr;

${RCS_DIR}/bin/nmlcfgsvr --debug --minchecktime 1.0 &
sleep 1;
DO_NML_START=1
export DO_NML_START;

 ./nml_test_write b0 ntw "nmlcfgsvr::::create=checkcreate:options=port=5001 willstart=1" 99 30 &
sleep 5;
./nml_test_read b0 ntr "nmlcfgsvr::::create=checkcreate:options=port=5001 willstart=1" 99 3 &
