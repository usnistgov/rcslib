#!/bin/sh

if [ -f ~/.debug_scripts ] ; then
    set -x
    echo '$#='$#
    echo '$0='$0
    echo '$1='$1
    echo '$2='$2
fi

if [ $# -gt 0 ] ; then
    PLAT=$1
fi

if [ $# -gt 1 ] ; then
    RCSLIB_MAIN_DIR=$2
fi

if [ -d ${RCSLIB_MAIN_DIR}  -a '!' -e ${RCSLIB_MAIN_DIR}/plat ] ; then
    mkdir ${RCSLIB_MAIN_DIR}/plat;
fi

cd ${RCSLIB_MAIN_DIR}/plat ; 

	
if [ ! -h ${PLAT} ] ; then  
    echo ${PLAT} ;
else
    ls -l ${PLAT} | sed 's#^l.*-> ##' ;
fi 
