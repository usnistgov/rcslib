#!/bin/sh

if test "x${DEBUG}" = "x1" ; then
    set -x;
fi

if test "x${RCAW_PID_LIST_FILE}" != "x" ; then
    echo -n " $$ " >> "${RCAW_PID_LIST_FILE}";
fi

LD_LIBRARY_PATH="${RCAW_LL_PATH}:${LD_LIBRARY_PATH}"

#echo -ne "\033]0; $*  \007"
if test "x${RCSLIB_DIR}" != "x" -a -d "${RCSLIB_DIR}/lib/" ; then
    if ! echo "${LD_LIBRARY_PATH}" | grep "${RCSLIB_DIR}/lib/" >/dev/null 2>/dev/null ; then
	LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:"${RCSLIB_DIR}/lib/";
    fi
fi

echo "LD_LIBRARY_PATH=${LD_LIBRARY_PATH}";
export LD_LIBRARY_PATH;

echo "Running:  \"$*\" from " `pwd` " . . ."
if test "x${CREATE_RCAW_LOG}" != "x" ; then
    h=`hostname`;
    n=${1##*/}
    if test "x${RCAW_LOG_DIR}" != "x" ; then
	d=`(cd ${RCAW_LOG_DIR}; pwd)`;
	nl=${d}/rcaw_${n}_${h%%.*}_$$.log;
	\rm -f ${d}/rcaw_${n}_${h%%.*}_[0-9]*[0-9].log
    else
	nl=rcaw_${n}_${h%%.*}_$$.log;
	\rm -f rcaw_${n}_${h%%.*}_[0-9]*[0-9].log
    fi

    (set -x ; pwd ; $*  2>&1 ) | tee ${nl}
else
    $*;
fi


echo "Finished:  $* . . ."
echo "Press enter to finish."
read f
