#! /bin/sh

set -x

sleep 2

date
set
pwd
ls -l
rm *.log

if test "x${tmpdir}" = "x" ; then
    tmpdir="${HOME}/.tmp/$$";
    export tmpdir;
fi

#DEBUG_RCSLIB=1
#export DEBUG_RCSLIB
#RCS_PRINT_DO_FLUSH=1
#export RCS_PRINT_DO_FLUSH
#NML_TEST_QR_VERBOSE=1
#export NML_TEST_QR_VERBOSE

if test "x${RCSLIB_DIR}" = "x" ; then
    RCSLIB_DIR=../..
fi

if test "x${JDK_DIR}" != "x" ; then
    JDK_DIR=`(cd ${JDK_DIR} ; pwd )`;
    export JDK_DIR
fi

if test "x${JDK_DIR}" = "x" -a -x "${RCSLIB_DIR}/etc/jdk_dir" ; then
    JDK_DIR=`${RCSLIB_DIR}/etc/jdk_dir`;
fi

if test "x${JAVA}" = "x" ; then
    if test "x${JDK_DIR}" != "x" -a -x ${JDK_DIR}/bin/java ; then
	JAVA=${JDK_DIR}/bin/java ;
    elif test -d /usr/java/ ; then
	JAVA=`find /usr/java/ -name java -type f -follow | head -n 1`
    else
	JAVA=java
    fi
fi

if test "x${JAVAC}" = "x" ; then
    if test "x${JDK_DIR}" != "x" -a -x ${JDK_DIR}/bin/javac ; then
	JAVA=${JDK_DIR}/bin/java ;
    elif test -d /usr/java ; then
	JAVAC=`find /usr/java/ -name javac -type f -follow | head -n 1`
    else
	JAVAC=javac
    fi
fi

export JAVA
export JAVAC

echo RCSLIB_DIR=${RCSLIB_DIR}

if test "x${RCSLIB_BIN_DIR}" = "x" ; then
    if test "x${PLAT}" != "x" -a -d "${RCSLIB_DIR}/plat/${PLAT}/bin" ; then
	RCSLIB_BIN_DIR=${RCSLIB_DIR}/plat/${PLAT}/bin
    elif test -d "${RCSLIB_DIR}/bin" ; then
	RCSLIB_BIN_DIR=${RCSLIB_DIR}/bin
    elif test  -d bin ; then
	RCSLIB_BIN_DIR=${PWD}/bin
    else
	RCSLIB_BIN_DIR=${PWD}
    fi
fi

if test "x${RCSLIB_LIB_DIR}" = "x" ; then
    if test "x${PLAT}" != "x" -a  -d "${RCSLIB_DIR}/plat/${PLAT}/lib" ; then
	RCSLIB_LIB_DIR=${RCSLIB_DIR}/plat/${PLAT}/lib
    elif test  -d "${RCSLIB_DIR}/lib" ; then
	RCSLIB_LIB_DIR=${RCSLIB_DIR}/lib
    elif test -d lib; then
	RCSLIB_LIB_DIR=${PWD}/lib
    else
	RCSLIB_LIB_DIR=${PWD}
    fi
fi

export RCSLIB_LIB_DIR
export RCSLIB_BIN_DIR

LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${RCSLIB_LIB_DIR}
export LD_LIBRARY_PATH

NMLCFG_PROG=${RCSLIB_BIN_DIR}/nmlcfg

for i in extra_te*.nml endoflist ; do
    if test "x${i}" != "x" -a "x${i}" != "xextra_te\*.nml" -a "x${i}" != "xendoflist"; then
	${RCSLIB_BIN_DIR}/nmlclean ${i}
	\rm -f ${i}
    fi
done

LAUNCHER_TO_KILL=
if test "x${PROGRAM_LAUNCHER}" != "x" ; then
    LAUNCHER_TO_KILL=`echo ${PROGRAM_LAUNCHER} | sed 's/ --//'`
fi

if test "x${KILL_LAUNCHER}" = "x" ; then
    KILL_LAUNCHER=true;
fi

echo RCSLIB_BIN_DIR=${RCSLIB_BIN_DIR}

(rm ${tmpdir}/extra_tests_killnt.sh ) >/dev/null 2>/dev/null 

cat >${tmpdir}/extra_tests_killnt.sh <<EOF
#!/bin/sh
    date
    set -x
    sleep 1
    ps -ae
    ps
    echo "Kill any nml test programs still running."
    ps | grep nml_tes | grep -v grep 
    procstokill=\`ps | grep nml_tes | grep -v grep  | awk '{printf(" %s",\$1);}'\`
    while test "x\${procstokill}" != "x" ; do
	sleep 1
        ps | grep nml_tes | grep -v grep 

	if test x != "x\${procstokill}" ; then
	    kill -INT \$procstokill
	    sleep 2
	    procstokill=\`ps | grep nml_tes | grep -v grep  | awk '{printf(" %s",\$1);}'\`
	fi

	if test x != "x\${procstokill}" ; then
	    kill -INT \$procstokill
	    sleep 2
	    procstokill=\`ps | grep nml_tes | grep -v grep  | awk '{printf(" %s",\$1);}'\`
	fi

	if test x != "x\${procstokill}" ; then
	    kill -TERM \$procstokill
	    sleep 2
	    procstokill=\`ps | grep nml_tes | grep -v grep  | awk '{printf(" %s",\$1);}'\`
	fi

	if test x != "x\${procstokill}" ; then
	    kill -TERM \$procstokill
	    sleep 2
	    procstokill=\`ps | grep nml_tes | grep -v grep  | awk '{printf(" %s",\$1);}'\`
	fi

	if test x != "x\${procstokill}" ; then
	    kill -KILL \$procstokill
	    sleep 2
	    procstokill=\`ps | grep nml_tes | grep -v grep  | awk '{printf(" %s",\$1);}'\`
	fi

	if test x != "x\${procstokill}" ; then
	    kill -KILL \$procstokill
	    sleep 2
	    procstokill=\`ps | grep nml_tes | grep -v grep  | awk '{printf(" %s",\$1);}'\`
	fi
    done
    sleep 1
    if test "x${LAUNCHER_TO_KILL}" != "x" ; then 
    ps -ae
    ps
    ps | grep ${LAUNCHER_TO_KILL} | grep -v grep 
    procstokill=\`ps | grep ${LAUNCHER_TO_KILL} | grep -v grep  | awk '{printf(" %s",\$1);}'\`
    while test "x\${procstokill}" != "x" ; do
	sleep 1
        ps | grep nml_tes | grep -v grep 

	if test x != "x\${procstokill}" ; then
	    kill -INT \$procstokill
	    sleep 2
	    procstokill=\`ps | grep nml_tes | grep -v grep  | awk '{printf(" %s",\$1);}'\`
	fi

	if test x != "x\${procstokill}" ; then
	    kill -INT \$procstokill
	    sleep 2
	    procstokill=\`ps | grep nml_tes | grep -v grep  | awk '{printf(" %s",\$1);}'\`
	fi

	if test x != "x\${procstokill}" ; then
	    kill -TERM \$procstokill
	    sleep 2
	    procstokill=\`ps | grep nml_tes | grep -v grep  | awk '{printf(" %s",\$1);}'\`
	fi

	if test x != "x\${procstokill}" ; then
	    kill -TERM \$procstokill
	    sleep 2
	    procstokill=\`ps | grep nml_tes | grep -v grep  | awk '{printf(" %s",\$1);}'\`
	fi

	if test x != "x\${procstokill}" ; then
	    kill -KILL \$procstokill
	    sleep 2
	    procstokill=\`ps | grep nml_tes | grep -v grep  | awk '{printf(" %s",\$1);}'\`
	fi

	if test x != "x\${procstokill}" ; then
	    kill -KILL \$procstokill
	    sleep 2
	    procstokill=\`ps | grep nml_tes | grep -v grep  | awk '{printf(" %s",\$1);}'\`
	fi
    done
    fi
    sleep 2

    ipcrm -M 101
    ipcrm -M 102
    ipcrm -M 103
    ipcrm -M 201
    ipcrm -M 202
    ipcrm -M 203
    ipcrm -M 301
    ipcrm -M 302
    ipcrm -M 303

    ipcrm -S 101
    ipcrm -S 102
    ipcrm -S 103
    ipcrm -S 201
    ipcrm -S 202
    ipcrm -S 203
    ipcrm -S 301
    ipcrm -S 302
    ipcrm -S 303

    if test -d /dev/shm ; then
       \rm -f /dev/shm/*.shm ;
       \rm -f /dev/shm/*.sem ;
    else 

    if test -x ${RCSLIB_BIN_DIR}/posix_shm_unlink ; then
	  ${RCSLIB_BIN_DIR}/posix_shm_unlink /_101.shm
	  ${RCSLIB_BIN_DIR}/posix_shm_unlink /_201.shm
	  ${RCSLIB_BIN_DIR}/posix_shm_unlink /_102.shm
	  ${RCSLIB_BIN_DIR}/posix_shm_unlink /_202.shm
	  ${RCSLIB_BIN_DIR}/posix_shm_unlink /_301.shm
	  ${RCSLIB_BIN_DIR}/posix_shm_unlink /_302.shm
    fi

    if test -x ${RCSLIB_BIN_DIR}/posix_sem_unlink ; then
	${RCSLIB_BIN_DIR}/posix_sem_unlink /_101.sem
	${RCSLIB_BIN_DIR}/posix_sem_unlink /_201.sem
	${RCSLIB_BIN_DIR}/posix_sem_unlink /_102.sem
	${RCSLIB_BIN_DIR}/posix_sem_unlink /_202.sem
	${RCSLIB_BIN_DIR}/posix_sem_unlink /_301.sem
	${RCSLIB_BIN_DIR}/posix_sem_unlink /_302.sem
    fi

    if test -x \${RCSLIB_BIN_DIR}/posix_shm_unlink ; then
	\${RCSLIB_BIN_DIR}/posix_shm_unlink /_101.shm
	\${RCSLIB_BIN_DIR}/posix_shm_unlink /_201.shm
	\${RCSLIB_BIN_DIR}/posix_shm_unlink /_102.shm
	\${RCSLIB_BIN_DIR}/posix_shm_unlink /_202.shm
	\${RCSLIB_BIN_DIR}/posix_shm_unlink /_301.shm
	\${RCSLIB_BIN_DIR}/posix_shm_unlink /_302.shm
    fi

    if test -x \${RCSLIB_BIN_DIR}/posix_sem_unlink ; then
	\${RCSLIB_BIN_DIR}/posix_sem_unlink /_101.sem
	\${RCSLIB_BIN_DIR}/posix_sem_unlink /_201.sem
	\${RCSLIB_BIN_DIR}/posix_sem_unlink /_102.sem
	\${RCSLIB_BIN_DIR}/posix_sem_unlink /_202.sem
	\${RCSLIB_BIN_DIR}/posix_sem_unlink /_301.sem
	\${RCSLIB_BIN_DIR}/posix_sem_unlink /_302.sem
    fi
   fi

    sleep 1

    PAUSE_ON_RCS_ERROR=
    unset PAUSE_ON_RCS_ERROR

    if test -x ${RCSLIB_BIN_DIR}/nmlclean ; then
	for nmlfile in *.nml endlist ; do
	    echo nmlfile=\${nmlfile}
	    if test "x\${nmlfile}" != "x" -a "x\${nmlfile}" != 'x*.nml' -a "x\${nmlfile}" != "xendlist" ; then
               ${RCSLIB_BIN_DIR}/nmlclean \${nmlfile} ;
	    fi
        done
     fi
 
     if test -x \${RCSLIB_BIN_DIR}/nmlclean ; then
	for nmlfile in *.nml endlist ; do
	    echo nmlfile=\${nmlfile}
	    if test "x\${nmlfile}" != "x" -a "x\${nmlfile}" != 'x*.nml' -a "x\${nmlfile}" != "xendlist" ; then
               \${RCSLIB_BIN_DIR}/nmlclean \${nmlfile} ;
	    fi
        done
    fi
    if test "x\${KILL_LAUNCHER}" != "x" ; then
       \${KILL_LAUNCHER}; 
    fi
    ( ${KILL_LAUNCHER} )

    ipcs 
    ipcrm  `ipcs -s | gawk '{if(\$1 != "0x00000000" && \$1 != "") printf("-s %s\n",\$2);}' | grep -vi sh | grep -vi si | grep -vi se`
ipcrm  `ipcs -m | gawk '{if(\$1 != "0x00000000" && \$1 != "") printf("-m %s\n",\$2);}' | grep -vi sh | grep -vi si | grep -vi se`

EOF

if test "x${KILL_LAUNCHER}" = "xtrue" ; then
    KILL_LAUNCHER=
fi

chmod a+x ${tmpdir}/extra_tests_killnt.sh

ls -l ${tmpdir}/extra_tests_killnt.sh

cat ${tmpdir}/extra_tests_killnt.sh

echo " ( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_1.log "
( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_1.log


if test "x${NMLCFG_FILE1}" = "x" ; then
    NMLCFG_FILE1=${CONFIG_NML}
fi

if test "x${NMLCFG_FILE1}" = "x" ; then
    NMLCFG_FILE1=dltest.nml
fi

echo PROGRAM_LAUNCHER="${PROGRAM_LAUNCHER}"
echo EXEEXT="${EXEEXT}"
echo KILL_LAUNCHER="${KILL_LAUNCHER}"
echo NMLCFG_FILE1=${NMLCFG_FILE1}

if test "x${EXTRA_TEST_BLIST}" = "x" ; then
#    EXTRA_TEST_BLIST="bB bC b1 b2 b3 b4 b5 b6 b7 b8 b9 bA"
    EXTRA_TEST_BLIST="b1 b2 b6 b7"
    uname_m=`uname -m`
    if test "x${uname_m}" = "xx86_64" ; then 
	EXTRA_TEST_BLIST="b2 b7";
    fi
fi

for buftotest in ${EXTRA_TEST_BLIST} ; do 

echo buftotest=${buftotest}

date

(ipcs || true)
( netstat -na || true)
( netstat -napee || true)

env NML_SET_TO_SERVER=1 ${PROGRAM_LAUNCHER} ./nml_test_server${EXEEXT} ${buftotest} b1s ${NMLCFG_FILE1} &

sleep 2

ps -ae
ps
(ipcs || true)
( netstat -na || true)
( netstat -napee || true)

rm eightyeight;
sync;

cwd=`pwd`;
date
cmd="${PROGRAM_LAUNCHER} ${cwd}/nml_test_dl_write${EXEEXT} ${buftotest} rw ${NMLCFG_FILE1} 89"
${cmd}
sts=$?
if test "x${sts}" != "x0" ; then
    echo "${cmd} failed.sts=${sts}"
    echo " ( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_2.${buftotest}.log "
    ( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_2.${buftotest}.log
    sleep 1
    echo "${cmd} failed.sts=${sts}"
    exit ${sts}
fi

sleep 1

echo "${cmd} OK"
echo

if test "x${NO_GET_TYPE}" = "x" ; then 
    date
    type_cmd="${PROGRAM_LAUNCHER} ${cwd}/nml_get_type${EXEEXT} ${buftotest} rr ${NMLCFG_FILE1}";
    msg_type=`${type_cmd}`;
    sts=$?
    if test "x${msg_type}" != "x101" ; then
	echo "${type_cmd} failed.msg_type=${msg_type} sts=${sts}"
	echo " ( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_3.${buftotest}.log "
	( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_3.${buftotest}.log
	sleep 1
	echo "${type_cmd} failed.sts=${sts}"
	exit ${sts}
    fi

    date
    type_cmd="${PROGRAM_LAUNCHER} ${cwd}/nml_get_type${EXEEXT} ${buftotest} lr ${NMLCFG_FILE1}";
    msg_type=`${type_cmd}`;
    sts=$?
    if test "x${msg_type}" != "x101" ; then
	echo "${type_cmd} failed.msg_type=${msg_type} sts=${sts}"
	echo " ( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_3.${buftotest}.log "
	( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_3.${buftotest}.log
	sleep 1
	echo "${type_cmd} failed.sts=${sts}"
	exit ${sts}
    fi

    type_cmd="${PROGRAM_LAUNCHER} ${JAVA}  nml_test_java_get_msg_type ${buftotest} rr ${NMLCFG_FILE1} 101";
    msg_type=`${type_cmd}`;
    sts=$?
    if test "x${msg_type}" != "x101" ; then
	echo "${type_cmd} failed.msg_type=${msg_type} sts=${sts}"
	echo " ( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_3.${buftotest}.log "
	( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_3.${buftotest}.log
	sleep 1
	echo "${type_cmd} failed.sts=${sts}"
	exit ${sts}
    fi
fi


cmd="${PROGRAM_LAUNCHER} ${cwd}/nml_test_read${EXEEXT} ${buftotest} lr ${NMLCFG_FILE1} 89"
${cmd}
sts=$?
if test "x${sts}" != "x0" ; then
    echo "${cmd} failed.sts=${sts}"
    echo " ( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_3.${buftotest}.log "
    ( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_3.${buftotest}.log
    sleep 1
    echo "${cmd} failed.sts=${sts}"
    exit ${sts}
fi

echo "${cmd} OK"
echo

if test "x${NO_GET_TYPE}" = "x" ; then
    type_cmd="${PROGRAM_LAUNCHER} ${cwd}/nml_get_type${EXEEXT} ${buftotest} rr ${NMLCFG_FILE1}";
    msg_type=`${type_cmd}`;
    sts=$?
    if test "x${msg_type}" != "x101" ; then
	echo "${type_cmd} failed.msg_type=${msg_type} sts=${sts}"
	echo " ( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_3.${buftotest}.log "
	( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_3.${buftotest}.log
	sleep 1
	echo "${type_cmd} failed.sts=${sts}"
	exit ${sts}
    fi

    type_cmd="${PROGRAM_LAUNCHER} ${JAVA}  nml_test_java_get_msg_type ${buftotest} rr ${NMLCFG_FILE1} 101";
    msg_type=`${type_cmd}`;
    sts=$?
    if test "x${msg_type}" != "x101" ; then
	echo "${type_cmd} failed.msg_type=${msg_type} sts=${sts}"
	echo " ( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_3.${buftotest}.log "
	( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_3.${buftotest}.log
	sleep 1
	echo "${type_cmd} failed.sts=${sts}"
	exit ${sts}
    fi
fi


cmd="${PROGRAM_LAUNCHER} ${cwd}/nml_test_dl_read${EXEEXT} ${buftotest} rr ${NMLCFG_FILE1} 89"
${cmd}
sts=$?
if test "x${sts}" != "x0" ; then
    echo "${cmd} failed.sts=${sts}"
    echo " ( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_3.${buftotest}.log "
    ( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_3.${buftotest}.log
    sleep 1
    echo "${cmd} failed.sts=${sts}"
    exit ${sts}
fi

echo "${cmd} OK"
echo

date
cmd="${PROGRAM_LAUNCHER} ${cwd}/nml_test_dl_write${EXEEXT} ${buftotest} lw ${NMLCFG_FILE1} 88"
${cmd}
sts=$?
if test "x${sts}" != "x0" ; then
    echo "${cmd} failed.sts=${sts}"
    echo " ( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_2.${buftotest}.log "
    ( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_2.${buftotest}.log
    sleep 1
    echo "${cmd} failed.sts=${sts}"
    exit ${sts}
fi

touch eightyeight;
sync;

sleep 1

echo "${cmd} OK"
echo

date
cmd="${PROGRAM_LAUNCHER} ${cwd}/nml_test_read${EXEEXT} ${buftotest} lr ${NMLCFG_FILE1} 88"
${cmd}

date
cmd="${PROGRAM_LAUNCHER} ${cwd}/nml_test_dl_read${EXEEXT} ${buftotest} rr ${NMLCFG_FILE1} 88"
${cmd}
sts=$?
if test "x${sts}" != "x0" ; then
    echo "${cmd} failed.sts=${sts}"
    echo " ( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_3.${buftotest}.log "
    ( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_3.${buftotest}.log
    sleep 1
    echo "${cmd} failed.sts=${sts}"
    exit ${sts}
fi

echo "${cmd} OK"
echo

date
if test -f "${cwd}/nml_test_read_c${EXEEXT}" ; then
    cmd="${PROGRAM_LAUNCHER} ${cwd}/nml_test_read_c${EXEEXT} ${buftotest} rr ${NMLCFG_FILE1} 88"
    ${cmd}
    sts=$?
    if test "x${sts}" != "x0" ; then
	echo "${cmd} failed.sts=${sts}"
	echo " ( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_3.${buftotest}.log "
	( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_3.${buftotest}.log
	sleep 1
	echo "${cmd} failed.sts=${sts}"
	exit ${sts}
    fi
    
    echo "${cmd} OK"
    echo
fi

date
if test -f "${cwd}/nml_test_dl_read_ada${EXEEXT}" ; then
    cmd="${PROGRAM_LAUNCHER} ${cwd}/nml_test_dl_read_ada${EXEEXT} ${buftotest} rr ${NMLCFG_FILE1} 88"
    ${cmd}
    sts=$?
    if test "x${sts}" != "x0" ; then
	echo "${cmd} failed.sts=${sts}"
	echo " ( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_3.${buftotest}.log "
	( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_3.${buftotest}.log
	sleep 1
	echo "${cmd} failed.sts=${sts}"
	exit ${sts}
    fi
    
    echo "${cmd} OK"
    echo
fi

if test -f "${cwd}/nml_test_write_c${EXEEXT}" ; then
    date
    cmd="${PROGRAM_LAUNCHER} ${cwd}/nml_test_write_c${EXEEXT} ${buftotest} rw ${NMLCFG_FILE1} 88"
    ${cmd}
    sts=$?
    if test "x${sts}" != "x0" ; then
	echo "${cmd} failed.sts=${sts}"
	echo " ( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_3.${buftotest}.log "
	( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_3.${buftotest}.log
	sleep 1
	echo "${cmd} failed.sts=${sts}"
	exit ${sts}
    fi

    echo "${cmd} OK"
    echo

fi

date
if test -f "${cwd}/nml_test_read_c${EXEEXT}" ; then
    cmd="${PROGRAM_LAUNCHER} ${cwd}/nml_test_read_c${EXEEXT} ${buftotest} rr ${NMLCFG_FILE1} 88"
    ${cmd}
    sts=$?
    if test "x${sts}" != "x0" ; then
	echo "${cmd} failed.sts=${sts}"
	echo " ( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_3.${buftotest}.log "
	( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_3.${buftotest}.log
	sleep 1
	echo "${cmd} failed.sts=${sts}"
	exit ${sts}
    fi
    
    echo "${cmd} OK"
    echo
fi

date
if test -f "${cwd}/nml_test_dl_read_ada${EXEEXT}" ; then
    cmd="${PROGRAM_LAUNCHER} ${cwd}/nml_test_dl_read_ada${EXEEXT} ${buftotest} rr ${NMLCFG_FILE1} 88"
    ${cmd}
    sts=$?
    if test "x${sts}" != "x0" ; then
	echo "${cmd} failed.sts=${sts}"
	echo " ( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_3.${buftotest}.log "
	( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_3.${buftotest}.log
	sleep 1
	echo "${cmd} failed.sts=${sts}"
	exit ${sts}
    fi
    
    echo "${cmd} OK"
    echo
fi

date
cmd="${PROGRAM_LAUNCHER} ${cwd}/nml_test_dl_read${EXEEXT} ${buftotest} rr ${NMLCFG_FILE1} 88"
${cmd}
sts=$?
if test "x${sts}" != "x0" ; then
    echo "${cmd} failed.sts=${sts}"
    echo " ( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_3.${buftotest}.log "
    ( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_3.${buftotest}.log
    sleep 1
    echo "${cmd} failed.sts=${sts}"
    exit ${sts}
fi

echo "${cmd} OK"
echo


if test -f "${cwd}/nml_test_dl_write_ada${EXEEXT}" ; then
    date
    cmd="${PROGRAM_LAUNCHER} ${cwd}/nml_test_dl_write_ada${EXEEXT} ${buftotest} rw ${NMLCFG_FILE1} 87"
    ${cmd}
    sts=$?
    if test "x${sts}" != "x0" ; then
	echo "${cmd} failed.sts=${sts}"
	echo " ( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_3.${buftotest}.log "
	( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_3.${buftotest}.log
	sleep 1
	echo "${cmd} failed.sts=${sts}"
	exit ${sts}
    fi

    echo "${cmd} OK"
    echo

    date
    if test -f "${cwd}/nml_test_read_c${EXEEXT}" ; then
	cmd="${PROGRAM_LAUNCHER} ${cwd}/nml_test_read_c${EXEEXT} ${buftotest} rr ${NMLCFG_FILE1} 87"
	${cmd}
	sts=$?
	if test "x${sts}" != "x0" ; then
	    echo "${cmd} failed.sts=${sts}"
	    echo " ( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_3.${buftotest}.log "
	    ( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_3.${buftotest}.log
	    sleep 1
	    echo "${cmd} failed.sts=${sts}"
	    exit ${sts}
	fi
	
	echo "${cmd} OK"
	echo
    fi
    
    date
    if test -f "${cwd}/nml_test_dl_read_ada${EXEEXT}" ; then
	cmd="${PROGRAM_LAUNCHER} ${cwd}/nml_test_dl_read_ada${EXEEXT} ${buftotest} rr ${NMLCFG_FILE1} 87"
	${cmd}
	sts=$?
	if test "x${sts}" != "x0" ; then
	    echo "${cmd} failed.sts=${sts}"
	    echo " ( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_3.${buftotest}.log "
	    ( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_3.${buftotest}.log
	    sleep 1
	    echo "${cmd} failed.sts=${sts}"
	    exit ${sts}
	fi
	
	echo "${cmd} OK"
	echo
    fi
    
    date
    cmd="${PROGRAM_LAUNCHER} ${cwd}/nml_test_dl_read${EXEEXT} ${buftotest} rr ${NMLCFG_FILE1} 87"
    ${cmd}
    sts=$?
    if test "x${sts}" != "x0" ; then
	echo "${cmd} failed.sts=${sts}"
	echo " ( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_3.${buftotest}.log "
	( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_3.${buftotest}.log
	sleep 1
	echo "${cmd} failed.sts=${sts}"
	exit ${sts}
    fi
    
    echo "${cmd} OK"
    echo
fi


date
cmd="${PROGRAM_LAUNCHER} ${cwd}/nml_test_dl_write${EXEEXT} ${buftotest} rw ${NMLCFG_FILE1} 86"
${cmd}
sts=$?
if test "x${sts}" != "x0" ; then
    echo "${cmd} failed.sts=${sts}"
    echo " ( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_2.${buftotest}.log "
    ( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_2.${buftotest}.log
    sleep 1
    echo "${cmd} failed.sts=${sts}"
    exit ${sts}
fi

sleep 1

echo "${cmd} OK"
echo

if test -f ${RCSLIB_DIR}/plat/java/rcs.jar ; then
    RCS_JAR=${RCSLIB_DIR}/plat/java/rcs.jar;
elif  test -f ${RCSLIB_DIR}/bin/rcs.jar ; then
    RCS_JAR=${RCSLIB_DIR}/bin/rcs.jar
elif test -f ${RCSLIB_DIR}/plat/java/lib/rcs/RCS_VERSION.class ; then
    RCS_JAR=${RCSLIB_DIR}/plat/java/lib;
else
    RCS_JAR=`find ${RCSLIB_DIR} -name rcs.jar | head -n 1`;
fi

cp "${RCS_JAR}" .
#RCS_JAR=${RCS_JAR##*/};
RCS_JAR=rcs.jar;
jar -xf rcs.jar;


date
cmd="${JAVA}  nml_test_java_dl_read ${buftotest} rr ${NMLCFG_FILE1} 86"
${cmd}
sts=$?
if test "x${sts}" != "x0" ; then
    echo "${cmd} failed.sts=${sts}"
    echo " ( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_4.${buftotest}.log "
    ( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_4.${buftotest}.log
    sleep 1
    cat -v ${cwd}/nml_test_java_dl_read.${buftotest}.log
    echo "${cmd} failed.sts=${sts}"
    exit ${sts}
fi

cat -v ${cwd}/nml_test_java_dl_read.${buftotest}.log
echo "${cmd} OK"
echo

date
cmd="${JAVA} nml_test_java_dl_write ${buftotest} rw ${NMLCFG_FILE1} 99"
${cmd}
sts=$?
if test "x${sts}" != "x0" ; then
    echo "${cmd} failed.sts=${sts}"
    echo " ( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_5.${buftotest}.log "
    ( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_5.${buftotest}.log
    sleep 1
    cat -v ${cwd}/nml_test_java_dl_write.${buftotest}.log
    echo "${cmd} failed.sts=${sts}"
    exit ${sts}
fi

cat -v ${cwd}/nml_test_java_dl_write.${buftotest}.log
echo "${cmd} OK"
echo

date

cmd="${JAVA}  nml_test_java_dl_read ${buftotest} rr ${NMLCFG_FILE1} 99"
${cmd}
sts=$?
if test "x${sts}" != "x0" ; then
    echo "${cmd} failed.sts=${sts}"
    echo " ( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_6.${buftotest}.log "
    ( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_6.${buftotest}.log
    sleep 1
    cat -v ${cwd}/nml_test_java_dl_read.${buftotest}.log
    echo "${cmd} failed.sts=${sts}"
    exit ${sts}
fi

cat -v ${cwd}/nml_test_java_dl_read.${buftotest}.log
echo "${cmd} OK"
echo

date
cmd="${PROGRAM_LAUNCHER} ${cwd}/nml_test_dl_read${EXEEXT} ${buftotest} rr ${NMLCFG_FILE1} 99"
${cmd}
sts=$?
if test "x${sts}" != "x0" ; then
    echo "${cmd} failed.sts=${sts}"
    echo " ( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_7.${buftotest}.log "
    ( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_7.${buftotest}.log
    sleep 1
    echo "${cmd} failed.sts=${sts}"
    exit ${sts}
fi

date
cmd="${PROGRAM_LAUNCHER} ${cwd}/nml_test_dl_read${EXEEXT} ${buftotest} rr ${NMLCFG_FILE1} 99"
${cmd}
sts=$?
if test "x${sts}" != "x0" ; then
    echo "${cmd} failed.sts=${sts}"
    echo " ( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_7.${buftotest}.log "
    ( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_7.${buftotest}.log
    sleep 1
    echo "${cmd} failed.sts=${sts}"
    exit ${sts}
fi

date
if test -f "${cwd}/nml_test_read_c${EXEEXT}" ; then
    cmd="${PROGRAM_LAUNCHER} ${cwd}/nml_test_read_c${EXEEXT} ${buftotest} rr ${NMLCFG_FILE1} 99"
    ${cmd}
    sts=$?
    if test "x${sts}" != "x0" ; then
	echo "${cmd} failed.sts=${sts}"
	echo " ( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_3.${buftotest}.log "
	( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_3.${buftotest}.log
	sleep 1
	echo "${cmd} failed.sts=${sts}"
	exit ${sts}
    fi
    
    echo "${cmd} OK"
    echo
fi

date
if test -f "${cwd}/nml_test_dl_read_ada${EXEEXT}" ; then
    cmd="${PROGRAM_LAUNCHER} ${cwd}/nml_test_dl_read_ada${EXEEXT} ${buftotest} rr ${NMLCFG_FILE1} 99"
    ${cmd}
    sts=$?
    if test "x${sts}" != "x0" ; then
	echo "${cmd} failed.sts=${sts}"
	echo " ( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_3.${buftotest}.log "
	( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_3.${buftotest}.log
	sleep 1
	echo "${cmd} failed.sts=${sts}"
	exit ${sts}
    fi
    
    echo "${cmd} OK"
    echo
fi

grep ${buftotest} ${NMLCFG_FILE1} | grep B 

echo " ( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_8.${buftotest}.log "
( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_8.${buftotest}.log

done

if "x${ENABLE_QR}" != "x" ; then 
    echo " ( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_9.log "
    ( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_9.log
${cwd}/test_qr.sh
     
     sts=$?;

     echo " ( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_11.log "
     ( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_11.log

fi

echo "${0} (extra_tests.sh) ($$) sts=${sts}"


exit ${sts}


