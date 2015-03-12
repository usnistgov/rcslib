#! /bin/sh

set -x

export DO_NOT_PRINT_CMS_QUEUE_FULL=1;
export DO_NOT_PRINT_NML_QUEUE_FULL=1;

echo "${0}:${LINENO} Starting $0 $* from  ${PWD}"
echo "${0}:${LINENO} Starting $0 $* from  ${PWD}" >&2

if test "x${TESTNML_SLEEP}" = "x" ; then
    export TESTNML_SLEEP=1;
    if uname -a | grep "Darwin" >/dev/null 2>/dev/null ; then 
	# Work around for Macs being incredibly unreliable and slow.
	export TESTNML_SLEEP=10;
    fi
fi

if test "x${LOG_TO_STDOUT}" != "x" ; then
    echo "${0}:${LINENO} Running $0 $* . . .";
    pwd
    set
    printenv;
    echo "${0}:${LINENO} Ready?"
    read ready_confirm;
fi

if test "x${USER}" = "xroot" -a "x${RUN_CHECK_AS_ROOT}" != "xy" ; then
    echo "${0}:${LINENO} This program is unsafe to run as root.\n"
    echo "${0}:${LINENO} This program is unsafe to run as root.\n" >&2
    exit 1
fi

WHOAMI_RET=`whoami >/dev/null 2>/dev/null`;

if test "x${WHOAMI_RET}" = "xroot" -a "x${RUN_CHECK_AS_ROOT}" != "xy" ; then
    echo "${0}:${LINENO} This program is unsafe to run as root.\n"
    echo "${0}:${LINENO} This program is unsafe to run as root.\n" >&2
    exit 1
fi

if test "x${NML_SET_TO_SERVER}" != "x" ; then
    NML_SET_TO_SERVER=
fi

if test "x${server_startup_delay}" = "x" ; then
    server_startup_delay=10;
fi

export_n_out=`(export -n NML_SET_TO_SERVER 2>/dev/null && echo OK) | tail -n 1`

#DEBUG_OUTPUT=STDOUT
#export DEBUG_OUTPUT

if test "x${export_n_out}" = "xOK" ; then
    export -n NML_SET_TO_SERVER >/dev/null 2>/dev/null
fi
unset NML_SET_TO_SERVER >/dev/null 2>/dev/null

unalias ls >/dev/null 2>/dev/null
unalias rm >/dev/null 2>/dev/null
unalias mv >/dev/null 2>/dev/null
unalias cp >/dev/null 2>/dev/null
unalias cat >/dev/null 2>/dev/null
unalias cd >/dev/null 2>/dev/null

if test "x${tmpdir}" != "x" ; then
    temp_dir="${tmpdir}";
elif test "x${temp_dir}" = "x" ; then
    tmpdir="/tmp";
    temp_dir="/tmp";
fi

if test "x${temp_dir}" = "x" ; then
    mkdir /tmp/tni_$$ && touch /tmp/tni_$$/.touch_$$ && temp_dir="/tmp/tni_$$";
fi

if test "x${temp_dir}" = "x" ; then
    mkdir ${HOME}/.tmp && mkdir ${HOME}/.tmp/tni_internal_$$ && touch ${HOME}/.tmp/tni_internal_$$/.touch_$$ && temp_dir="${HOME}/.tmp/tni_internal_$$";
fi

if test '!' -d ${temp_dir} ; then
    (mkdir  ${temp_dir} || true );
    (mkdir -p ${temp_dir} || true );
fi

tmpdir="${temp_dir}"

( mkdir ${tmpdir} && chmod u+w ${tmpdir}  || true) >/dev/null 2>/dev/null ;


cat >${tmpdir}/psgrepi.sh <<EOF
#! /bin/sh
ps | grep -v internal | grep -i \${1}
ps -o pid,command | grep -v internal | grep -i \${1}
EOF

chmod a+x ${tmpdir}/psgrepi.sh

# PS_GREP_I should be set to something that can be used to produce a list
# of all processes created by running a certain program with a given string
# in the name.
# Cygwin is the only system I know of where the -i is really needed 
# (because the ps under cygwin does not preserve case ) 
if test "x${PS_GREP_I}" = "x" ; then
    PS_GREP_I="${tmpdir}/psgrepi.sh"
fi

if test -f ${tmpdir}/${TESTNAME}_test.nml ; then
    \rm -f ${TESTNAME}_test.nml;
fi

echo \$0=$0
echo \$\$=$$
echo \$PPID=\$PPID
 
ps | grep $$
ps -ael | grep $$
ps -ael | grep $PPID

ps -eo pid,cmd | grep $$
ps -eo pid,cmd | grep $PPID

# This will make the rcslib slow and verbose
#RCS_PRINT_FLAGS=0xFFFFFFF;
#export RCS_PRINT_FLAGS

if test "x${HOME}" = "x" ; then
    HOME=~
fi

if test '!' -d ${tmpdir} ; then
    mkdir ${tmpdir}
fi

echo "${0}:${LINENO} Test Date:"
date
echo "${0}:${LINENO} Current directory:"
pwd
echo "${0}:${LINENO} SET"
set
echo "${0}:${LINENO} PRINTENV"
printenv

builddir=`pwd`

if test '!' -x "${builddir}/nmlclean${EXEEXT}" -a '!' -x "${builddir}/nmlcfg${EXEEXT}" -a '!' -x "${builddir}/nml_test_server${EXEEXT}" ; then
    if test -x './=build/nmlclean${EXEEXT}' -a -x './=build/nmlcfg${EXEEXT}' -a -x './=build/nml_test_server${EXEEXT}' ; then
	builddir=`pwd`'/=build';
	echo "${0}:${LINENO} build directory set to:"
	echo ${builddir}
    fi
fi

if test '!' -x "${builddir}/nmlclean${EXEEXT}" -a '!' -x "${builddir}/nmlcfg${EXEEXT}" -a '!' -x "${builddir}/nml_test_server${EXEEXT}" ; then

    if test "x${srcdir}" != "x" ; then
	if test ! -d ./etc -a -d "${srcdir}/etc" ; then
	    cd "${srcdir}";
	    echo "${0}:${LINENO} Directory changed to:" 
	    pwd
	fi
    fi
fi

NMLCLEAN_LOCAL_ONLY=1;
export NMLCLEAN_LOCAL_ONLY;


if test "x${NO_NMLCLEAN}" = "x" ; then
    if test -f "${tmpdir}/${TESTNAME}_test.nml" ; then
	"${builddir}/nmlclean${EXEEXT}" "${tmpdir}/${TESTNAME}_test.nml";
	if test -f nmlclean${EXEEXT}.log ; then
	    mv "nmlclean${EXEEXT}.log" "${tmpdir}/nmlclean${EXEEXT}-1-$$.log";
	fi
	\rm -f "${tmpdir}/${TESTNAME}_test.nml";
    fi
fi


if test '!' -x "${builddir}/nmlclean${EXEEXT}" -a '!' -x "${builddir}/nmlcfg${EXEEXT}"  ; then
    if test -x "${builddir}/bin/nmlclean${EXEEXT}" -a  -x "${builddir}/bin/nmlcfg${EXEEXT}"  ; then
	bindir="${builddir}/bin";
    else

	if test -x "${builddir}/.lastbuild/nmlclean${EXEEXT}" -a  -x "${builddir}/.lastbuild/nmlcfg${EXEEXT}"  ; then
	    bindir="${builddir}/.lastbuild";
	else
	    bindir="${builddir}";
	fi
    fi
else
    bindir="${builddir}"
fi

if test '!' -x "${builddir}/nml_test_server${EXEEXT}" -a '!' -x "${builddir}/nml_test_read${EXEEXT}" -a '!' -x "${builddir}/nml_test_write${EXEEXT}" -a '!' -x "${builddir}/nml_test_single_process_queue_test${EXEEXT}" -a '!' -x "${builddir}/nml_test_blocking_read${EXEEXT}" ; then

    if test -x "${builddir}/.lastbuild/nml_test_server${EXEEXT}" -a  -x "${builddir}/.lastbuild/nml_test_read${EXEEXT}" -a  -x "${builddir}/.lastbuild/nml_test_write${EXEEXT}" -a  -x "${builddir}/.lastbuild/nml_test_single_process_queue_test${EXEEXT}" -a  -x "${builddir}/.lastbuild/nml_test_blocking_read${EXEEXT}" ; then
	testbindir="${builddir}/.lastbuild";
    else
	if test -x "${builddir}/bin/nml_test_server${EXEEXT}" -a  -x "${builddir}/bin/nml_test_read${EXEEXT}" -a  -x "${builddir}/bin/nml_test_write${EXEEXT}" -a  -x "${builddir}/bin/nml_test_single_process_queue_test${EXEEXT}" -a  -x "${builddir}/bin/nml_test_blocking_read${EXEEXT}" ; then
	    testbindir="${builddir}/bin";
	else
	    testbindir="${builddir}";
	fi
    fi
else
    testbindir="${builddir}"
fi

if test "x${srcdir}" = "x" -a -f "${builddir}/src/test/test.nml2" -a '!' -f "${srcdir}/src/test/test.nml2" ; then
    srcdir="${builddir}";
fi

echo "${0}:${LINENO} ______________________________________"
echo "${0}:${LINENO} Starting NML TESTS:"

ls -l 
ls -l >&2

echo "${0}:${LINENO} Current directory:"
pwd
pwd >&2

(\rm -f ${tmpdir}/killnt.${TESTNAME}.*.sh ) >/dev/null 2>/dev/null 
(\rm -f ${tmpdir}/logstat.${TESTNAME}.*.sh ) >/dev/null 2>/dev/null 

LAUNCHER_TO_KILL=
if test "x${PROGRAM_LAUNCHER}" != "x" ; then
    LAUNCHER_TO_KILL=`echo ${PROGRAM_LAUNCHER} | sed 's/ --//'`
fi


echo PROGRAM_LAUNCHER="${PROGRAM_LAUNCHER}"
echo EXEEXT="${EXEEXT}"
echo KILL_LAUNCHER="${KILL_LAUNCHER}"

sleep ${TESTNML_SLEEP}

echo "${0}:${LINENO} Testing nmlcfg${EXEEXT}  . . ."

${KILL_LAUNCHER}

if test "x${IPV6}" != "x" ; then
    NMLCFG_OPTIONS="${NMLCFG_OPTIONS}%%%host=::1";
fi

echo NMLCFG_OPTIONS="${NMLCFG_OPTIONS}"

NMLCFG_PROG="${testbindir}/nmlcfg${EXEEXT}";

if test -x "${bindir}/nmlcfg${EXEEXT}" \
    -a '!' -x "${testbindir}/nmlcfg${EXEEXT}" ; then
    NMLCFG_PROG="${bindir}/nmlcfg${EXEEXT}";
fi

NMLCFGCMD="${PROGRAM_LAUNCHER} ${NMLCFG_PROG} ${NMLCFG_OPTIONS} ${srcdir}/src/test/test.nml2 --HEADER_DIR=${srcdir}/src/test/  -o ${tmpdir}/${TESTNAME}_test.nml";
echo NMLCFGCMD=${NMLCFGCMD}
${PROGRAM_LAUNCHER} "${NMLCFG_PROG}" ${NMLCFG_OPTIONS} "${srcdir}/src/test/test.nml2" "--HEADER_DIR=${srcdir}/src/test/"  -o "${tmpdir}/${TESTNAME}_test.nml";

nmlcfgsts=$?

echo "${0}:${LINENO} nmlcfgsts=${nmlcfgsts}";

if test ${nmlcfgsts} -ne 0 ; then
    echo "${0}:${LINENO} nmlcfg${EXEEXT} test failed.";
    exit 254;
fi

if test '!' -f "${srcdir}/src/test/test.nml2" ; then
    echo "${0}:${LINENO} nmlcfg${EXEEXT} test failed.";
    exit 253;
fi  

echo "${0}:${LINENO} ${srcdir}/src/test/test.nml2"
ls -l "${srcdir}/src/test/test.nml2"
cat "${srcdir}/src/test/test.nml2"

echo "${0}:${LINENO} ${tmpdir}/${TESTNAME}_test.nml"
ls -l "${tmpdir}/${TESTNAME}_test.nml"
cat "${tmpdir}/${TESTNAME}_test.nml"

if test -f ./config.dat ; then
    cat ./config.dat;
else
    echo "${0}:${LINENO} No config.dat"
fi


if test $? -ne 0 ; then
    echo "${0}:${LINENO} nmlcfg${EXEEXT} test failed"
    exit 252
fi

if test ! -f ${tmpdir}/${TESTNAME}_test.nml ; then
    echo "${0}:${LINENO} nmlcfg${EXEEXT} test failed"
    exit 251
fi

echo "${0}:${LINENO} nmlcfg${EXEEXT} test Passed"


#idlist="10001 10002 10003 10004 20001 20002 20003 20004" 

idlist1=`grep -i SHMEM ${srcdir}/src/test/*.nml | awk '{printf(" %s ",$10);}'`
idlist2=`grep -i SHMEM ${tmpdir}/*.nml | awk '{printf(" %s ",$10);}'`
idlist="${idlist1} ${idlist2}"

cat >${tmpdir}/logstat.${TESTNAME}.$$.sh <<EOF
#!/bin/sh
set -x
echo Running ${tmpdir}/logstat.${TESTNAME}.$$.sh from $0 $*
date_string=`date +%Y%m%d%H%M%S_%N`;
    ( ( 
    set -x; 
    pwd
    ls -l
    date
    ps
    ps -ae
    ipcs 
    netstat -napee
    netstat -na
    ls -l /dev/shmem/*
    ls -l /dev/sem/*
    ) 2>&1 ) > ${tmpdir}/logstat_${TESTNAME}.$$.\${date_string}.log
echo Finished ${tmpdir}/logstat.${TESTNAME}.$$.sh from $0 $*
echo Log saved to ${tmpdir}/logstat_${TESTNAME}.$$.\${date_string}.log;
ls -l ${tmpdir}/logstat_${TESTNAME}.$$.\${date_string}.log;

EOF

cat >${tmpdir}/killnt.${TESTNAME}.$$.sh <<EOF
#!/bin/sh
echo ""
echo ""
echo ""
set -x;
echo Running ${tmpdir}/killnt.${TESTNAME}.$$.sh from $0 $*
date_string=`date +%Y%m%d%H%M%S_%N`;
    ( ( 
    set -x
    sleep ${TESTNML_SLEEP}
    ps -ae
    ps
    echo "${0}:${LINENO} Kill any nml test programs still running."
    ${PS_GREP_I} nml_te | grep -v grep 
    procstokill=\`${PS_GREP_I} nml_te | grep -v grep  | awk '{printf(" %s",\$1);}'\`
    while test "x\${procstokill}" != "x" ; do
	sleep ${TESTNML_SLEEP}
        ${PS_GREP_I} nml_te | grep -v grep 

	if test x != "x\${procstokill}" ; then
	    kill -INT \$procstokill
	    sleep 2
	    procstokill=\`${PS_GREP_I} nml_te | grep -v grep  | awk '{printf(" %s",\$1);}'\`
	fi

	if test x != "x\${procstokill}" ; then
	    kill -INT \$procstokill
	    sleep 2
	    procstokill=\`${PS_GREP_I} nml_te | grep -v grep  | awk '{printf(" %s",\$1);}'\`
	fi

	if test x != "x\${procstokill}" ; then
	    kill -TERM \$procstokill
	    sleep 2
	    procstokill=\`${PS_GREP_I} nml_te | grep -v grep  | awk '{printf(" %s",\$1);}'\`
	fi

	if test x != "x\${procstokill}" ; then
	    kill -TERM \$procstokill
	    sleep 2
	    procstokill=\`${PS_GREP_I} nml_te | grep -v grep  | awk '{printf(" %s",\$1);}'\`
	fi

	if test x != "x\${procstokill}" ; then
	    kill -KILL \$procstokill
	    sleep 2
	    procstokill=\`${PS_GREP_I} nml_te | grep -v grep  | awk '{printf(" %s",\$1);}'\`
	fi

	if test x != "x\${procstokill}" ; then
	    kill -KILL \$procstokill
	    sleep 2
	    procstokill=\`${PS_GREP_I} nml_te | grep -v grep  | awk '{printf(" %s",\$1);}'\`
	fi
    done
    sleep ${TESTNML_SLEEP}
    if test "x${LAUNCHER_TO_KILL}" != "x" ; then 
    ps -ae
    ps
    ${PS_GREP_I} ${LAUNCHER_TO_KILL} | grep -v grep 
    procstokill=\`${PS_GREP_I} ${LAUNCHER_TO_KILL} | grep -v grep  | awk '{printf(" %s",\$1);}'\`
    while test "x\${procstokill}" != "x" ; do
	sleep ${TESTNML_SLEEP}
        ${PS_GREP_I} nml_te | grep -v grep 

	if test x != "x\${procstokill}" ; then
	    kill -INT \$procstokill
	    sleep 2
	    procstokill=\`${PS_GREP_I} nml_te | grep -v grep  | awk '{printf(" %s",\$1);}'\`
	fi

	if test x != "x\${procstokill}" ; then
	    kill -INT \$procstokill
	    sleep 2
	    procstokill=\`${PS_GREP_I} nml_te | grep -v grep  | awk '{printf(" %s",\$1);}'\`
	fi

	if test x != "x\${procstokill}" ; then
	    kill -TERM \$procstokill
	    sleep 2
	    procstokill=\`${PS_GREP_I} nml_te | grep -v grep  | awk '{printf(" %s",\$1);}'\`
	fi

	if test x != "x\${procstokill}" ; then
	    kill -TERM \$procstokill
	    sleep 2
	    procstokill=\`${PS_GREP_I} nml_te | grep -v grep  | awk '{printf(" %s",\$1);}'\`
	fi

	if test x != "x\${procstokill}" ; then
	    kill -KILL \$procstokill
	    sleep 2
	    procstokill=\`${PS_GREP_I} nml_te | grep -v grep  | awk '{printf(" %s",\$1);}'\`
	fi

	if test x != "x\${procstokill}" ; then
	    kill -KILL \$procstokill
	    sleep 2
	    procstokill=\`${PS_GREP_I} nml_te | grep -v grep  | awk '{printf(" %s",\$1);}'\`
	fi
    done
    fi
    sleep ${TESTNML_SLEEP}
    ipcs 
    ipcrm  `ipcs -s | gawk '{if(\$1 != "0x00000000" && \$1 != "") printf("-s %s\n",\$2);}' | grep -vi sh | grep -vi si | grep -vi se`
ipcrm  `ipcs -m | gawk '{if(\$1 != "0x00000000" && \$1 != "") printf("-m %s\n",\$2);}' | grep -vi sh | grep -vi si | grep -vi se`
    ipcrm -M \${id}
    ipcrm -S \${id}
    ipcrm -m \${id}
    ipcrm -s \${id}
   
    if test -x ${builddir}/posix_shm_unlink ; then
        for id in ${idlist} ; do
	    "${builddir}/posix_shm_unlink" /_\${id}.shm
        done
    fi

    if test -x ${builddir}/posix_sem_unlink ; then
        for id in ${idlist} ; do
	    "${builddir}/posix_sem_unlink" /_\${id}.sem
        done
    fi

    if test -x \${builddir}/posix_shm_unlink ; then
        for id in ${idlist} ; do
	    "\${builddir}/posix_shm_unlink" /_\${id}.shm
        done
    fi

    if test -x \${builddir}/posix_sem_unlink ; then
        for id in ${idlist} ; do
	    "\${builddir}/posix_sem_unlink" /_\${id}.sem
        done
    fi

    if test -x ${testbindir}/posix_shm_unlink ; then
        for id in ${idlist} ; do
	    "${testbindir}/posix_shm_unlink" /_\${id}.shm
        done
    fi

    if test -x ${testbindir}/posix_sem_unlink ; then
        for id in ${idlist} ; do
	    "${testbindir}/posix_sem_unlink" /_\${id}.sem
        done
    fi

    if test -x \${testbindir}/posix_shm_unlink ; then
        for id in ${idlist} ; do
	    "\${testbindir}/posix_shm_unlink" /_\${id}.shm
        done
    fi

    if test -x \${testbindir}/posix_sem_unlink ; then
        for id in ${idlist} ; do
	    "\${testbindir}/posix_sem_unlink" /_\${id}.sem
        done
    fi

    if test -x ${bindir}/posix_shm_unlink ; then
        for id in ${idlist} ; do
	    "${bindir}/posix_shm_unlink" /_\${id}.shm
        done
    fi

    if test -x ${bindir}/posix_sem_unlink ; then
        for id in ${idlist} ; do
	    "${bindir}/posix_sem_unlink" /_\${id}.sem
        done
    fi

    if test -x \${bindir}/posix_shm_unlink ; then
        for id in ${idlist} ; do
	    "\${bindir}/posix_shm_unlink" /_\${id}.shm
        done
    fi

    if test -x \${bindir}/posix_sem_unlink ; then
        for id in ${idlist} ; do
	    "\${bindir}/posix_sem_unlink" /_\${id}.sem
        done
    fi
    rm /dev/sem/_*.sem
    rm /dev/shmem/_*.shm
    ps -ae | grep defun
    ps -ael | grep defun
    ps -ael | grep defunct |  awk '{printf("%d ",$4);}'
    defunct_pids=`ps -ael | grep defunct |  awk '{printf("%d ",$4);}'`
    kill -KILL ${defunct_pids}
    ps -ae | grep defunct |  awk '{printf("%d ",$1);}'
    defunct_pids=`ps -ael | grep defunct |  awk '{printf("%d ",$1);}'`
    kill -KILL ${defunct_pids}
    ${EXTRA_KILL_COMMAND} 
    ) 2>&1 ) > ${tmpdir}/killnt_${TESTNAME}.$$.\${date_string}.log
echo Finished ${tmpdir}/killnt.${TESTNAME}.$$.sh from $0 $*
echo Log saved to ${tmpdir}/killnt_${TESTNAME}.$$.\${date_string}.log;
ls -l ${tmpdir}/killnt_${TESTNAME}.$$.\${date_string}.log;
echo ""
echo ""
echo ""

EOF


chmod a+x "${tmpdir}/killnt.${TESTNAME}.$$.sh";
ls -l "${tmpdir}/killnt.${TESTNAME}.$$.sh";
#cat "${tmpdir}/killnt.${TESTNAME}.$$.sh";

"${tmpdir}/killnt.${TESTNAME}.$$.sh"


chmod a+x "${tmpdir}/logstat.${TESTNAME}.$$.sh";
ls -l "${tmpdir}/logstat.${TESTNAME}.$$.sh";
#cat "${tmpdir}/logstat.${TESTNAME}.$$.sh";

"${tmpdir}/logstat.${TESTNAME}.$$.sh"

${KILL_LAUNCHER}
sleep 5


${KILL_LAUNCHER}

pwd
sleep 5

"${tmpdir}/killnt.${TESTNAME}.$$.sh";

"${tmpdir}/logstat.${TESTNAME}.$$.sh";


if test "x${PROGRAM_LAUNCHER}" != "x" ; then
    "${PS_GREP_I}" "${PROGRAM_LAUNCHER}"
fi
"${PS_GREP_I}" nml_te;

echo NML_TEST_LOCAL="${NML_TEST_LOCAL}";

if test  "x${NML_TEST_LOCAL}" != "xno" ; then

SERVER_LOG="${tmpdir}/${TESTNAME}_test_server1.log";

echo "${0}:${LINENO} Starting server${EXEEXT} . . ."

if test -f nml_test_server.running ; then \rm -f nml_test_server.running; fi
( ( set -x;  ${PROGRAM_LAUNCHER} "${testbindir}/nml_test_server${EXEEXT}" b1 b1s "${tmpdir}/${TESTNAME}_test.nml" 2>&1 ) | tee "${SERVER_LOG}" ) &
sleep ${TESTNML_SLEEP};
tries=0;
while true ; do
    echo "${0}:${LINENO} waiting for nml_test_server..."
    sleep ${TESTNML_SLEEP};
    let tries++;
    if test ${tries} -gt 40 ; then
	echo "${0}:${LINENO}  timedout.";
	exit 2;
    fi
    echo -n ".";

    if test -f "nml_test_server.running" ; then
	echo "${0}:${LINENO}  done.";
	break;
    fi
done;
sleep ${TESTNML_SLEEP}
if test "x${PROGRAM_LAUNCHER}" != "x" ; then
    "${PS_GREP_I}" ${PROGRAM_LAUNCHER}
fi
"${PS_GREP_I}" nml_te;

sleep ${TESTNML_SLEEP}0

"${tmpdir}/logstat.${TESTNAME}.$$.sh";

echo "${0}:${LINENO} Testing local write . . . "
${PROGRAM_LAUNCHER} "${testbindir}/nml_test_write${EXEEXT}" b1 lw "${tmpdir}/${TESTNAME}_test.nml" 7;
nmltestwritests=$?
echo "${0}:${LINENO} nmltestwritests=${nmltestwritests}"

if test ${nmltestwritests} -ne 0 ; then 
    echo "${0}:${LINENO} ERROR: Local write test failed."
    echo "${0}:${LINENO} ERROR: Local write test failed. (2)" >&2
    "${tmpdir}/logstat.${TESTNAME}.$$.sh"
    "${tmpdir}/killnt.${TESTNAME}.$$.sh";
    echo "${0}:${LINENO} ERROR: Local write test failed."
    echo "${0}:${LINENO} ERROR: Local write test failed. (2)" >&2
    echo "${0}:${LINENO} ERROR: BEGIN -> ${SERVER_LOG}"
    tail -n 30 "${SERVER_LOG}"
    echo "${0}:${LINENO} ERROR: END <- ${SERVER_LOG}"
    exit 250
fi
echo "${0}:${LINENO} local write test passed.";

echo "${0}:${LINENO} Testing local read . . . ";
${PROGRAM_LAUNCHER}  "${testbindir}/nml_test_read${EXEEXT}" b1 lr "${tmpdir}/${TESTNAME}_test.nml" 7;
nmltestreadsts=$?
echo "${0}:${LINENO} nmltestreadsts=${nmltestreadsts}";

if test ${nmltestreadsts} -ne 0 ; then 
    echo "${0}:${LINENO} ERROR: Local read test failed."
    echo "${0}:${LINENO} ERROR: Local read test failed. (2)" >&2
    "${tmpdir}/logstat.${TESTNAME}.$$.sh"
    "${tmpdir}/killnt.${TESTNAME}.$$.sh";
    echo "${0}:${LINENO} ERROR: Local read test failed."
    echo "${0}:${LINENO} ERROR: Local read test failed. (2)" >&2
    echo "${0}:${LINENO} ERROR: BEGIN -> ${SERVER_LOG}"
    tail -n 30 "${SERVER_LOG}"
    echo "${0}:${LINENO} ERROR: END <- ${SERVER_LOG}"
    exit 119
fi

echo "${0}:${LINENO} Testing local read using passed address  . . . ";
${PROGRAM_LAUNCHER}  "${testbindir}/nml_test_read${EXEEXT}" b1 lr "${tmpdir}/${TESTNAME}_test.nml" 7 use_read_with_args;
nmltestreadsts=$?
echo "${0}:${LINENO} nmltestreadsts=${nmltestreadsts}";

if test ${nmltestreadsts} -ne 0 ; then 
    echo "${0}:${LINENO} ERROR: Local read test failed."
    echo "${0}:${LINENO} ERROR: Local read test failed. (2)" >&2
    "${tmpdir}/logstat.${TESTNAME}.$$.sh"
    "${tmpdir}/killnt.${TESTNAME}.$$.sh";
    echo "${0}:${LINENO} ERROR: Local read test failed."
    echo "${0}:${LINENO} ERROR: Local read test failed. (2)" >&2
    echo "${0}:${LINENO} ERROR: BEGIN -> ${SERVER_LOG}"
    tail -n 30 "${SERVER_LOG}"
    echo "${0}:${LINENO} ERROR: END <- ${SERVER_LOG}"
    exit 119
fi
echo "${0}:${LINENO} local read test passed."

"${tmpdir}/killnt.${TESTNAME}.$$.sh";

${KILL_LAUNCHER}
sleep 5
${KILL_LAUNCHER}

sleep 5

"${tmpdir}/logstat.${TESTNAME}.$$.sh";

if test "x${NO_NMLCLEAN}" = "x" ; then

    NMLCLEAN_PROG="${testbindir}/nmlclean${EXEEXT}";

    if test -x "${bindir}/nmlclean${EXEEXT}" -a '!' -x "${testbindir}/nmlclean${EXEEXT}" ; then
	NMLCLEAN_PROG="${bindir}/nmlclean${EXEEXT}";
    fi
    
    NMLCLEANCMD="${PROGRAM_LAUNCHER} ${NMLCLEAN_PROG} ${tmpdir}/${TESTNAME}_test.nml";
    echo NMLCLEANCMD="${NMLCLEANCMD}";
    
    ${PROGRAM_LAUNCHER} "${NMLCLEAN_PROG}" "${tmpdir}/${TESTNAME}_test.nml";
    
    nmlcleansts=$?

    echo "${0}:${LINENO} nmlcleansts=${nmlcleansts}";
fi

"${tmpdir}/logstat.${TESTNAME}.$$.sh";

# matches "x${NML_TEST_LOCAL}" != "xno" 
fi

if test "x${NML_TEST_QUEUE}" != "xno" -a "x${NML_TEST_LOCAL}" != "xno" ; then
    
sleep 5
ps -ae
if test "x${PROGRAM_LAUNCHER}" != "x" ; then
    "${PS_GREP_I}" ${PROGRAM_LAUNCHER}
fi
"${PS_GREP_I}" nml_te;
sync
sleep ${TESTNML_SLEEP}

SERVER_LOG="${tmpdir}/${TESTNAME}_test_server2.log";

echo "${0}:${LINENO} Starting server${EXEEXT} . . ."
if test -f nml_test_server.running ; then \rm -f nml_test_server.running; fi
( ( set -x; ${PROGRAM_LAUNCHER} "${testbindir}/nml_test_server${EXEEXT}" qb b1s "${tmpdir}/${TESTNAME}_test.nml" 2>&1 ) | tee "${SERVER_LOG}" ) &
sleep ${TESTNML_SLEEP};
tries=0;
while true ; do
    echo "${0}:${LINENO} waiting for nml_test_server..."
    sleep ${TESTNML_SLEEP};
    let tries++;
    if test ${tries} -gt 40 ; then
	echo "${0}:${LINENO}  timedout.";
	exit 2;
    fi
    echo -n ".";

    if test -f "nml_test_server.running" ; then
	echo "${0}:${LINENO}  done.";
	break;
    fi
done;
sleep ${TESTNML_SLEEP}


if test "x${PROGRAM_LAUNCHER}" != "x" ; then
    "${PS_GREP_I}" ${PROGRAM_LAUNCHER}
fi
"${PS_GREP_I}" nml_te;

"${tmpdir}/logstat.${TESTNAME}.$$.sh";

sleep ${TESTNML_SLEEP}

if test "x${NO_SINGLE_PROCESS_QUEUE_TEST}" = "x" ; then

    echo "${0}:${LINENO} Testing local single process queue tests . . . "
    \rm -f "${testbindir}/ntspqt_lw.${TESTNAME}.$$.log";
    ( ${PROGRAM_LAUNCHER} "${testbindir}/nml_test_single_process_queue_test${EXEEXT}" qb lw "${tmpdir}/${TESTNAME}_test.nml" 99 100 2>&1 ) >"${testbindir}/ntspqt_lw.${TESTNAME}.$$.log";
    nmltestsingleprocessqueuests=$?
    echo "${0}:${LINENO} nmlsingleprocessqueuests=${nmltestsingleprocessqueuests}"
    ls -l "${testbindir}/ntspqt_lw.${TESTNAME}.$$.log";
    
    if test ${nmltestsingleprocessqueuests} -ne 0 ; then 
	echo "${0}:${LINENO} ERROR: Local single process queue test failed."
	echo "${0}:${LINENO} ERROR: Local single process queue test failed. (2)" >&2
	"${tmpdir}/logstat.${TESTNAME}.$$.sh"
	"${tmpdir}/killnt.${TESTNAME}.$$.sh";
	echo "${0}:${LINENO} ERROR: Local single process test failed."
	echo "${0}:${LINENO} ERROR: Local single process test failed. (2)" >&2
	echo "${0}:${LINENO} ERROR: BEGIN -> ${testbindir}/ntspqt_lw.${TESTNAME}.$$.log"
	tail -n 30 "${testbindir}/ntspqt_lw.${TESTNAME}.$$.log"
	echo "${0}:${LINENO} ERROR: END <- ${testbindir}/ntspqt_lw.${TESTNAME}.$$.log"
	echo "${0}:${LINENO} ERROR: BEGIN -> ${SERVER_LOG}"
	tail -n 30 "${SERVER_LOG}"
	echo "${0}:${LINENO} ERROR: END <- ${SERVER_LOG}"
	exit 122
    fi
    echo "${0}:${LINENO} local single process queue passed."

fi


echo "${0}:${LINENO} Testing local write to queued buffer . . . "
${PROGRAM_LAUNCHER} "${testbindir}/nml_test_write${EXEEXT}" qb lw "${tmpdir}/${TESTNAME}_test.nml" 99;
nmltestwritests=$?
echo "${0}:${LINENO} nmltestwritests=${nmltestwritests}"

if test ${nmltestwritests} -ne 0 ; then 
    echo "${0}:${LINENO} Local write test to queued buffer failed."
    "${tmpdir}/logstat.${TESTNAME}.$$.sh"
    "${tmpdir}/killnt.${TESTNAME}.$$.sh";
    echo "${0}:${LINENO} Local write test to queued buffer failed."
    echo "${0}:${LINENO} ERROR: BEGIN -> ${SERVER_LOG}"
    tail -n 30 "${SERVER_LOG}"
    echo "${0}:${LINENO} ERROR: END <- ${SERVER_LOG}"
    exit 118
fi
echo "${0}:${LINENO} local write to queued buffer test passed."

echo "${0}:${LINENO} Testing get_queue_length . . . "
${PROGRAM_LAUNCHER} "${testbindir}/nml_test_get_queue_length${EXEEXT}" qb lw "${tmpdir}/${TESTNAME}_test.nml" 1;
nmltestgetqueuelengthsts=$?
echo "${0}:${LINENO} nmltestgetqueuelengthsts=${nmltestgetqueuelengthsts}"

if test ${nmltestgetqueuelengthsts} -ne 0 ; then 
    echo "${0}:${LINENO} Local get_queue_length failed."
    "${tmpdir}/logstat.${TESTNAME}.$$.sh"
    "${tmpdir}/killnt.${TESTNAME}.$$.sh"
    echo "${0}:${LINENO} Local get_queue_length failed."
    echo "${0}:${LINENO} ERROR: BEGIN -> ${SERVER_LOG}"
    tail -n 30 "${SERVER_LOG}"
    echo "${0}:${LINENO} ERROR: END <- ${SERVER_LOG}"
   exit 117
fi
echo "${0}:${LINENO} local get_queue_length test passed."

${PROGRAM_LAUNCHER} "${testbindir}/nml_test_write${EXEEXT}" qb lw "${tmpdir}/${TESTNAME}_test.nml" 98
nmltestwritests=$?
echo "${0}:${LINENO} nmltestwritests=${nmltestwritests}"

if test ${nmltestwritests} -ne 0 ; then 
    echo "${0}:${LINENO} Local write test to queued buffer failed."
    "${tmpdir}/logstat.${TESTNAME}.$$.sh"
    "${tmpdir}/killnt.${TESTNAME}.$$.sh"
    echo "${0}:${LINENO} Local write test to queued buffer failed."
    echo "${0}:${LINENO} ERROR: BEGIN -> ${SERVER_LOG}"
    tail -n 30 "${SERVER_LOG}"
    echo "${0}:${LINENO} ERROR: END <- ${SERVER_LOG}"
   exit 117
fi
echo "${0}:${LINENO} local write to queued buffer test passed."

${PROGRAM_LAUNCHER} "${testbindir}/nml_test_write${EXEEXT}" qb lw "${tmpdir}/${TESTNAME}_test.nml" 97;
nmltestwritests=$?
echo "${0}:${LINENO} nmltestwritests=${nmltestwritests}"

if test ${nmltestwritests} -ne 0 ; then 
    echo "${0}:${LINENO} Local write test to queued buffer failed."
    "${tmpdir}/logstat.${TESTNAME}.$$.sh"
    "${tmpdir}/killnt.${TESTNAME}.$$.sh";
    echo "${0}:${LINENO} Local write test to queued buffer failed."
    echo "${0}:${LINENO} ERROR: BEGIN -> ${SERVER_LOG}"
    tail -n 30 "${SERVER_LOG}"
    echo "${0}:${LINENO} ERROR: END <- ${SERVER_LOG}"
    exit 116
fi
echo "${0}:${LINENO} local write to queued buffer test passed."

echo "${0}:${LINENO} Testing get_queue_length . . . "
${PROGRAM_LAUNCHER} "${testbindir}/nml_test_get_queue_length${EXEEXT}" qb lw "${tmpdir}/${TESTNAME}_test.nml" 3;
nmltestgetqueuelengthsts=$?
echo "${0}:${LINENO} nmltestgetqueuelengthsts=${nmltestgetqueuelengthsts}"

if test ${nmltestgetqueuelengthsts} -ne 0 ; then 
    echo "${0}:${LINENO} Local get_queue_length failed."
    "${tmpdir}/logstat.${TESTNAME}.$$.sh"
    "${tmpdir}/killnt.${TESTNAME}.$$.sh"
    echo "${0}:${LINENO} Local get_queue_length failed."
    echo "${0}:${LINENO} ERROR: BEGIN -> ${SERVER_LOG}"
    tail -n 30 "${SERVER_LOG}"
    echo "${0}:${LINENO} ERROR: END <- ${SERVER_LOG}"
   exit 117
fi
echo "${0}:${LINENO} local get_queue_length test passed."
sleep 2

echo "${0}:${LINENO} Testing local read from queued buffer. . . "
${PROGRAM_LAUNCHER}  "${testbindir}/nml_test_read${EXEEXT}" qb lr "${tmpdir}/${TESTNAME}_test.nml" 99;
nmltestreadsts=$?
echo "${0}:${LINENO} nmltestreadsts=${nmltestreadsts}"
if test ${nmltestreadsts} -ne 0 ; then 
    echo "${0}:${LINENO} Local read test from queued buffer failed."
    "${tmpdir}/logstat.${TESTNAME}.$$.sh"
    "${tmpdir}/killnt.${TESTNAME}.$$.sh";
    echo "${0}:${LINENO} Local read test from queued buffer failed."
    echo "${0}:${LINENO} ERROR: BEGIN -> ${SERVER_LOG}"
    tail -n 30 "${SERVER_LOG}"
    echo "${0}:${LINENO} ERROR: END <- ${SERVER_LOG}"
    exit 115
fi

${PROGRAM_LAUNCHER}  "${testbindir}/nml_test_read${EXEEXT}" qb lr "${tmpdir}/${TESTNAME}_test.nml" 98;
nmltestreadsts=$?
echo "${0}:${LINENO} nmltestreadsts=${nmltestreadsts}"
if test ${nmltestreadsts} -ne 0 ; then 
    echo "${0}:${LINENO} Local read test from queued buffer failed."
    "${tmpdir}/killnt.${TESTNAME}.$$.sh";
    echo "${0}:${LINENO} Local read test from queued buffer failed."
    echo "${0}:${LINENO} ERROR: BEGIN -> ${SERVER_LOG}"
    tail -n 30 "${SERVER_LOG}"
    echo "${0}:${LINENO} ERROR: END <- ${SERVER_LOG}"
    exit 114
fi

${PROGRAM_LAUNCHER}  "${testbindir}/nml_test_read${EXEEXT}" qb lr "${tmpdir}/${TESTNAME}_test.nml" 97 use_read_with_args;
nmltestreadsts=$?
echo "${0}:${LINENO} nmltestreadsts=${nmltestreadsts}"
if test ${nmltestreadsts} -ne 0 ; then 
    echo "${0}:${LINENO} Local read test from queued buffer failed."
    "${tmpdir}/logstat.${TESTNAME}.$$.sh"
    "${tmpdir}/killnt.${TESTNAME}.$$.sh";
    echo "${0}:${LINENO} Local read test from queued buffer failed."
    echo "${0}:${LINENO} ERROR: BEGIN -> ${SERVER_LOG}"
    tail -n 30 "${SERVER_LOG}"
    echo "${0}:${LINENO} ERROR: END <- ${SERVER_LOG}"
    exit 113
fi

echo "${0}:${LINENO} local read from queued buffer test passed."

echo "${0}:${LINENO} Testing get_queue_length . . . "
${PROGRAM_LAUNCHER} "${testbindir}/nml_test_get_queue_length${EXEEXT}" qb lw "${tmpdir}/${TESTNAME}_test.nml" 0;
nmltestgetqueuelengthsts=$?
echo "${0}:${LINENO} nmltestgetqueuelengthsts=${nmltestgetqueuelengthsts}"

if test ${nmltestgetqueuelengthsts} -ne 0 ; then 
    echo "${0}:${LINENO} ERROR: Local get_queue_length failed."
    "${tmpdir}/logstat.${TESTNAME}.$$.sh"
    "${tmpdir}/killnt.${TESTNAME}.$$.sh"
    echo "${0}:${LINENO} ERROR: Local get_queue_length failed."
    echo "${0}:${LINENO} ERROR: BEGIN -> ${SERVER_LOG}"
    tail -n 30 "${SERVER_LOG}"
    echo "${0}:${LINENO} ERROR: END <- ${SERVER_LOG}"
   exit 117
fi
echo "${0}:${LINENO} local get_queue_length test passed."


"${tmpdir}/killnt.${TESTNAME}.$$.sh";

${KILL_LAUNCHER}
sleep 5
${KILL_LAUNCHER}

sleep 5

"${tmpdir}/logstat.${TESTNAME}.$$.sh";

if test "x${NO_NMLCLEAN}" = "x" ; then

    NMLCLEAN_PROG="${testbindir}/nmlclean${EXEEXT}";

    if test -x "${bindir}/nmlclean${EXEEXT}" -a '!' -x "${testbindir}/nmlclean${EXEEXT}" ; then
	NMLCLEAN_PROG="${bindir}/nmlclean${EXEEXT}";
    fi
    
    NMLCLEANCMD="${PROGRAM_LAUNCHER} ${NMLCLEAN_PROG} ${tmpdir}/${TESTNAME}_test.nml";
    echo "${0}:${LINENO} NMLCLEANCMD=${NMLCLEANCMD}";
    
    ${PROGRAM_LAUNCHER} "${NMLCLEAN_PROG}" "${tmpdir}/${TESTNAME}_test.nml";

    nmlcleansts=$?

    echo "${0}:${LINENO} nmlcleansts=${nmlcleansts}"

fi

"${tmpdir}/logstat.${TESTNAME}.$$.sh";

# matches  test "x${NML_TEST_QUEUE}" != "xno" -a "x${NML_TEST_LOCAL}" != "xno"
fi

"${PS_GREP_I}" nml_te;

SERVER_LOG="${tmpdir}/${TESTNAME}_test_server3.log";

echo "${0}:${LINENO} Starting server${EXEEXT} . . ."
if test -f nml_test_server.running ; then \rm -f nml_test_server.running; fi
( ( set -x; ${PROGRAM_LAUNCHER} "${testbindir}/nml_test_server${EXEEXT}" b2 b2s "${tmpdir}/${TESTNAME}_test.nml" 2>&1 ) | tee "${SERVER_LOG}" ) &
sleep ${TESTNML_SLEEP};
tries=0;
while true ; do
    echo "${0}:${LINENO} waiting for nml_test_server..."
    sleep ${TESTNML_SLEEP};
    let tries++;
    if test ${tries} -gt 40 ; then
	echo "${0}:${LINENO}  timedout.";
	exit 2;
    fi
    echo -n ".";

    if test -f "nml_test_server.running" ; then
	echo "${0}:${LINENO}  done.";
	break;
    fi
done;
sleep ${TESTNML_SLEEP}

"${tmpdir}/logstat.${TESTNAME}.$$.sh";

sleep ${TESTNML_SLEEP}

echo NML_TEST_LOCAL="${NML_TEST_LOCAL}";
echo NML_TEST_BLOCKING="${NML_TEST_BLOCKING}";

if test "x${NML_TEST_BLOCKING}" != "xno" -a "x${NML_TEST_LOCAL}" != "xno" ; then 
( sleep 5 ; ${PROGRAM_LAUNCHER} "${testbindir}/nml_test_write${EXEEXT}" b2 lw "${tmpdir}/${TESTNAME}_test.nml" 8; \
    echo nml_test_write for blocking read test exited with status $? ; \
    ${PROGRAM_LAUNCHER} "${testbindir}/nml_test_blocking_read${EXEEXT}" b2 lr "${tmpdir}/${TESTNAME}_test.nml" 10 8 \
 ) &

echo "${0}:${LINENO} Testing local blocking read . . . "
${PROGRAM_LAUNCHER} "${testbindir}/nml_test_blocking_read${EXEEXT}" b2 lr "${tmpdir}/${TESTNAME}_test.nml" 30 8 &
${PROGRAM_LAUNCHER} "${testbindir}/nml_test_blocking_read${EXEEXT}" b2 lr "${tmpdir}/${TESTNAME}_test.nml" 30 8 &
${PROGRAM_LAUNCHER} "${testbindir}/nml_test_blocking_read${EXEEXT}" b2 lr "${tmpdir}/${TESTNAME}_test.nml" 30 8 &
sleep ${TESTNML_SLEEP}
${PROGRAM_LAUNCHER} "${testbindir}/nml_test_blocking_read${EXEEXT}" b2 lr "${tmpdir}/${TESTNAME}_test.nml" 30 8 &
${PROGRAM_LAUNCHER} "${testbindir}/nml_test_blocking_read${EXEEXT}" b2 lr "${tmpdir}/${TESTNAME}_test.nml" 30 8

nmltestblockingreadsts=$?
echo "${0}:${LINENO} nmltestblockingreadsts=${nmltestblockingreadsts}"

if test ${nmltestblockingreadsts} -ne 0 ; then 
    echo "${0}:${LINENO} ERROR: Local blocking read test failed."
    echo "${0}:${LINENO} ERROR: Local blocking read test failed. (2)" >&2
    "${tmpdir}/logstat.${TESTNAME}.$$.sh"
    "${tmpdir}/killnt.${TESTNAME}.$$.sh"
    echo "${0}:${LINENO} ERROR: Local blocking read test failed."
    echo "${0}:${LINENO} ERROR: Local blocking read test failed. (2)" >&2
    echo "${0}:${LINENO} ERROR: BEGIN -> ${SERVER_LOG}"
    tail -n 30 "${SERVER_LOG}"
    echo "${0}:${LINENO} ERROR: END <- ${SERVER_LOG}"
    exit 112
fi
echo "${0}:${LINENO} local read blocking test passed."

"${tmpdir}/killnt.${TESTNAME}.$$.sh";

${KILL_LAUNCHER}
sleep 5
${KILL_LAUNCHER}

sleep 5
"${tmpdir}/logstat.${TESTNAME}.$$.sh";

## "x${NML_TEST_BLOCKING}" != "xno" -a "x${NML_TEST_LOCAL}" != "xno"
fi


echo NML_TEST_REMOTE="${NML_TEST_REMOTE}"

if test "x${NML_TEST_REMOTE}" != "xno" ; then

sleep 5

if test "x${PROGRAM_LAUNCHER}" != "x" ; then
    "${PS_GREP_I}" ${PROGRAM_LAUNCHER}
fi
"${PS_GREP_I}" nml_te;

SERVER_LOG="${tmpdir}/${TESTNAME}_test_server4.log";

echo "${0}:${LINENO} Starting server${EXEEXT} . . ."
if test -f nml_test_server.running ; then \rm -f nml_test_server.running; fi
( ( set -x; ${PROGRAM_LAUNCHER} "${testbindir}/nml_test_server${EXEEXT}" b1 b1s "${tmpdir}/${TESTNAME}_test.nml" 2>&1 ) | tee "${SERVER_LOG}" ) &
sleep ${TESTNML_SLEEP};
tries=0;
while true ; do
    echo "${0}:${LINENO} waiting for nml_test_server..."
    sleep ${TESTNML_SLEEP};
    let tries++;
    if test ${tries} -gt 40 ; then
	echo "${0}:${LINENO}  timedout.";
	exit 2;
    fi
    echo -n ".";

    if test -f "nml_test_server.running" ; then
	echo "${0}:${LINENO}  done.";
	break;
    fi
done;
sleep ${TESTNML_SLEEP}

if test "x${PROGRAM_LAUNCHER}" != "x" ; then
    "${PS_GREP_I}" ${PROGRAM_LAUNCHER};
fi
"${PS_GREP_I}" nml_te;

"${tmpdir}/logstat.${TESTNAME}.$$.sh";

sleep ${TESTNML_SLEEP}0


echo "${0}:${LINENO} Testing remote write . . . "
${PROGRAM_LAUNCHER} "${testbindir}/nml_test_write${EXEEXT}" b1 rw "${tmpdir}/${TESTNAME}_test.nml" 7

nmltestwritests=$?
echo "${0}:${LINENO} nmltestwritests=${nmltestwritests}"

if test ${nmltestwritests} -ne 0 ; then 
    echo "${0}:${LINENO} ERROR: Remote write test failed."
    "${tmpdir}/logstat.${TESTNAME}.$$.sh"
    "${tmpdir}/killnt.${TESTNAME}.$$.sh";
    echo "${0}:${LINENO} ERROR: Remote write test failed."
    echo "${0}:${LINENO} Begin ${SERVER_LOG}"
    tail -n 30 "${SERVER_LOG}"
    echo "${0}:${LINENO} End ${SERVER_LOG}"    
    exit 111
fi
echo "${0}:${LINENO} remote write test passed."


echo "${0}:${LINENO} Testing remote read . . . "
${PROGRAM_LAUNCHER} "${testbindir}/nml_test_read${EXEEXT}" b1 rr "${tmpdir}/${TESTNAME}_test.nml" 7

nmltestreadsts=$?
echo "${0}:${LINENO} nmltestreadsts=${nmltestreadsts}"

if test ${nmltestreadsts} -ne 0 ; then 
    echo "${0}:${LINENO} ERROR: Remote read failed."
    "${tmpdir}/logstat.${TESTNAME}.$$.sh"
    "${tmpdir}/killnt.${TESTNAME}.$$.sh";
    echo "${0}:${LINENO} ERROR: Remote read failed."
    echo "${0}:${LINENO} Begin ${SERVER_LOG}"
    tail -n 30 "${SERVER_LOG}"
    echo "${0}:${LINENO} End ${SERVER_LOG}"    
    exit 110
fi


echo "${0}:${LINENO} Testing remote read with passed address . . . "
${PROGRAM_LAUNCHER} "${testbindir}/nml_test_read${EXEEXT}" b1 rr "${tmpdir}/${TESTNAME}_test.nml" 7 use_read_with_args

nmltestreadsts=$?
echo "${0}:${LINENO} nmltestreadsts=${nmltestreadsts}"

if test ${nmltestreadsts} -ne 0 ; then 
    echo "${0}:${LINENO} ERROR: Remote read failed."
    "${tmpdir}/logstat.${TESTNAME}.$$.sh"
    "${tmpdir}/killnt.${TESTNAME}.$$.sh";
    echo "${0}:${LINENO} ERROR: Remote read failed."
    echo "${0}:${LINENO} Begin ${SERVER_LOG}"
    tail -n 30 "${SERVER_LOG}"
    echo "${0}:${LINENO} End ${SERVER_LOG}"    
    exit 110
fi
echo "${0}:${LINENO} remote read test passed."

${KILL_LAUNCHER}

"${tmpdir}/killnt.${TESTNAME}.$$.sh";

sleep 5

"${tmpdir}/logstat.${TESTNAME}.$$.sh";

if test "x${NO_NMLCLEAN}" = "x" ; then

    NMLCLEAN_PROG="${testbindir}/nmlclean${EXEEXT}";
    
    if test -x "${bindir}/nmlclean${EXEEXT}" -a '!' -x "${testbindir}/nmlclean${EXEEXT}" ; then
	NMLCLEAN_PROG="${bindir}/nmlclean${EXEEXT}";
    fi
    
    NMLCLEANCMD="${PROGRAM_LAUNCHER} ${NMLCLEAN_PROG} ${tmpdir}/${TESTNAME}_test.nml";
    echo NMLCLEANCMD=${NMLCLEANCMD}

    ${PROGRAM_LAUNCHER} "${NMLCLEAN_PROG}" "${tmpdir}/${TESTNAME}_test.nml";
      
    nmlcleansts=$?

    echo "${0}:${LINENO} nmlcleansts=${nmlcleansts}"

fi

"${tmpdir}/logstat.${TESTNAME}.$$.sh";


if test "x${NML_TEST_QUEUE}" != "xno" ; then
    
sleep 5
"${PS_GREP_I}" nml_te

SERVER_LOG="${tmpdir}/${TESTNAME}_test_server5.log";

echo "${0}:${LINENO} Starting server${EXEEXT} . . ."
if test -f nml_test_server.running ; then \rm -f nml_test_server.running; fi
( ( set -x; ${PROGRAM_LAUNCHER} "${testbindir}/nml_test_server${EXEEXT}" qb b1s "${tmpdir}/${TESTNAME}_test.nml" 2>&1 ) | tee "${SERVER_LOG}" ) &
sleep ${TESTNML_SLEEP};
tries=0;
while true ; do
    echo "${0}:${LINENO} waiting for nml_test_server..."
    sleep ${TESTNML_SLEEP};
    let tries++;
    if test ${tries} -gt 40 ; then
	echo "${0}:${LINENO}  timedout.";
	exit 2;
    fi
    echo -n ".";

    if test -f "nml_test_server.running" ; then
	echo "${0}:${LINENO}  done.";
	break;
    fi
done;
sleep ${TESTNML_SLEEP}


"${tmpdir}/logstat.${TESTNAME}.$$.sh";

sleep ${TESTNML_SLEEP}

if test "x${NO_SINGLE_PROCESS_QUEUE_TEST}" = "x" ; then

    echo "${0}:${LINENO} Testing remote single process queue tests . . . "
    \rm -f "${testbindir}/ntspqt_rw.${TESTNAME}.$$.log";
    ( ${PROGRAM_LAUNCHER} "${testbindir}/nml_test_single_process_queue_test${EXEEXT}" qb rw "${tmpdir}/${TESTNAME}_test.nml" 99 100 2>&1) >"${testbindir}/ntspqt_rw.${TESTNAME}.$$.log";
    nmltestsingleprocessqueuests=$?
    echo "${0}:${LINENO} nmlsingleprocessqueuests=${nmltestsingleprocessqueuests}"
    ls -l "${testbindir}/ntspqt_rw.${TESTNAME}.$$.log";
    
    if test ${nmltestsingleprocessqueuests} -ne 0 ; then 
	echo "${0}:${LINENO} ERROR: Remote single process queue test failed."
	echo "${0}:${LINENO} ERROR: Remote single process queue test failed. (2)" >&2
	"${tmpdir}/logstat.${TESTNAME}.$$.sh"
	"${tmpdir}/killnt.${TESTNAME}.$$.sh";
	echo "${0}:${LINENO} ERROR: Remote single process test failed."
	echo "${0}:${LINENO} ERROR: Remote single process test failed. (2)" >&2
	echo "${0}:${LINENO} ERROR: BEGIN -> ${testbindir}/ntspqt_rw.${TESTNAME}.$$.log"
	tail -n 30  "${testbindir}/ntspqt_rw.${TESTNAME}.$$.log"
	echo "${0}:${LINENO} ERROR: END <- ${testbindir}/ntspqt_rw.${TESTNAME}.$$.log"
	echo "${0}:${LINENO} Begin ${SERVER_LOG}"
	tail -n 30 "${SERVER_LOG}"
	echo "${0}:${LINENO} End ${SERVER_LOG}"    
	exit 123
    fi
    echo "${0}:${LINENO} remote single process queue passed."
fi


echo "${0}:${LINENO} Testing remote write to queued buffer . . . "
${PROGRAM_LAUNCHER} "${testbindir}/nml_test_write${EXEEXT}" qb rw "${tmpdir}/${TESTNAME}_test.nml" 99
nmltestwritests=$?
echo "${0}:${LINENO} nmltestwritests=${nmltestwritests}"

if test ${nmltestwritests} -ne 0 ; then 
    echo "${0}:${LINENO} ERROR: Remote write test to queued buffer failed."
    "${tmpdir}/logstat.${TESTNAME}.$$.sh";
    "${tmpdir}/killnt.${TESTNAME}.$$.sh";
    echo "${0}:${LINENO} ERROR: Remote write test to queued buffer failed."
    echo "${0}:${LINENO} Begin ${SERVER_LOG}"
    tail -n 30 "${SERVER_LOG}"
    echo "${0}:${LINENO} End ${SERVER_LOG}"        
    exit 109
fi
echo "${0}:${LINENO} remote write to queued buffer test passed."

echo "${0}:${LINENO} Testing get_queue_length . . . "
${PROGRAM_LAUNCHER} "${testbindir}/nml_test_get_queue_length${EXEEXT}" qb rw "${tmpdir}/${TESTNAME}_test.nml" 1;
nmltestgetqueuelengthsts=$?
echo "${0}:${LINENO} nmltestgetqueuelengthsts=${nmltestgetqueuelengthsts}"
if test ${nmltestgetqueuelengthsts} -ne 0 ; then 
    echo "${0}:${LINENO} ERROR: Remote get_queue_length failed."
    "${tmpdir}/logstat.${TESTNAME}.$$.sh";
    "${tmpdir}/killnt.${TESTNAME}.$$.sh";
    echo "${0}:${LINENO} ERROR: Remote get_queue_length failed."
    echo "${0}:${LINENO} Begin ${SERVER_LOG}"
    tail -n 30 "${SERVER_LOG}"
    echo "${0}:${LINENO} End ${SERVER_LOG}"        
    exit 108
fi

${PROGRAM_LAUNCHER} "${testbindir}/nml_test_write${EXEEXT}" qb rw "${tmpdir}/${TESTNAME}_test.nml"  98
nmltestwritests=$?
echo "${0}:${LINENO} nmltestwritests=${nmltestwritests}"

if test ${nmltestwritests} -ne 0 ; then 
    echo "${0}:${LINENO} ERROR: Remote write test to queued buffer failed."
    "${tmpdir}/logstat.${TESTNAME}.$$.sh";
    "${tmpdir}/killnt.${TESTNAME}.$$.sh";
    echo "${0}:${LINENO} ERROR: Remote write test to queued buffer failed."
    echo "${0}:${LINENO} Begin ${SERVER_LOG}"
    tail -n 30 "${SERVER_LOG}"
    echo "${0}:${LINENO} End ${SERVER_LOG}"
    exit 121
fi
echo "${0}:${LINENO} remote write to queued buffer test passed."
${PROGRAM_LAUNCHER} "${testbindir}/nml_test_write${EXEEXT}" qb rw "${tmpdir}/${TESTNAME}_test.nml" 97
nmltestwritests=$?
echo "${0}:${LINENO} nmltestwritests=${nmltestwritests}"

if test ${nmltestwritests} -ne 0 ; then 
    echo "${0}:${LINENO} ERROR: Remote write test to queued buffer failed."
    "${tmpdir}/logstat.${TESTNAME}.$$.sh";
    "${tmpdir}/killnt.${TESTNAME}.$$.sh";
    echo "${0}:${LINENO} ERROR: Remote write test to queued buffer failed."
    echo "${0}:${LINENO} Begin ${SERVER_LOG}"
    tail -n 30 "${SERVER_LOG}"
    echo "${0}:${LINENO} End ${SERVER_LOG}"
    exit 107
fi
echo "${0}:${LINENO} remote write to queued buffer test passed."

echo "${0}:${LINENO} Testing get_queue_length . . . "
${PROGRAM_LAUNCHER} "${testbindir}/nml_test_get_queue_length${EXEEXT}" qb rw "${tmpdir}/${TESTNAME}_test.nml" 3;
nmltestgetqueuelengthsts=$?
echo "${0}:${LINENO} nmltestgetqueuelengthsts=${nmltestgetqueuelengthsts}"

sleep 5

echo "${0}:${LINENO} Testing remote read from queued buffer. . . "
${PROGRAM_LAUNCHER}  "${testbindir}/nml_test_read${EXEEXT}" qb rr "${tmpdir}/${TESTNAME}_test.nml" 99
nmltestreadsts=$?
echo "${0}:${LINENO} nmltestreadsts=${nmltestreadsts}"
if test ${nmltestreadsts} -ne 0 ; then 
    echo "${0}:${LINENO} ERROR: Remote read test from queued buffer failed."
    "${tmpdir}/logstat.${TESTNAME}.$$.sh";
    "${tmpdir}/killnt.${TESTNAME}.$$.sh"
    echo "${0}:${LINENO} ERROR: Remote read test from queued buffer failed."
    echo "${0}:${LINENO} Begin ${SERVER_LOG}"
    tail -n 30 "${SERVER_LOG}"
    echo "${0}:${LINENO} End ${SERVER_LOG}"
    exit 106
fi

${PROGRAM_LAUNCHER}  "${testbindir}/nml_test_read${EXEEXT}" qb rr "${tmpdir}/${TESTNAME}_test.nml" 98
nmltestreadsts=$?
echo "${0}:${LINENO} nmltestreadsts=${nmltestreadsts}"
if test ${nmltestreadsts} -ne 0 ; then 
    echo "${0}:${LINENO} ERROR: Remote read test from queued buffer failed."
    "${tmpdir}/logstat.${TESTNAME}.$$.sh";
    "${tmpdir}/killnt.${TESTNAME}.$$.sh";
    echo "${0}:${LINENO} ERROR: Remote read test from queued buffer failed."
    echo "${0}:${LINENO} Begin ${SERVER_LOG}"
    tail -n 30 "${SERVER_LOG}"
    echo "${0}:${LINENO} End ${SERVER_LOG}"
    exit 105
fi

${PROGRAM_LAUNCHER}  "${testbindir}/nml_test_read${EXEEXT}" qb rr "${tmpdir}/${TESTNAME}_test.nml" 97
nmltestreadsts=$?
echo "${0}:${LINENO} nmltestreadsts=${nmltestreadsts}"
if test ${nmltestreadsts} -ne 0 ; then 
    echo "${0}:${LINENO} ERROR: Remote read test from queued buffer failed."
    "${tmpdir}/logstat.${TESTNAME}.$$.sh";
    "${tmpdir}/killnt.${TESTNAME}.$$.sh";
    echo "${0}:${LINENO} ERROR: Remote read test from queued buffer failed."
    echo "${0}:${LINENO} Begin ${SERVER_LOG}"
    tail -n 30 "${SERVER_LOG}"
    echo "${0}:${LINENO} End ${SERVER_LOG}"
    exit 104
fi

echo "${0}:${LINENO} remote read from queued buffer test passed."

echo "${0}:${LINENO} Testing get_queue_length . . . "
${PROGRAM_LAUNCHER} "${testbindir}/nml_test_get_queue_length${EXEEXT}" qb rw "${tmpdir}/${TESTNAME}_test.nml" 0;
nmltestgetqueuelengthsts=$?
echo "${0}:${LINENO} nmltestgetqueuelengthsts=${nmltestgetqueuelengthsts}"

if test ${nmltestgetqueuelengthsts} -ne 0 ; then 
    echo "${0}:${LINENO} ERROR: Remote get_queue_length failed."
    "${tmpdir}/logstat.${TESTNAME}.$$.sh";
    "${tmpdir}/killnt.${TESTNAME}.$$.sh";
    echo "${0}:${LINENO} ERROR: Remote get_queue_length failed."
    echo "${0}:${LINENO} Begin ${SERVER_LOG}"
    tail -n 30 "${SERVER_LOG}"
    echo "${0}:${LINENO} End ${SERVER_LOG}"
    exit 104
fi

echo "${0}:${LINENO} remote get_queue_length passed."

"${tmpdir}/killnt.${TESTNAME}.$$.sh";

${KILL_LAUNCHER}
sleep 5
${KILL_LAUNCHER}

sleep 5
"${tmpdir}/logstat.${TESTNAME}.$$.sh";

if test "x${NO_NMLCLEAN}" = "x" ; then

    NMLCLEAN_PROG="${testbindir}/nmlclean${EXEEXT}";

    if test -x "${bindir}/nmlclean${EXEEXT}" -a '!' -x "${testbindir}/nmlclean${EXEEXT}" ; then
	NMLCLEAN_PROG="${bindir}/nmlclean${EXEEXT}";
    fi
    
    NMLCLEANCMD="${PROGRAM_LAUNCHER} ${NMLCLEAN_PROG} ${tmpdir}/${TESTNAME}_test.nml"
    echo NMLCLEANCMD=${NMLCLEANCMD}
    
    ${PROGRAM_LAUNCHER} "${NMLCLEAN_PROG}" "${tmpdir}/${TESTNAME}_test.nml"
    
    nmlcleansts=$?
    
    echo "${0}:${LINENO} nmlcleansts=${nmlcleansts}"
fi

"${tmpdir}/logstat.${TESTNAME}.$$.sh";

# test "x${NML_TEST_QUEUE}" != "xno"
fi

"${PS_GREP_I}" nml_te;

SERVER_LOG="${tmpdir}/${TESTNAME}_test_server6.log"
echo "${0}:${LINENO} Starting server${EXEEXT} . . ."
if test -f nml_test_server.running ; then \rm -f nml_test_server.running; fi
( ( ${PROGRAM_LAUNCHER} "${testbindir}/nml_test_server${EXEEXT}" b2 b2s "${tmpdir}/${TESTNAME}_test.nml"  2>&1 ) | tee "${SERVER_LOG}" ) &
sleep ${TESTNML_SLEEP};
tries=0;
while true ; do
    echo "${0}:${LINENO} waiting for nml_test_server..."
    sleep ${TESTNML_SLEEP};
    let tries++;
    if test ${tries} -gt 40 ; then
	echo "${0}:${LINENO}  timedout.";
	exit 2;
    fi
    echo -n ".";

    if test -f "nml_test_server.running" ; then
	echo "${0}:${LINENO}  done.";
	break;
    fi
done;
sleep ${TESTNML_SLEEP}

"${tmpdir}/logstat.${TESTNAME}.$$.sh";

sleep ${TESTNML_SLEEP}

echo NML_TEST_BLOCKING="${NML_TEST_BLOCKING}"
echo NML_TEST_REMOTE="${NML_TEST_REMOTE}"

if test "x${NML_TEST_BLOCKING}" != "xno" ; then

( sleep 5 ; ${PROGRAM_LAUNCHER} "${testbindir}/nml_test_write${EXEEXT}" b2 rw "${tmpdir}/${TESTNAME}_test.nml" 8 ; \
    ${PROGRAM_LAUNCHER} "${testbindir}/nml_test_blocking_read${EXEEXT}" b2 lr "${tmpdir}/${TESTNAME}_test.nml" 30 8 ) &

echo "${0}:${LINENO} Testing remote blocking read . . . "
${PROGRAM_LAUNCHER} "${testbindir}/nml_test_blocking_read${EXEEXT}" b2 lr "${tmpdir}/${TESTNAME}_test.nml" 30 8 &
${PROGRAM_LAUNCHER} "${testbindir}/nml_test_blocking_read${EXEEXT}" b2 rr "${tmpdir}/${TESTNAME}_test.nml" 30 8 &
${PROGRAM_LAUNCHER} "${testbindir}/nml_test_blocking_read${EXEEXT}" b2 rr "${tmpdir}/${TESTNAME}_test.nml" 30 8 &
sleep ${TESTNML_SLEEP}
${PROGRAM_LAUNCHER} "${testbindir}/nml_test_blocking_read${EXEEXT}" b2 lr "${tmpdir}/${TESTNAME}_test.nml" 30 8 &
date
${PROGRAM_LAUNCHER} "${testbindir}/nml_test_blocking_read${EXEEXT}" b2 rr "${tmpdir}/${TESTNAME}_test.nml" -1 8

nmltestblockingreadsts=$?
echo "${0}:${LINENO} nmltestblockingreadsts=${nmltestblockingreadsts}"

if test ${nmltestblockingreadsts} -ne 0 ; then 
    echo "${0}:${LINENO} ERROR: Remote blocking read test failed."
    echo "${0}:${LINENO} ERROR: Remote blocking read test failed. (2)" >&2
    "${tmpdir}/logstat.${TESTNAME}.$$.sh";
    "${tmpdir}/killnt.${TESTNAME}.$$.sh";
    echo "${0}:${LINENO} ERROR: Remote blocking read test failed."
    echo "${0}:${LINENO} ERROR: Remote blocking read test failed. (2)" >&2
    echo "${0}:${LINENO} Begin ${SERVER_LOG}"
    tail -n 30 "${SERVER_LOG}"
    echo "${0}:${LINENO} End ${SERVER_LOG}"
    exit 103
fi
date
echo "${0}:${LINENO} remote read blocking test passed."

## "x${NML_TEST_BLOCKING}" != "xno"
fi


## "x${NML_TEST_REMOTE}" != "xno" 
fi

echo "${0}:${LINENO} Kill any nml test programs still running."
"${tmpdir}/killnt.${TESTNAME}.$$.sh";

${KILL_LAUNCHER}

if test "x${NO_NMLCLEAN}" = "x" ; then

    ${PROGRAM_LAUNCHER} "${bindir}/nmlclean${EXEEXT}" "${tmpdir}/${TESTNAME}_test.nml";
    if test -f nmlclean${EXEEXT}.log ; then
	mv nmlclean${EXEEXT}.log "${tmpdir}/nmlclean${EXEEXT}-1-$$.log";
    fi
fi

if test -x /usr/bin/killall -a -x /usr/X11R6/bin/xterm ; then 
    killall "${TESTNAME}taillog.sh"
fi

echo "${0}:${LINENO} End of $0 $* from  ${PWD}" 
echo "${0}:${LINENO} End of $0 $* from  ${PWD}"  >&2


exit 0
