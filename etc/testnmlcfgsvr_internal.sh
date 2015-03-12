#! /bin/sh

set -x

echo "${0}:${LINENO} Starting $0 $* from  ${PWD}"
echo "${0}:${LINENO} Starting $0 $* from  ${PWD} (2)" >&2

if test "x${USER}" = "xroot" -a "x${RUN_CHECK_AS_ROOT}" != "xy" ; then
    echo "${0}:${LINENO} This program is unsafe to run as root.\n"
    echo "${0}:${LINENO} This program is unsafe to run as root. (2)\n" >&2
    exit 1
fi

WHOAMI_RET=`whoami >/dev/null 2>/dev/null`;

if test "x${WHOAMI_RET}" = "xroot" -a "x${RUN_CHECK_AS_ROOT}" != "xy" ; then
    echo "${0}:${LINENO} This program is unsafe to run as root.\n"
    echo "${0}:${LINENO} This program is unsafe to run as root. (2)\n" >&2
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

if test "x${tmpdir}" = "x" ; then
    tmpdir=/tmp;
fi


cat >"${tmpdir}/psgrepi.sh" <<EOF
#! /bin/sh
ps | grep -v internal | grep -i \${1}
ps -o pid,command  | grep -v internal | grep -i \${1}
EOF

chmod a+x "${tmpdir}/psgrepi.sh"

# PS_GREP_I should be set to something that can be used to produce a list
# of all processes created by running a certain program with a given string
# in the name.
# Cygwin is the only system I know of where the -i is really needed 
# (because the ps under cygwin does not preserve case ) 
if test "x${PS_GREP_I}" = "x" ; then
    PS_GREP_I="${tmpdir}/psgrepi.sh"
fi

if test -f "${tmpdir}/${TESTNAME}_test.nml" ; then
    \rm -f "${TESTNAME}_test.nml";
fi

if test -x /usr/X11R6/bin/xterm ; then 

cat >"${tmpdir}/${TESTNAME}taillog.sh" <<EOF
#!/bin/sh
tail -f "${tmpdir}/${TESTNAME}testnml.log"
EOF
chmod a+x "${tmpdir}/${TESTNAME}taillog.sh"

xterm -sl 1000 -sb -e "${tmpdir}/${TESTNAME}taillog.sh" &

fi

echo \$0=$0
echo \$\$=$$
echo \$PPID=\$PPID
 
ps | grep $$
ps -ael | grep $$
ps -ael | grep $PPID

ps -eo pid,cmd | grep $$
ps -eo pid,cmd | grep $PPID

# This make the rcslib slow and verbose
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


if test -f "${tmpdir}/${TESTNAME}_test.nml" ; then
    "${builddir}/nmlclean${EXEEXT}" "${tmpdir}/${TESTNAME}_test.nml";
    if test -f "nmlclean${EXEEXT}.log" ; then
	mv "nmlclean${EXEEXT}.log" "${tmpdir}/nmlclean${EXEEXT}-1-$$.log";
    fi
    \rm -f "${tmpdir}/${TESTNAME}_test.nml";
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

if test '!' -x "${builddir}/nml_test_server${EXEEXT}" -a '!' -x "${builddir}/nml_test_read${EXEEXT}" -a '!' -x "${builddir}/nml_test_write${EXEEXT}" -a '!' -x "${builddir}/nml_test_nmlset${EXEEXT}" -a '!' -x "${builddir}/nml_test_blocking_read${EXEEXT}" ; then

    if test -x "${builddir}/.lastbuild/nml_test_server${EXEEXT}" -a  -x "${builddir}/.lastbuild/nml_test_read${EXEEXT}" -a  -x "${builddir}/.lastbuild/nml_test_write${EXEEXT}" -a  -x "${builddir}/.lastbuild/nml_test_nmlset${EXEEXT}" -a  -x "${builddir}/.lastbuild/nml_test_blocking_read${EXEEXT}" ; then
	testbindir="${builddir}/.lastbuild";
    else
	if test -x "${builddir}/bin/nml_test_server${EXEEXT}" -a  -x "${builddir}/bin/nml_test_read${EXEEXT}" -a  -x "${builddir}/bin/nml_test_write${EXEEXT}" -a  -x "${builddir}/bin/nml_test_nmlset${EXEEXT}" -a  -x "${builddir}/bin/nml_test_blocking_read${EXEEXT}" ; then
	    testbindir="${builddir}/bin";
	else
	    testbindir="${builddir}";
	fi
    fi
else
    testbindir="${builddir}"
fi

echo "${0}:${LINENO} ______________________________________"
echo "${0}:${LINENO} Starting NMLCFGSVR TESTS:"

ls -l 
ls -l >&2

echo "${0}:${LINENO} Current directory:"
pwd
pwd >&2

(rm "${tmpdir}/killnt.${TESTNAME}.*.sh" ) >/dev/null 2>/dev/null 

LAUNCHER_TO_KILL=
if test "x${PROGRAM_LAUNCHER}" != "x" ; then
    LAUNCHER_TO_KILL=`echo ${PROGRAM_LAUNCHER} | sed 's/ --//'`
fi

idlist="10001 10002 10003 10004 20001 20002 20003 20004 30000 30001 30002 30003 30004 30005 30006 30007 30008 40000 40001 40002 40003 40004 40005 40006 40007 40008" 

cat >"${tmpdir}/killnt.${TESTNAME}.$$.sh" <<EOF
#!/bin/sh
echo Running ${tmpdir}/killnt.${TESTNAME}.$$.sh from $0 $*
date_string=`date +%Y%m%d%H%M%S_%N`;
    ( ( 
    set -x
    sleep 1
    ps -ae
    ps
    echo "${0}:${LINENO} Kill any nml test programs still running."
    ${PS_GREP_I} nml_te | grep -v grep 
    procstokill=\`${PS_GREP_I} nml_te | grep -v grep  | awk '{printf(" %s",\$1);}'\`
    while test "x\${procstokill}" != "x" ; do
	sleep 1
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
    sleep 1
    if test "x${LAUNCHER_TO_KILL}" != "x" ; then 
    ps -ae
    ps
    ${PS_GREP_I} ${LAUNCHER_TO_KILL} | grep -v grep 
    procstokill=\`${PS_GREP_I} ${LAUNCHER_TO_KILL} | grep -v grep  | awk '{printf(" %s",\$1);}'\`
    while test "x\${procstokill}" != "x" ; do
	sleep 1
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
    sleep 1
    ipcs 
    ipcrm  `ipcs -s | gawk '{if(\$1 != "0x00000000" && \$1 != "") printf("-s %s\n",\$2);}' | grep -vi sh | grep -vi si | grep -vi se`
ipcrm  `ipcs -m | gawk '{if(\$1 != "0x00000000" && \$1 != "") printf("-m %s\n",\$2);}' | grep -vi sh | grep -vi si | grep -vi se`
    ipcrm -M \${id}
    ipcrm -S \${id}
    ipcrm -m \${id}
    ipcrm -s \${id}
   
    if test -x ${builddir}/posix_shm_unlink ; then
        for id in ${idlist} ; do
	    ${builddir}/posix_shm_unlink /_\${id}.shm
        done
    fi

    if test -x ${builddir}/posix_sem_unlink ; then
        for id in ${idlist} ; do
	    ${builddir}/posix_sem_unlink /_\${id}.sem
        done
    fi

    if test -x \${builddir}/posix_shm_unlink ; then
        for id in ${idlist} ; do
	    \${builddir}/posix_shm_unlink /_\${id}.shm
        done
    fi

    if test -x \${builddir}/posix_sem_unlink ; then
        for id in ${idlist} ; do
	    \${builddir}/posix_sem_unlink /_\${id}.sem
        done
    fi

    if test -x ${testbindir}/posix_shm_unlink ; then
        for id in ${idlist} ; do
	    ${testbindir}/posix_shm_unlink /_\${id}.shm
        done
    fi

    if test -x ${testbindir}/posix_sem_unlink ; then
        for id in ${idlist} ; do
	    ${testbindir}/posix_sem_unlink /_\${id}.sem
        done
    fi

    if test -x \${testbindir}/posix_shm_unlink ; then
        for id in ${idlist} ; do
	    \${testbindir}/posix_shm_unlink /_\${id}.shm
        done
    fi

    if test -x \${testbindir}/posix_sem_unlink ; then
        for id in ${idlist} ; do
	    \${testbindir}/posix_sem_unlink /_\${id}.sem
        done
    fi

    if test -x ${bindir}/posix_shm_unlink ; then
        for id in ${idlist} ; do
	    ${bindir}/posix_shm_unlink /_\${id}.shm
        done
    fi

    if test -x ${bindir}/posix_sem_unlink ; then
        for id in ${idlist} ; do
	    ${bindir}/posix_sem_unlink /_\${id}.sem
        done
    fi

    if test -x \${bindir}/posix_shm_unlink ; then
        for id in ${idlist} ; do
	    \${bindir}/posix_shm_unlink /_\${id}.shm
        done
    fi

    if test -x \${bindir}/posix_sem_unlink ; then
        for id in ${idlist} ; do
	    \${bindir}/posix_sem_unlink /_\${id}.sem
        done
    fi
    killall -INT nmlcfgsvr
    killall -INT lt-nmlcfgsvr
    killall -INT memcheck
    sleep 3
    killall -KILL nmlcfgsvr
    killall -KILL lt-nmlcfgsvr
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
EOF


chmod a+x "${tmpdir}/killnt.${TESTNAME}.$$.sh";

ls -l "${tmpdir}/killnt.${TESTNAME}.$$.sh";

cat "${tmpdir}/killnt.${TESTNAME}.$$.sh";

"${tmpdir}/killnt.${TESTNAME}.$$.sh";


echo PROGRAM_LAUNCHER="${PROGRAM_LAUNCHER}"
echo EXEEXT="${EXEEXT}"
echo KILL_LAUNCHER="${KILL_LAUNCHER}"

sleep 1

echo "${0}:${LINENO} Launching nmlcfgsvr${EXEEXT}  . . ."

${KILL_LAUNCHER}

sleep 1

NMLCFGSVR_PROG="${testbindir}/nmlcfgsvr${EXEEXT}";

if test -x "${bindir}/nmlcfgsvr${EXEEXT}" \
    -a '!' -x "${testbindir}/nmlcfgsvr${EXEEXT}" ; then
    NMLCFGSVR_PROG="${bindir}/nmlcfgsvr${EXEEXT}";
fi


\rm -rf nmlcfgsvr*.log nmlcfgsvr*.err ${tmpdir}/nmlcfgsvr*.log ${tmpdir}/nmlcfgsvr*.err

${PROGRAM_LAUNCHER} "${NMLCFGSVR_PROG}" --startkey 30000 --debug >${tmpdir}/nmlcfgsvr.log 2>${tmpdir}/nmlcfgsvr.err  &

sleep 5

${PROGRAM_LAUNCHER} "${NMLCFGSVR_PROG}" --port 54545 --startkey 40000 --debug >${tmpdir}/nmlcfgsvr2.log 2>${tmpdir}/nmlcfgsvr2.err &

sleep 5


( ps -ae || true )
( ps || true )
( ipcs || true )
( netstat -naptee && netstat -napuee || netstat -na || true)

NML_TEST_WRITE_PROG="${testbindir}/nml_test_write${EXEEXT}";

if test -x "${bindir}/nml_test_write${EXEEXT}" \
    -a '!' -x "${testbindir}/nml_test_write${EXEEXT}" ; then
    NML_TEST_WRITE_PROG="${bindir}/nml_test_write${EXEEXT}";
fi

NMLCFGSVR_DOMAIN="~newnmldomain~"
export NMLCFGSVR_DOMAIN

${PROGRAM_LAUNCHER} "${NML_TEST_WRITE_PROG}" b1 ntw1 nmlcfgsvr/ 99 50 5.0 &

sleep 5

( ps -ae || true )
( ps || true )
( ipcs || true )
( netstat -naptee && netstat -napuee || netstat -na || true)

echo "${0}:${LINENO} NMLCFGSVR_DOMAIN=${NMLCFGSVR_DOMAIN}"

NMLCFGSVR_DOMAIN="~newnmldomain~"
export NMLCFGSVR_DOMAIN

${PROGRAM_LAUNCHER} "${NML_TEST_WRITE_PROG}" b2 ntw1 nmlcfgsvr//54545 98 50 5.0 &

sleep 5

( ps -ae || true )
( ps || true )
( ipcs || true )
( netstat -naptee && netstat -napuee || netstat -na || true)


NMLCFGSVR_DOMAIN="ND5"
export NMLCFGSVR_DOMAIN

${PROGRAM_LAUNCHER} "${NML_TEST_WRITE_PROG}" b1 ntw3 nmlcfgsvr/ 97 50 5.0 &

( ps -ae || true )
( ps || true )
( ipcs || true )
( netstat -naptee && netstat -napuee || netstat -na || true)

NML_TEST_NMLSET_PROG="${testbindir}/nml_test_nmlset${EXEEXT}";

if test -x "${bindir}/nml_test_nmlset${EXEEXT}" \
    -a '!' -x "${testbindir}/nml_test_nmlset${EXEEXT}" ; then
    NML_TEST_NMLSET_PROG="${bindir}/nml_test_nmlset${EXEEXT}";
fi

${PROGRAM_LAUNCHER} "${NML_TEST_NMLSET_PROG}" 'b1;b2' ntw1 'nmlcfgsvr/;nmlcfgsvr//54545' 3 50
nmlsetsts=$?

echo "${0}:${LINENO} nmlsetsts=${nmlsetsts}"

echo "${0}:${LINENO} Kill any nml test programs still running."
"${tmpdir}/killnt.${TESTNAME}.$$.sh";

${KILL_LAUNCHER}

NMLCLEAN_LOCAL_ONLY=1;
export NMLCLEAN_LOCAL_ONLY;

${NMLCLEANCMD}

${PROGRAM_LAUNCHER} "${bindir}/nmlclean${EXEEXT}" "${tmpdir}/${TESTNAME}_test.nml";
if test -f "nmlclean${EXEEXT}.log" ; then
    mv "nmlclean${EXEEXT}.log" "${tmpdir}/nmlclean${EXEEXT}-1-$$.log"
fi

if test -x /usr/bin/killall -a -x /usr/X11R6/bin/xterm ; then 
    killall "${TESTNAME}taillog.sh"
fi

ls -l nmlcfgsvr*.log nmlcfgsvr*.err

echo "${0}:${LINENO} End of $0 $* from  ${PWD}" 
echo "${0}:${LINENO} End of $0 $* from  ${PWD} (2)"  >&2

exit ${nmlsetsts}
