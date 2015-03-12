#! /bin/sh

set -x

if test "x${NML_SET_TO_SERVER}" != "x" ; then
    NML_SET_TO_SERVER=
fi

export_n_out=`(export -n NML_SET_TO_SERVER 2>/dev/null && echo OK) | tail -n 1`

if test "x${export_n_out}" = "xOK" ; then
    export -n NML_SET_TO_SERVER >/dev/null 2>/dev/null
fi

if test "x${CONFIG_NML}" != "x" ; then
    CONFIG_NML=
fi

export_n_out=`(export -n CONFIG_NML 2>/dev/null && echo OK) | tail -n 1`

if test "x${export_n_out}" = "xOK" ; then
    export -n CONFIG_NML >/dev/null 2>/dev/null
fi


if test "x${tmpdir}" = "x" ; then
    tmpdir="/tmp/${USER}/";
    (mkdir -p ${tmpdir} >/dev/null 2>/dev/null || true );
    export tmpdir;
fi


unset NML_SET_TO_SERVER >/dev/null 2>/dev/null

unalias ls >/dev/null 2>/dev/null
unalias rm >/dev/null 2>/dev/null
unalias mv >/dev/null 2>/dev/null
unalias cp >/dev/null 2>/dev/null
unalias cat >/dev/null 2>/dev/null
unalias cd >/dev/null 2>/dev/null

#DEBUG_RCSLIB=1
#export DEBUG_RCSLIB

if test "x${MAKEFAGS}" != "x" ; then
    MAKEFLAGS=
fi

if test "x${MAKELEVEL}" != "x" ; then
    MAKELEVEL=
fi

if test "x${MAKEOVERRIDES}" != "x" ; then
    MAKEOVERRIDES=
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

# This make the rcslib slow and verbose
#RCS_PRINT_FLAGS=0xFFFFFFFF;
#export RCS_PRINT_FLAGS

if test "x${HOME}" = "x" ; then
    HOME=~
fi

if test '!' -d ${tmpdir} ; then
    mkdir ${tmpdir}
fi

echo "Test Date:"
date
echo "Current directory:"
pwd
echo "SET"
set
echo "PRINTENV"
printenv

builddir=`pwd`

NMLCLEAN_LOCAL_ONLY=1;
export NMLCLEAN_LOCAL_ONLY;


if test '!' -x "${builddir}/nmlclean${EXEEXT}" -a '!' -x "${builddir}/nmlcfg${EXEEXT}" -a '!' -x "${builddir}/nml_test_server${EXEEXT}" ; then
    if test -x './=build/nmlclean${EXEEXT}' -a -x './=build/nmlcfg${EXEEXT}' -a -x './=build/nml_test_server${EXEEXT}' ; then
	builddir=`pwd`'/=build';
	echo "build directory set to:"
	echo ${builddir}
    fi
fi

if test '!' -x "${builddir}/nmlclean${EXEEXT}" -a '!' -x "${builddir}/nmlcfg${EXEEXT}" -a '!' -x "${builddir}/nml_test_server${EXEEXT}" ; then

    if test "x${srcdir}" != "x" ; then
	if test ! -d ./etc -a -d "${srcdir}/etc" ; then
	    cd "${srcdir}";
	    echo "Directory changed to:"
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

if test '!' -x "${builddir}/nml_test_server${EXEEXT}" -a '!' -x "${builddir}/nml_test_read${EXEEXT}" -a '!' -x "${builddir}/nml_test_write${EXEEXT}" -a '!' -x "${builddir}/nml_test_blocking_read${EXEEXT}" ; then

    if test -x "${builddir}/.lastbuild/nml_test_server${EXEEXT}" -a  -x "${builddir}/.lastbuild/nml_test_read${EXEEXT}" -a  -x "${builddir}/.lastbuild/nml_test_write${EXEEXT}" -a  -x "${builddir}/.lastbuild/nml_test_blocking_read${EXEEXT}" ; then
	testbindir="${builddir}/.lastbuild";
    else
	if test -x "${builddir}/bin/nml_test_server${EXEEXT}" -a  -x "${builddir}/bin/nml_test_read${EXEEXT}" -a  -x "${builddir}/bin/nml_test_write${EXEEXT}" -a  -x "${builddir}/bin/nml_test_blocking_read${EXEEXT}" ; then
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

echo "______________________________________"
echo "Starting NML TESTS:"

ls -l 
ls -l >&2

echo "Current directory:"
pwd
pwd >&2

(rm "${tmpdir}/killnt.sh" ) >/dev/null 2>/dev/null 

orig_src_dir=`(cd ${srcdir} ; pwd)`
RCSLIB_DIR=${builddir}

if test "x${PLAT}" != "x" ; then
    if test '!' -d ${RCSLIB_DIR}/plat/${PLAT}/lib -a  -f ${builddir}/librcs.la ; then
	RCSLIB_LIB_DIR="${builddir}";
	USE_LIBTOOL=1;
	export USE_LIBTOOL
	export RCSLIB_LIB_DIR
    fi
    if test '!' -d "${RCSLIB_DIR}/plat/${PLAT}/include" \
	-a  -f "${orig_src_dir}/src/rcs.hh"  ; then
	RCSLIB_DIR="${orig_src_dir}";
	USE_SRC_PATH=1;
	export USE_SRC_PATH;
    fi
else
    if test '!' -d "${RCSLIB_DIR}/include" \
	-a  -f "${orig_src_dir}/src/rcs.hh"  ; then
	RCSLIB_DIR=${orig_src_dir};
    fi
    if test '!' -d "${RCSLIB_DIR}/include" \
	-a  -f "${RCSLIB_DIR}/src/rcs.hh"  ; then    
	USE_SRC_PATH=1;
	export USE_SRC_PATH;
    fi
    if test '!' -d "${RCSLIB_DIR}/lib" \
	-a -f "${builddir}/librcs.la" ; then
	RCSLIB_LIB_DIR="${builddir}";
	if test -x "${builddir}/libtool" ; then
	    LIBTOOL="${builddir}/libtool";
	fi
	USE_LIBTOOL=1;
	export USE_LIBTOOL
	export RCSLIB_LIB_DIR
    fi    
fi

if test "x${RCSLIB_BIN_DIR}" = "x" ; then
    if test "x${PLAT}" != "x" \
	-a -d "${RCSLIB_DIR}/plat/${PLAT}/bin" ; then
	RCSLIB_BIN_DIR="${RCSLIB_DIR}/plat/${PLAT}/bin";
    elif test -d "${RCSLIB_DIR}/bin" ; then
	RCSLIB_BIN_DIR="${RCSLIB_DIR}/bin";
    elif test  -d bin ; then
	RCSLIB_BIN_DIR="${PWD}/bin";
    else
	RCSLIB_BIN_DIR="${PWD}";
    fi
fi

if test "x${RCSLIB_LIB_DIR}" = "x" ; then
    if test "x${PLAT}" != "x" \
	-a  -d "${RCSLIB_DIR}/plat/${PLAT}/lib" ; then
	RCSLIB_LIB_DIR="${RCSLIB_DIR}/plat/${PLAT}/lib";
    elif test  -d "${RCSLIB_DIR}/lib" ; then
	RCSLIB_LIB_DIR="${RCSLIB_DIR}/lib";
    elif test -d lib; then
	RCSLIB_LIB_DIR="${PWD}/lib";
    else
	RCSLIB_LIB_DIR="${PWD}";
    fi
fi

export RCSLIB_LIB_DIR
export RCSLIB_BIN_DIR

LD_LIBRARY_PATH="${RCSLIB_LIB_DIR}:${RCSLIB_LIB_DIR}/.libs:${LD_LIBRARY_PATH}";
export LD_LIBRARY_PATH

PATH="${RCSLIB_BIN_DIR}:${PATH}";

export PATH


cat >"${tmpdir}/killnt.sh" <<EOF
#!/bin/sh
    set -x
    sleep 1
    echo "Kill any nml test programs still running."
    ps | grep nml_tes | grep -v grep
    procstokill=\`ps | grep nml_tes | grep -v grep | awk '{printf(" %s",\$1);}'\`
    while test "x\${procstokill}" != "x" ; do
	sleep 1
        ps | grep nml_tes | grep -v grep

	if test x != "x\${procstokill}" ; then
	    kill -INT \$procstokill
	    sleep 2
	    procstokill=\`ps | grep nml_tes | grep -v grep | awk '{printf(" %s",\$1);}'\`
	fi

	if test x != "x\${procstokill}" ; then
	    kill -INT \$procstokill
	    sleep 2
	    procstokill=\`ps | grep nml_tes | grep -v grep | awk '{printf(" %s",\$1);}'\`
	fi

	if test x != "x\${procstokill}" ; then
	    kill -TERM \$procstokill
	    sleep 2
	    procstokill=\`ps | grep nml_tes | grep -v grep | awk '{printf(" %s",\$1);}'\`
	fi

	if test x != "x\${procstokill}" ; then
	    kill -TERM \$procstokill
	    sleep 2
	    procstokill=\`ps | grep nml_tes | grep -v grep | awk '{printf(" %s",\$1);}'\`
	fi

	if test x != "x\${procstokill}" ; then
	    kill -KILL \$procstokill
	    sleep 2
	    procstokill=\`ps | grep nml_tes | grep -v grep | awk '{printf(" %s",\$1);}'\`
	fi

	if test x != "x\${procstokill}" ; then
	    kill -KILL \$procstokill
	    sleep 2
	    procstokill=\`ps | grep nml_tes | grep -v grep | awk '{printf(" %s",\$1);}'\`
	fi
    done
    sleep 1
    if test -x "${RCSLIB_BIN_DIR}/posix_shm_unlink" ; then
	"${RCSLIB_BIN_DIR}/posix_shm_unlink" /_101.shm
	"${RCSLIB_BIN_DIR}/posix_shm_unlink" /_201.shm
	"${RCSLIB_BIN_DIR}/posix_shm_unlink" /_102.shm
	"${RCSLIB_BIN_DIR}/posix_shm_unlink" /_202.shm
	"${RCSLIB_BIN_DIR}/posix_shm_unlink" /_301.shm
	"${RCSLIB_BIN_DIR}/posix_shm_unlink" /_302.shm
    fi

    if test -x "${RCSLIB_BIN_DIR}/posix_sem_unlink" ; then
	"${RCSLIB_BIN_DIR}/posix_sem_unlink" /_101.sem
	"${RCSLIB_BIN_DIR}/posix_sem_unlink" /_201.sem
	"${RCSLIB_BIN_DIR}/posix_sem_unlink" /_102.sem
	"${RCSLIB_BIN_DIR}/posix_sem_unlink" /_202.sem
	"${RCSLIB_BIN_DIR}/posix_sem_unlink" /_301.sem
	"${RCSLIB_BIN_DIR}/posix_sem_unlink" /_302.sem
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

EOF

chmod a+x "${tmpdir}/killnt.sh";

ls -l "${tmpdir}/killnt.sh"

cat "${tmpdir}/killnt.sh"

"${tmpdir}/killnt.sh"


echo PROGRAM_LAUNCHER="${PROGRAM_LAUNCHER}"
echo EXEEXT="${EXEEXT}"
echo KILL_LAUNCHER="${KILL_LAUNCHER}"

sleep 1

if test "x${JDK_DIR}" != "x" ; then
    JDK_DIR=`(cd ${JDK_DIR} ; pwd )`;
    export JDK_DIR
fi

if test "x${JDK_DIR}" != "x" \
    -a -f "${JDK_DIR}/bin/java" \
    -a -f "${JDK_DIR}/bin/javac" ; then
    JAVA="${JDK_DIR}/bin/java";
    JAVAC="${JDK_DIR}/bin/javac";
fi

if test "x${JAVA}" = "x" ; then
    if test -d /usr/java ; then
	JAVA=`find /usr/java/ -name java -type f -follow | head -n 1`
    else
	JAVA=java
    fi
fi

if test "x${JAVAC}" = "x" ; then
    if test -d /usr/java ; then
	JAVAC=`find /usr/java/ -name javac -type f -follow | head -n 1`
    else
	JAVAC=javac
    fi
fi

export JAVA
export JAVAC

echo "Testing nmlcfg${EXEEXT}  . . ."

${KILL_LAUNCHER}

echo NMLCFG_OPTIONS="${NMLCFG_OPTIONS}"

NMLCFG_PROG="${testbindir}/nmlcfg${EXEEXT}";

if test -x "${bindir}/nmlcfg${EXEEXT}" \
    -a '!' -x "${testbindir}/nmlcfg${EXEEXT}" ; then
    NMLCFG_PROG="${bindir}/nmlcfg${EXEEXT}";
fi

NMLCFGCMD="${PROGRAM_LAUNCHER} ${NMLCFG_PROG} ${NMLCFG_OPTIONS} ${srcdir}/src/test/test.nml2 -o ${tmpdir}/${TESTNAME}_test.nml"
echo NMLCFGCMD=${NMLCFGCMD}

${PROGRAM_LAUNCHER} "${NMLCFG_PROG}" ${NMLCFG_OPTIONS} "--HEADER_DIR=${srcdir}/src/test/" "${srcdir}/src/test/test.nml2" -o "${tmpdir}/${TESTNAME}_test.nml"


nmlcfgsts=$?

echo "nmlcfgsts=${nmlcfgsts}"

if test ${nmlcfgsts} -ne 0 ; then
    echo "nmlcfg${EXEEXT} test failed."
    exit 254
fi

if test '!' -f "${srcdir}/src/test/test.nml2" ; then
    echo "nmlcfg${EXEEXT} test failed."
    exit 253
fi  

echo "${srcdir}/src/test/"
ls -l "${srcdir}/src/test/"

\rm -f "${tmpdir}/nml_extra_tests.tar";
\rm -rf "${tmpdir}/extra_test_dir/";
mkdir -p "${tmpdir}/extra_test_dir/";
\rm -f "${srcdir}/src/test/nml_extra_tests.tar";


if test -f "${tmpdir}/nml_extra_tests.tar" ; then
    echo "Delete ${tmpdir}/nml_extra_tests.tar"
    echo "Delete ${tmpdir}/nml_extra_tests.tar" >&2
    exit 1;
fi

if test -f "${tmpdir}/extra_test_dir/nml_extra_tests.tar" ; then
    echo "Delete ${tmpdir}/extra_test_dir/nml_extra_tests.tar"
    echo "Delete ${tmpdir}/extra_test_dir/nml_extra_tests.tar" >&2
    exit 1;
fi

if test -f "${srcdir}/src/test/nml_extra_tests.tar" ; then
    echo "Delete ${srcdir}/src/test/nml_extra_tests.tar"
    echo "Delete ${srcdir}/src/test/nml_extra_tests.tar" >&2
    exit 1;
fi


mkdir -p "${tmpdir}/extra_test_dir";

(cd "${tmpdir}/extra_test_dir" ; rm -f *.{c,cc,h,hh,java,o,class,obj,exe} ; ) >/dev/null 2>/dev/null

( \
    cd "${srcdir}/src/test"; \
    tar -cf "${tmpdir}/extra_test_dir/nml_extra_tests.tar" nml_test_format.hh nml_test_unbounded_format*.hh otherheader.hh echo_types.hh nml_test_[a-e,g-z]*.cc nml_get*.cc nml_check_*.cc nml_peek*.cc nml_read*.cc nml_test_[a-e,g-z]*_c.c nml*test*.java Makefile.extra_tests *.nml *.nml2 extra_tests.sh test_qr.sh config.dat number.txt nml_test_server_ada.adb nml_test_dl_read_ada.adb nml_test_dl_write_ada.adb check_test*.cc check_test*.hh etime_print.cc print_test_sizes.cc nml_ex1.hh nml_ex1_n.cc nml_ex1_n_codegen_protos.hh; \
)

if test '!' -f "${tmpdir}/extra_test_dir/nml_extra_tests.tar" ; then
    echo "Could not create tar file";
    exit 252
fi


cd "${tmpdir}/extra_test_dir";
tar -xf nml_extra_tests.tar; 

if test -x "${builddir}/libtool" ; then
    LIBTOOL="${builddir}/libtool";
    export LIBTOOL
    USE_LIBTOOL=1;
    export USE_LIBTOOL
fi


cat >"${HOME}/tailtest.txt" <<EOF
1
2
3
EOF

if test "x${TAIL1}" = "x" ; then 
    if cat "${HOME}/tailtest.txt" | tail -n 1 >/dev/null 2>/dev/null ; then
	if test  3 = `cat ${HOME}/tailtest.txt | tail -n 1 2>/dev/null` ; then
	    TAIL1='tail -n 1'
	fi
    fi
fi

if test "x${TAIL1}" = "x" ; then 
    if cat "${HOME}/tailtest.txt" | tail -1 >/dev/null 2>/dev/null ; then
	if test  3 = `cat ${HOME}/tailtest.txt | tail -1 2>/dev/null` ; then
	    TAIL1='tail -1'
	fi
    fi
fi

if test "x${TAIL1}" = "x" ; then 
    TAIL1='tail -n 1'
fi

\rm -f tailtest.txt >/dev/null 2>/dev/null

LIBS_FROM_CONFIG_LOG=
LDFLAGS_FROM_CONFIG_LOG=
if test -f ${builddir}/config.log ; then
    LIBS_FROM_CONFIG_LOG=`grep LIBS= ${builddir}/config.log | ${TAIL1} | sed s#^LIBS=\'## | sed s#^LIBS=## | sed s#\'## 2>/dev/null`
    if test "x${LIBS_FROM_CONFIG_LOG}" != "x" ; then
	OS_LIBS="${OS_LIBS} ${LIBS_FROM_CONFIG_LOG}"
    fi
    LDFLAGS_FROM_CONFIG_LOG=`grep LDFLAGS= ${builddir}/config.log | ${TAIL1} | sed s#^LDFLAGS=\'## | sed s#^LDFLAGS=## | sed s#\'## 2>/dev/null`           
    if test "x${LIBS_FROM_CONFIG_LOG}" != "x" ; then
	OS_LIBS="${LDFLAGS_FROM_CONFIG_LOG} ${OS_LIBS}"
    fi
    export OS_LIBS
elif test -f ${RCSLIB_DIR}/.lastbuild/config.log ; then
    LIBS_FROM_CONFIG_LOG=`grep LIBS= ${RCSLIB_DIR}/.lastbuild/config.log | ${TAIL1} | sed s#^LIBS=\'## | sed s#^LIBS=## | sed s#\'## 2>/dev/null`
    if test "x${LIBS_FROM_CONFIG_LOG}" != "x" ; then
	OS_LIBS="${OS_LIBS} ${LIBS_FROM_CONFIG_LOG}"
    fi
    LDFLAGS_FROM_CONFIG_LOG=`grep LDFLAGS= ${RCSLIB_DIR}/.lastbuild/config.log | ${TAIL1} | sed s#^LDFLAGS=\'## | sed s#^LDFLAGS=## | sed s#\'## 2>/dev/null`
    if test "x${LIBS_FROM_CONFIG_LOG}" != "x" ; then
	OS_LIBS="${LDFLAGS_FROM_CONFIG_LOG} ${OS_LIBS}"
    fi
    export OS_LIBS
fi

export RCSLIB_DIR;
echo TEST_CFLAGS=${TEST_CFLAGS}
echo TEST_CXXFLAGS=${TEST_CXXFLAGS}


if test "x${TEST_CFLAGS}" != "x" ; then
    CFLAGS="${CFLAGS} ${TEST_CFLAGS}";
    export CFLAGS;
fi

if echo "x${CFLAGS}x" | grep -- "-Werror" ; then
    CFLAGS=`echo ${CFLAGS} | sed s#-Werror##`
    export CFLAGS;
fi

if test "x${TEST_CXXFLAGS}" != "x" ; then
    CXXFLAGS="${CXXFLAGS} ${TEST_CXXFLAGS}";
fi

if echo "x${CXXFLAGS}x" | grep -- "-Werror" ; then
    CXXFLAGS=`echo ${CXXFLAGS} | sed s#-Werror##`;
    export CXXFLAGS
fi

echo CFLAGS=${CFLAGS}
echo CXXFLAGS=${CXXFLAGS}

if test "x${CODEGEN_JAR}" = "x" -a  "x${CODEGEN_COMMAND}" = "x" ; then
    
     if test -f "${testbindir}/CodeGenCmdLine.jar" ; then
	CODEGEN_JAR="${testbindir}/CodeGenCmdLine.jar";
     elif test -f "${bindir}/CodeGenCmdLine.jar" ; then
	CODEGEN_JAR="${bindir}/CodeGenCmdLine.jar";
     elif test -f "${builddir}/CodeGenCmdLine.jar" ; then
	CODEGEN_JAR="${builddir}/CodeGenCmdLine.jar";
     elif test -f "${builddir}/plat/java/lib/CodeGenCmdLine.jar" ; then
	CODEGEN_JAR="${builddir}/plat/java/lib/CodeGenCmdLine.jar";
     elif test -f "${RCSLIB_DIR}/plat/java/lib/CodeGenCmdLine.jar" ; then
	CODEGEN_JAR="${RCSLIB_DIR}/plat/java/lib/CodeGenCmdLine.jar";
     elif test -f "${orig_src_dir}/bin/CodeGenCmdLine.jar" ; then
	CODEGEN_JAR="${orig_src_dir}/bin/CodeGenCmdLine.jar";
     elif test -f "${orig_src_dir}/plat/java/lib/CodeGenCmdLine.jar" ; then
	CODEGEN_JAR="${orig_src_dir}/plat/java/lib/CodeGenCmdLine.jar";
     else
	make -C "${orig_src_dir}/src/java/rcs" PLAT=java;
	make -C "${orig_src_dir}/src/java/diagapplet/utils" PLAT=java;
	make -C "${orig_src_dir}/src/java/diagapplet/CodeGen" PLAT=java;
	CODEGEN_JAR="${orig_src_dir}/plat/java/lib/CodeGenCmdLine.jar";
     fi
fi

cp "${CODEGEN_JAR}" .;
CODEGEN_JAR=CodeGenCmdLine.jar;



if test "x${RCS_JAR}" = "x" ; then
    
     if test -f "${testbindir}/rcs.jar" ; then
	RCS_JAR="${testbindir}/rcs.jar";
     elif test -f "${bindir}/rcs.jar" ; then
	RCS_JAR="${bindir}/rcs.jar";
     elif test -f "${builddir}/rcs.jar" ; then
	RCS_JAR="${builddir}/rcs.jar";
     elif test -f "${builddir}/plat/java/lib/rcs.jar" ; then
	RCS_JAR="${builddir}/plat/java/lib/rcs.jar";
     elif test -f "${RCSLIB_DIR}/plat/java/lib/rcs.jar" ; then
	RCS_JAR="${RCSLIB_DIR}/plat/java/lib/rcs.jar";
     elif test -f "${orig_src_dir}/bin/rcs.jar" ; then
	RCS_JAR="${orig_src_dir}/bin/rcs.jar";
     elif test -f "${orig_src_dir}/plat/java/lib/rcs.jar" ; then
	RCS_JAR="${orig_src_dir}/plat/java/lib/rcs.jar";
     else
	make -C "${orig_src_dir}/src/java/rcs" PLAT=java;
	RCS_JAR="${orig_src_dir}/plat/java/lib/rcs.jar";
     fi
fi

cp "${RCS_JAR}" .;
#RCS_JAR=${RCS_JAR##/*}
RCS_JAR=rcs.jar;
jar -xf "${RCS_JAR}"

export RCS_JAR;
export CODEGEN_JAR;

if test "x${CODEGEN_JAR}" = "x" -o '!' -f "${CODEGEN_JAR}" ; then
    if test "x${CODEGEN_COMMAND}" = "x" ; then
	if test -x "${testbindir}/nml_codegen${EXE_EXT}" ; then
	    CODEGEN_COMMAND="${testbindir}/nml_codegen${EXE_EXT}";
	    export CODEGEN_COMMAND;
	elif test -x "${bindir}/nml_codegen${EXE_EXT}" ; then
	    CODEGEN_COMMAND="${bindir}/nml_codegen${EXE_EXT}";
	    export CODEGEN_COMMAND;
	elif test -x "${builddir}/nml_codegen${EXE_EXT}" ; then
	    CODEGEN_COMMAND="${builddir}/nml_codegen${EXE_EXT}";
	    export CODEGEN_COMMAND;
	fi
    fi
fi


echo CODEGEN_COMMAND=${CODEGEN_COMMAND}


echo CODEGEN_JAR=${CODEGEN_JAR}
echo RCS_JAR=${RCS_JAR}
echo RCSLIB_DIR=${RCSLIB_DIR}
echo RCSLIB_LIB_DIR=${RCSLIB_LIB_DIR}
echo USE_SRC_PATH=${USE_SRC_PATH}
echo USE_LIBTOOL=${USE_LIBTOOL}
echo EXEEXT=${EXEEXT}
echo MAKEFLAGS=${MAKEFLAGS}

if test "x${MAKEFLAGS}" != "x" ; then
    MAKEFLAGS=
    export MAKEFLAGS
fi

echo MAKELEVEL=${MAKELEVEL}

if test "x${MAKELEVEL}" != "x" ; then
    MAKELEVEL=
    export MAKELEVEL
fi

echo MAKEOVERRIDES=${MAKEOVERRIDES}
if test "x${MAKEOVERRIDES}" != "x" ; then
    MAKEOVERRIDES=
    export MAKEOVERRIDES
fi

if test "x${MAKE_CMD}" = "x" ; then
    MAKE_CMD=make;
fi

echo MAKE_CMD="${MAKE_CMD}"

"${MAKE_CMD}" --version

"${MAKE_CMD}" -f Makefile.extra_tests clean "CFLAGS=${CFLAGS}" "CXXFLAGS=${CXXFLAGS}"
"${MAKE_CMD}" -f Makefile.extra_tests all "CFLAGS=${CFLAGS}" "CXXFLAGS=${CXXFLAGS}" || ( echo "make failed." ; exit 251;) || exit 251

./extra_tests.sh || (sts=$?; echo "run_extra_tests failed sts=${sts}"; exit ${sts};  ) || exit $?
   
echo "OK"

exit 0



