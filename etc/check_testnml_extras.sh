#!/bin/sh

TESTNAME="EXTRAS"

export TESTNAME
export NMLCFG_OPTIONS
export NML_TEST_BLOCKING
export NML_TEST_LOCAL
export NML_TEST_REMOTE

if test "x${tmpdir}" != "x" ; then
    temp_dir="${tmpdir}";
fi

if test "x${temp_dir}" = "x" ; then
    mkdir "/tmp/tni_${TESTNAME}_$$" && touch "/tmp/tni_${TESTNAME}_$$/.touch_$$" && temp_dir="/tmp/tni_${TESTNAME}_$$";
fi

if test '!' -d "${temp_dir}" ; then
    (mkdir "${temp_dir}" || true );
    (mkdir -p "${temp_dir}" || true );
fi

tmpdir="${temp_dir}";

( mkdir "${tmpdir}" && chmod u+w "${tmpdir}" || true) >/dev/null 2>/dev/null ;

if test -f "${tmpdir}/${TESTNAME}testnml.log" ; then
    rm "${tmpdir}/${TESTNAME}testnml.log" ;
fi
( sync ) >/dev/null 2>/dev/null
( sync ) >/dev/null 2>/dev/null

echo "      ----  Testing ${TESTNAME} support"
echo "      ----  If this test fails or hangs check"
echo "      ----   ${tmpdir}/${TESTNAME}testnml.log"

if test '!' -d "${tmpdir}" ; then
    mkdir "${tmpdir}";
fi

etcdir=etc

if test ! -f etc/test_extras.sh ; then
    if test "x${srcdir}" != "x" -a -f ${srcdir}/etc/test_extras.sh ; then
	etcdir="${srcdir}/etc"
    fi
fi

JDK_DIR0=`${etcdir}/jdk_dir.sh`
if test "x${JDK_DIR0}" != "x" ; then
    JDK_DIR=`(cd ${JDK_DIR0} ; pwd )`;
    export JDK_DIR
fi

if test -f "${HOME}/.SKIP_RCSLIB_EXTRA_TESTS" ; then
    echo "      ----  Skipping extras test ${HOME}/.SKIP_RCSLIB_EXTRA_TESTS exists."
    exit 0;
fi

if test "x${SKIP_RCSLIB_EXTRA_TESTS}" != "x" ; then
    echo "      ----  Skipping extras test SKIP_RCSLIB_EXTRA_TESTS environment variable set.";
    exit 0;
fi

if test "x${JDK_DIR}" = "x" ; then
    echo "      ----  Skipping extras test no Java Developers Kit found."
    exit 0;
fi

( "${etcdir}/test_extras.sh"  2>&1 ) >"${tmpdir}/${TESTNAME}testnml.log"

sts=$?

if test ${sts} -ne 0 ; then 
    echo "    --- ${TESTNAME} support test failed. sts=${sts}" ; 
    if test -f "${HOME}/.ignore_nml_extras_test" ; then
	echo "    --- ${TESTNAME} failure ignored.";
	sts=0;
	exit 0;
    else
	echo "      ----  touch ${HOME}/.ignore_nml_extras_test to ignore this in the future";
    fi
else
    echo "    --- ${TESTNAME} support test PASSED."
    if test -f "${HOME}/.ignore_nml_extras_test" ; then
	echo "      ----  Removing ${HOME}/.ignore_nml_extras_test";
	rm "${HOME}/.ignore_nml_extras_test" ;
    fi
fi

exit ${sts}






