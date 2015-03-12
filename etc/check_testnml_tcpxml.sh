#!/bin/sh

TESTNAME="TCP_with_XML"
NMLCFG_OPTIONS="-D B1OPTS=remotetype=tcp%%%xml"
NML_TEST_BLOCKING=yes
NML_TEST_LOCAL=no

export TESTNAME
export NMLCFG_OPTIONS
export NML_TEST_BLOCKING
export NML_TEST_LOCAL

if test "x${tmpdir}" != "x" ; then
    temp_dir="${tmpdir}";
fi

if test "x${temp_dir}" = "x" ; then
    mkdir "/tmp/tni_${TESTNAME}_$$" && touch "/tmp/tni_${TESTNAME}_$$/.touch_$$" && temp_dir="/tmp/tni_${TESTNAME}_$$";
fi

if test "x${temp_dir}" = "x" ; then
    mkdir "${HOME}/.tmp" && mkdir "${HOME}/.tmp/tni_${TESTNAME}_$$" && touch "${HOME}/.tmp/tni_${TESTNAME}_$$/.touch_$$" && temp_dir="${HOME}/.tmp/tni_${TESTNAME}_$$";
fi

if test '!' -d "${temp_dir}" ; then
    (mkdir "${temp_dir}" || true );
    (mkdir -p "${temp_dir}" || true );
fi

tmpdir="${temp_dir}";

if test -f "${tmpdir}/${TESTNAME}testnml.log"; then
    rm "${tmpdir}/${TESTNAME}testnml.log" ;
fi
( sync ) >/dev/null 2>/dev/null
( sync ) >/dev/null 2>/dev/null


echo "      ----  Testing ${TESTNAME} support"
echo "      ----  If this test fails or hangs check"
echo "      ----   ${tmpdir}/${TESTNAME}testnml.log"

if test '!' -d  "${tmpdir}" ; then
    mkdir "${tmpdir}";
fi

etcdir=etc

if test ! -f etc/testnml_internal.sh ; then
    if test "x${srcdir}" != "x" \
	-a -f "${srcdir}/etc/testnml_internal.sh" ; then
	etcdir="${srcdir}/etc";
    fi
fi

( "${etcdir}/testnml_internal.sh"  2>&1 ) >"${tmpdir}/${TESTNAME}testnml.log"

sts=$?

if test ${sts} -ne 0 ; then 
    echo "    --- ${TESTNAME} support test failed. sts=${sts}" ; 
else
    echo "    --- ${TESTNAME} support test PASSED."
fi

exit ${sts}
