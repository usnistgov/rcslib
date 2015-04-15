#!/bin/sh

TESTNAME="NMLCFGSVR"


export TESTNAME


if test "x${tmpdir}" != "x" ; then
    temp_dir="${tmpdir}";
fi

if test "x${temp_dir}" = "x" ; then
    mkdir "/tmp/tni_${TESTNAME}_$$" && touch "/tmp/tni_${TESTNAME}_$$/.touch_$$" && temp_dir="/tmp/tni_${TESTNAME}_$$";
fi

if test '!' -d ${temp_dir} ; then
    (mkdir  ${temp_dir} || true );
    (mkdir -p ${temp_dir} || true );
fi

tmpdir="${temp_dir}"

if test "x${tmpdir}" = "x" ; then
    tmpdir="/tmp/";
fi
export tmpdir;

echo "      ----  Testing ${TESTNAME} support"
echo "      ----  If this test fails or hangs check"
echo "      ----   ${tmpdir}/${TESTNAME}testnml.log"

if test '!' -d  "${tmpdir}" ; then
    mkdir "${tmpdir}";
fi

touch "${tmpdir}/${TESTNAME}testnml.log";
etcdir=etc

if test ! -f etc/testnml_internal.sh ; then
    if test "x${srcdir}" != "x" -a -f "${srcdir}/etc/testnml_internal.sh" ; then
	etcdir="${srcdir}/etc"
    fi
fi

(  "${etcdir}/testnmlcfgsvr_internal.sh"  2>&1 ) >"${tmpdir}/${TESTNAME}testnml.log";

sts=$?

if test ${sts} -ne 0 ; then 
    echo "    --- ${TESTNAME} support test failed. sts=${sts}" ; 
else
    echo "    --- ${TESTNAME} support test PASSED."
fi

exit ${sts}


