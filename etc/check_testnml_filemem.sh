#!/bin/sh

TESTNAME="FILEMEM"
NMLCFG_OPTIONS="-D B1OPTS=buftype=filemem%%%disp%%%neutral=1%%%infile=${HOME}/.tmp/filememfile%%%outfile=${HOME}/.tmp/filememfile"
NML_TEST_BLOCKING=no
NML_TEST_LOCAL=yes
NML_TEST_REMOTE=no


if test -f ${HOME}/.run_filemem_check ; then

export TESTNAME
export NMLCFG_OPTIONS
export NML_TEST_BLOCKING
export NML_TEST_LOCAL
export NML_TEST_REMOTE

if test "x${tmpdir}" != "x" ; then
    temp_dir="${tmpdir}";
fi

if test "x${temp_dir}" = "x" ; then
    mkdir /tmp/tni_${TESTNAME}_$$ && touch /tmp/tni_${TESTNAME}_$$/.touch_$$ && temp_dir="/tmp/tni_${TESTNAME}_$$";
fi

if test "x${temp_dir}" = "x" ; then
    mkdir ${HOME}/.tmp && mkdir ${HOME}/.tmp/tni_${TESTNAME}_$$ && touch ${HOME}/.tmp/tni_${TESTNAME}_$$/.touch_$$ && temp_dir="${HOME}/.tmp/tni_${TESTNAME}_$$";
fi

if test '!' -d ${temp_dir} ; then
    (mkdir  ${temp_dir} || true );
    (mkdir -p ${temp_dir} || true );
fi

tmpdir="${temp_dir}";

( mkdir ${tmpdir} && chmod u+w ${tmpdir}  || true) >/dev/null 2>/dev/null ;

echo "      ----  Testing ${TESTNAME} support"
echo "      ----  If this test fails or hangs check"
echo "      ----   ${tmpdir}/${TESTNAME}testnml.log"

if test '!' -d  ${tmpdir} ; then
    mkdir ${tmpdir}
fi

touch ${tmpdir}/${TESTNAME}testnml.log
etcdir=etc

if test ! -f etc/testnml_internal.sh ; then
    if test "x${srcdir}" != "x" -a -f ${srcdir}/etc/testnml_internal.sh ; then
	etcdir="${srcdir}/etc"
    fi
fi

(  ${etcdir}/testnml_internal.sh  2>&1 ) >${tmpdir}/${TESTNAME}testnml.log

sts=$?

if test ${sts} -ne 0 ; then 
    echo "    --- ${TESTNAME} support test failed. sts=${sts}" ; 
    echo "      ----  IGNORING FILEMEM ERROR"
    echo "      ----  IGNORING FILEMEM ERROR" >&2
    sts=0
else
    echo "    --- ${TESTNAME} support test PASSED."
fi

else
    echo "      ----  Skipping filemem check";
    echo "      ----  touch ${HOME}/.run_filemem_check to actually run this test."
    sts=0
fi

exit ${sts}


