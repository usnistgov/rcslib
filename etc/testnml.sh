#!/bin/sh

echo "If this test fails check ${HOME}/.tmp/testnml.log and ${HOME}/.tmp/testnml.err."

if test -d ${HOME}/.tmp ; then
 \rm -r -f ${HOME}/.tmp
fi

mkdir ${HOME}/.tmp

if test ! -f etc/testnml_internal.sh ; then
    if test "x${srcdir}" != "x" -a -f ${srcdir}/etc/testnml_internal.sh ; then
	. ${srcdir}/etc/testnml_internal.sh >${HOME}/.tmp/testnml.log 2>${HOME}/.tmp/testnml.err 
	sts=$?
	echo sts=${sts}
	exit ${sts}
    fi
fi

. etc/testnml_internal.sh >${HOME}/.tmp/testnml.log 2>${HOME}/.tmp/testnml.err 
sts=$?
echo sts=${sts}
exit ${sts}

