#!/bin/sh

if test "x${DIAG_NB_JAR}" = "x" ; then
    if test -f ./diag_NB.jar; then
	export DIAG_NB_JAR=./diag_NB.jar;
    elif test -f ../rcslib/bin/diag_NB.jar; then
	export DIAG_NB_JAR=../rcslib/bin/diag_NB.jar;
    elif test -f ../rcslib/plat/java/lib/diag_NB.jar; then
	export DIAG_NB_JAR=../rcslib/plat/java/lib/diag_NB.jar;
    elif test -f ${HOME}/rcslib/plat/java/lib/diag_NB.jar; then
	export DIAG_NB_JAR=${HOME}/rcslib/plat/java/lib/diag_NB.jar;
    elif test -f ${HOME}/rcslib/bin/diag_NB.jar; then
	export DIAG_NB_JAR=${HOME}/rcslib/bin/diag_NB.jar;
    elif test -f /usr/local/rcslib/plat/java/lib/diag_NB.jar; then
	export DIAG_NB_JAR=/usr/local/rcslib/plat/java/lib/diag_NB.jar;
    elif test -f /usr/local/rcslib/bin/diag_NB.jar; then
	export DIAG_NB_JAR=/usr/local/rcslib/bin/diag_NB.jar;
    elif test -f /local/rcslib/plat/java/lib/diag_NB.jar; then
	export DIAG_NB_JAR=/local/rcslib/plat/java/lib/diag_NB.jar;
    elif test -f /local/rcslib/bin/diag_NB.jar; then
	export DIAG_NB_JAR=/local/rcslib/bin/diag_NB.jar;
    elif test -f /working/rcslib/plat/java/lib/diag_NB.jar; then
	export DIAG_NB_JAR=/working/rcslib/plat/java/lib/diag_NB.jar;
    elif test -f /working/rcslib/bin/diag_NB.jar; then
	export DIAG_NB_JAR=/working/rcslib/bin/diag_NB.jar;
    elif test -f /mxproj/rcslib/rcslib/plat/java/lib/diag_NB.jar; then
	export DIAG_NB_JAR=/mxproj/rcslib/rcslib/plat/java/lib/diag_NB.jar;
    fi
fi

echo "${DIAG_NB_JAR}";

if test "x${DIAG_NB_JAR}" != "x" ; then
    test -f "${DIAG_NB_JAR}" >/dev/null 2>/dev/null;
else
    false;
fi
