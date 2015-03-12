#!/bin/sh

if test "x${PLOTTER_NB_JAR}" = "x" ; then
    if test -f ./plotter_NB.jar; then
	export PLOTTER_NB_JAR=./plotter_NB.jar;
    elif test -f ../rcslib/bin/plotter_NB.jar; then
	export PLOTTER_NB_JAR=../rcslib/bin/plotter_NB.jar;
    elif test -f ../rcslib/plat/java/lib/plotter_NB.jar; then
	export PLOTTER_NB_JAR=../rcslib/plat/java/lib/plotter_NB.jar;
    elif test -f ${HOME}/rcslib/plat/java/lib/plotter_NB.jar; then
	export PLOTTER_NB_JAR=${HOME}/rcslib/plat/java/lib/plotter_NB.jar;
    elif test -f ${HOME}/rcslib/bin/plotter_NB.jar; then
	export PLOTTER_NB_JAR=${HOME}/rcslib/bin/plotter_NB.jar;
    elif test -f /usr/local/rcslib/plat/java/lib/plotter_NB.jar; then
	export PLOTTER_NB_JAR=/usr/local/rcslib/plat/java/lib/plotter_NB.jar;
    elif test -f /usr/local/rcslib/bin/plotter_NB.jar; then
	export PLOTTER_NB_JAR=/usr/local/rcslib/bin/plotter_NB.jar;
    elif test -f /local/rcslib/plat/java/lib/plotter_NB.jar; then
	export PLOTTER_NB_JAR=/local/rcslib/plat/java/lib/plotter_NB.jar;
    elif test -f /local/rcslib/bin/plotter_NB.jar; then
	export PLOTTER_NB_JAR=/local/rcslib/bin/plotter_NB.jar;
    elif test -f /working/rcslib/plat/java/lib/plotter_NB.jar; then
	export PLOTTER_NB_JAR=/working/rcslib/plat/java/lib/plotter_NB.jar;
    elif test -f /working/rcslib/bin/plotter_NB.jar; then
	export PLOTTER_NB_JAR=/working/rcslib/bin/plotter_NB.jar;
    elif test -f /mxproj/rcslib/rcslib/plat/java/lib/plotter_NB.jar; then
	export PLOTTER_NB_JAR=/mxproj/rcslib/rcslib/plat/java/lib/plotter_NB.jar;
    fi
fi

echo "${PLOTTER_NB_JAR}";

if test "x${PLOTTER_NB_JAR}" != "x" ; then
    test -f "${PLOTTER_NB_JAR}" >/dev/null 2>/dev/null;
else
    false;
fi
