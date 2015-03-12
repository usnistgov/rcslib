#! /bin/sh

if test "x${DEBUG}" = "x1" ; then
    set -x;
fi

\rm -f diag.running;

dir0=${0%/*};
dir0_abs=`cd $dir0 ; pwd`

if test "x${script_dir}" = "x" ; then
    if test -x ./diag_NB.sh ; then
	script_dir=./;
    elif test -x "${dir0_abs}/diag_NB.sh" ; then
	script_dir="${dir0_abs}/";
    elif test -x "${RCSLIB_DIR}/bin/diag_NB.sh" ; then
	script_dir="${RCSLIB_DIR}/bin/";
    elif test -x "${RCSLIB_DIR}/etc/diag_NB.sh" ; then
	script_dir="${RCSLIB_DIR}/etc/";
    fi
fi

if echo "${PATH}" | grep -v "/usr/java/current" >/dev/null 2>/dev/null ; then
    if test -d "/usr/java/currentJava/bin" ; then
	PATH="/usr/java/currentJava/bin:${PATH}";
    elif test -d "/usr/java/current_java/bin" ; then
	PATH="/usr/java/current_java/bin:${PATH}";
    fi
    export PATH;
fi

if test -x ${script_dir}jdk_dir.sh ; then
    JDK_DIR=`${script_dir}jdk_dir.sh`;
fi

if test -x ${script_dir}jre_dir.sh ; then
    JRE_DIR=`${script_dir}jre_dir.sh`;
fi

JAVA=java;

if test -x "${JDK_DIR}/bin/java" ; then
    JAVA="${JDK_DIR}/bin/java";
fi

if test -x "${JRE_DIR}/bin/java" ; then
    JAVA="${JRE_DIR}/bin/java";
fi

if test "x${DEBUG}" = "x1" ; then
    echo 'ls -ld /usr/java/?urrent*ava'
    ls -ld /usr/java/?urrent*ava;
fi


if test "x${DEBUG}" = "x1" ; then

    echo "JRE_DIR=${JRE_DIR}";
    echo "JDK_DIR=${JDK_DIR}";
    echo "JAVA=${JAVA}";

    echo "which java";
    which java;
    echo "which ${JAVA}";
    which "${JAVA}";

    echo "java -version";
    java -version;
    echo "${JAVA} -version";
    "${JAVA}" -version;

    echo 
    echo
fi


if test "x${CONFIG_NML}" = "x" -a "x${DIAG_FILE}" != "x" ; then
    if grep 'getenv' "${DIAG_FILE}" | grep 'CONFIG_NML' >/dev/null 2>/dev/null; then
	echo ""
	echo "Please set CONFIG_NML environment variable.";
	exit 1;
    fi
fi
    
if test "x${DIAG_NB_JAR}" = "x" ; then
    DIAG_NB_JAR=`${script_dir}find_diag_NB_jar.sh`;
fi


if test "x${DEBUG}" != "x" ; then
    export JAVA_DIAG_FLAGS="${JAVA_DIAG_FLAGS} -agentlib:jdwp=transport=dt_socket,address=8000,server=y,suspend=n ";
    echo "${JAVA}" ${JAVA_DIAG_FLAGS} -jar "${DIAG_NB_JAR}" ${DIAG_FILE} $*;
    echo 
    echo

fi

echo JAVA="${JAVA}";
"${JAVA}" -version;
"${JAVA}" ${JAVA_DIAG_FLAGS} -jar "${DIAG_NB_JAR}" ${DIAG_FILE} $*;
\rm -f diag.running;

