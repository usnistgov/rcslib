#! /bin/sh

if test "x${DEBUG}" != "x" ; then
    set -x;
    echo "Starting $0 $* ...";
    pwd
fi

orig_d=`pwd`;
d=`echo $0 | sed 's#build_CodeGenCmdLine_jar.sh##'`;
true_d=`(cd $d ; pwd)`;
if test "x${d}" != "x" -a "x${d}" != "x./" -a ${JSOURCE_REL_DIR}x = x ; then
    if test -w "${true_d}" ; then
	cd "${true_d}";
    else
	cp "${true_d}"/CodeGenCmdLine_sources.txt .
	cp -r "${true_d}/rcs" .;
	cp -r "${true_d}/diagapplet" .;
	chmod -R a+rw *;
    fi
fi


if test "x${JDK_DIR}" = "x" ; then
    if test -x ../../etc/jdk_dir.sh ; then
	JDK_DIR=`../../etc/jdk_dir.sh`;
	export JDK_DIR;
    fi
fi


if test "x${JAVAC}" = "x" ; then
    if test -x "${JDK_DIR}/bin/javac" ; then
	JAVAC="${JDK_DIR}/bin/javac";
    else
	JAVAC=javac;
    fi
fi
if test "x${JAR}" = "x" ; then
    if test -x "${JDK_DIR}/bin/jar" ; then
	JAR="${JDK_DIR}/bin/jar";
    else
	JAR=jar;
    fi
fi


\rm -rf CodeGenCmdLine_lib
mkdir CodeGenCmdLine_lib
jsources=`cat ${JSOURCE_REL_DIR}CodeGenCmdLine_sources.txt | awk '{printf("%s%s"," '"${JSOURCE_REL_DIR}"'",$1);}'` 
( set -x ; "${JAVAC}" ${JAVAC_FLAGS} -d CodeGenCmdLine_lib ${jsources} )  || exit 1
cp ${JSOURCE_REL_DIR}diagapplet/CodeGen/CodeGenCmdLineJarInfo.txt CodeGenCmdLine_lib
cd CodeGenCmdLine_lib
"${JAR}" -cmf0 CodeGenCmdLineJarInfo.txt CodeGenCmdLine.jar rcs diagapplet || exit 1
chmod a+rx CodeGenCmdLine.jar
cd ..


if test -d ${JSOURCE_REL_DIR}../../src -a -w ${JSOURCE_REL_DIR}../../src ; then
    mkdir -p ${JSOURCE_REL_DIR}../../plat/java/lib
    if test -d ${JSOURCE_REL_DIR}../../plat/java/lib -a -w ${JSOURCE_REL_DIR}../../plat/java/lib ; then
	cp -p CodeGenCmdLine_lib/CodeGenCmdLine.jar ${JSOURCE_REL_DIR}../../plat/java/lib;
    fi
fi

if test "x${d}" != "x" -a "x${d}" != "x./"  -a ${JSOURCE_REL_DIR}x = x ; then
    cur_dir=`pwd`;
    if test "x${cur_dir}" != "x${orig_d}" ; then
	cp -p CodeGenCmdLine_lib/CodeGenCmdLine.jar "${orig_d}";
    else
	cp -p CodeGenCmdLine_lib/CodeGenCmdLine.jar .;
	top_dir=`(cd ${d}; cd ../.. ; pwd )`;
	mkdir -p "${top_dir}/plat/java/lib";
	if test -d "${top_dir}/plat/java/lib" -a -w "${top_dir}/plat/java/lib" ; then
	    cp -p CodeGenCmdLine_lib/CodeGenCmdLine.jar "${top_dir}/plat/java/lib";
	fi
	if test -f "${HOME}/DO_BUILD_JARS_CLEAN" -o "x${HOME}" = "x/tmp/distcheckhome" ; then
	    \rm -rf rcs
	    \rm -rf diagapplet
	    \rm -rf CodeGenCmdLine_lib;
	    \rm -f *sources.txt;
	fi
    fi
fi


\rm -rf *_lib;

if test "x${DEBUG}" != "x" ; then
    echo "End $0 $*";
fi


