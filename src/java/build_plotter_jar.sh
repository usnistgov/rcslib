#! /bin/sh

set -x;

echo "Running $0 $* . . ."
pwd

orig_d=`pwd`;
d=`echo $0 | sed 's#build_plotter_jar.sh##'`;
true_d=`(cd $d ; pwd)`;
if test "x${d}" != "x" -a "x${d}" != "x./" -a ${JSOURCE_REL_DIR}x = x ; then
    if test -w "${true_d}"; then
	cd "${true_d}";
    else
	cp "${true_d}"/plotter_sources.txt .
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

\rm -rf plotter_lib
mkdir plotter_lib
chmod a+rw plotter_lib
pwd
jsources=`cat ${JSOURCE_REL_DIR}plotter_sources.txt | awk '{printf("%s%s"," '"${JSOURCE_REL_DIR}"'",$1);}'` 
( set -x ; "${JAVAC}" -d plotter_lib ${jsources} ) || exit 1
cp ${JSOURCE_REL_DIR}diagapplet/plotter/plotterJarInfo.txt plotter_lib
cd plotter_lib
"${JAR}" -cmf0 plotterJarInfo.txt plotter.jar rcs diagapplet || exit 1
cd ..

if test -d ${JSOURCE_REL_DIR}../../src -a -w ${JSOURCE_REL_DIR}../../src ; then
    mkdir -p ${JSOURCE_REL_DIR}../../plat/java/lib
    if test -d ${JSOURCE_REL_DIR}../../plat/java/lib -a -w ${JSOURCE_REL_DIR}../../plat/java/lib ; then
	cp plotter_lib/plotter.jar ${JSOURCE_REL_DIR}../../plat/java/lib;
    fi
fi

if test "x${d}" != "x" -a "x${d}" != "x./" -a ${JSOURCE_REL_DIR}x = x ; then
    cur_dir=`pwd`;
    if test "x${cur_dir}" != "x${orig_d}" ; then
	cp plotter_lib/plotter.jar "${orig_d}";
    else
	cp plotter_lib/plotter.jar .;
	top_dir=`(cd ${d}; cd ../.. ; pwd )`;
	mkdir -p "${top_dir}/plat/java/lib" ;
	if test -d "${top_dir}/plat/java/lib" -a -w "${top_dir}/plat/java/lib" ; then
	    cp plotter_lib/plotter.jar "${top_dir}/plat/java/lib";
	fi
	if test -f "${HOME}/DO_BUILD_JARS_CLEAN" -o "x${HOME}" = "x/tmp/distcheckhome" ; then
	    \rm -rf rcs
	    \rm -rf diagapplet;
	    \rm -f *_sources.txt;
	fi
	\rm -rf plotter_lib;
    fi
fi

if test -d plotter_lib ; then
    \rm -rf plotter_lib ;
fi





