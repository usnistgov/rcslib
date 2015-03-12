#! /bin/sh

set -x;

orig_d=`pwd`;
d=`echo $0 | sed 's#build_rcsDesign_jar.sh##'`;
true_d=`(cd $d ; pwd)`    
if test "x${d}" != "x" -a "x${d}" != "x./" ; then
    if test -w "${true_d}" ; then
	cd "${true_d}";
    else
	cp "${true_d}"/rcsDesign_sources.txt .
	cp -r "${true_d}/rcs" .;
	cp -r "${true_d}/diagapplet" .;
	cp -r "${true_d}/rcsdesign" .;
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

\rm -rf rcsDesign_lib
mkdir rcsDesign_lib
chmod a+rw rcsDesign_lib
( set -x ; "${JAVAC}"  -d rcsDesign_lib `cat rcsDesign_sources.txt` ) || exit 1
cp rcsdesign/rcsDesignJarInfo.txt rcsDesign_lib
cd rcsDesign_lib
"${JAR}" -cmf0 rcsDesignJarInfo.txt rcsDesign.jar rcs diagapplet rcsdesign || exit 1
cd ..

if test -d ../../src -a -w ../../src ; then
    mkdir -p ../../plat/java/lib
    if test -d ../../plat/java/lib -a -w ../../plat/java/lib ; then
	cp rcsDesign_lib/rcsDesign.jar ../../plat/java/lib;
    fi
fi

if test "x${d}" != "x" -a "x${d}" != "x./" ; then
    cur_dir=`pwd`;
    if test "x${cur_dir}" != "x${orig_d}" ; then
	cp rcsDesign_lib/rcsDesign.jar "${orig_d}";
    else
	cp rcsDesign_lib/rcsDesign.jar .;
	top_dir=`(cd ${d}; cd ../.. ; pwd )`;
	mkdir -p "${top_dir}/plat/java/lib" ;
	if test -d "${top_dir}/plat/java/lib" -a -w "${top_dir}/plat/java/lib" ; then
	    cp rcsDesign_lib/rcsDesign.jar "${top_dir}/plat/java/lib";
	fi
	if test -f "${HOME}/DO_BUILD_JARS_CLEAN" -o "x${HOME}" = "x/tmp/distcheckhome" ; then
	    \rm -rf rcs ;
	    \rm -rf diagapplet ;
	    \rm -rf rcsDesign ;
	    \rm -rf rcsdesign ;
	    \rm -rf *_sources.txt;
	    \rm -rf rcsDesign_lib;
	fi
    fi
fi

\rm -rf *_lib;




