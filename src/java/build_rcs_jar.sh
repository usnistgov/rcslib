#! /bin/sh


if test "x${DEBUG}" != "x" ; then
    set -x;
    echo "Running $0 $* . . ."
    pwd
fi

orig_d=`pwd`;
d=`echo $0 | sed 's#build_rcs_jar.sh##'`;
if test "x${d}" != "x" -a "x${d}" != "x./" ; then
    true_d=`(cd $d ; pwd)`
    if test -w "${true_d}" ; then
	cd "${true_d}";
    else
	cp "${true_d}"/rcs_sources.txt .
	cp -r "${true_d}/rcs" .;
	chmod -R a+rw *;
    fi
fi

need_build=0;
cur_dir=`pwd`;
if test "x${cur_dir}" != "x${orig_d}" ; then
    if test ! -e ${orig_d}/rcs.jar  ||  find rcs -newer ${orig_d}/rcs.jar | grep rcs ; then
      need_build=1;
    fi  
fi

if test ! -e ${JSOURCE_REL_DIR}../../plat/java/lib/rcs.jar  ||  find rcs -newer ${JSOURCE_REL_DIR}../../plat/java/lib/rcs.jar | grep rcs ; then
    need_build=1;
fi

if test "x${need_build}" != "x0" ; then
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

    ( if test "x${DEBUG}" != "x" ; then set -x; cd rcs ; ./update_ver; fi );
    \rm -rf rcs_lib
    mkdir rcs_lib
    chmod a+rw rcs_lib;
    which javac
    which $JAVAC
    ( set -x ; "${JAVAC}" ${JAVAC_FLAGS} -d rcs_lib `cat rcs_sources.txt` ) || exit 1
    cp ${JSOURCE_REL_DIR}rcs/rcsJarInfo.txt rcs_lib;
    cd rcs_lib
    "${JAR}" -cmf0  rcsJarInfo.txt rcs.jar rcs || exit 1
    chmod a+rx rcs.jar;
    cd ..
    
    if test "x${DEBUG}" != "x" ; then
	pwd;
	ls -l rcs_lib/;
    fi
    

    if test -d ../../src -a -w ../../src ; then
	mkdir -p ../../plat/java/lib
	if test -d ../../plat/java/lib -a -w ../../plat/java/lib ; then
	    cp -p rcs_lib/rcs.jar ../../plat/java/lib;
	fi
	mkdir -p ../../bin
	if test -d ../../bin -a -w ../../bin ; then
	    cp -p rcs_lib/rcs.jar ../../bin;
	fi
    fi

    if test "x${d}" != "x" -a "x${d}" != "x./" ; then
	cur_dir=`pwd`;
	if test "x${cur_dir}" != "x${orig_d}" ; then
	    cp -p rcs_lib/rcs.jar "${orig_d}";
	else
	    cp -p rcs_lib/rcs.jar .;
	    top_dir=`(cd ${d}; cd ../.. ; pwd )`;
	    mkdir -p "${top_dir}/plat/java/lib";
	    if test -d "${top_dir}/plat/java/lib" -a -w "${top_dir}/plat/java/lib" ; then
		cp -p rcs_lib/rcs.jar "${top_dir}/plat/java/lib";
	    fi
	    mkdir -p "${top_dir}/bin";
	    if test -d "${top_dir}/bin" -a -w "${top_dir}/bin" ; then
		cp -p rcs_lib/rcs.jar "${top_dir}/bin";
	    fi
	    if test -f "${HOME}/DO_BUILD_JARS_CLEAN" -o "x${HOME}" = "x/tmp/distcheckhome" ; then
		\rm -rf rcs
		\rm -rf rcs_lib;
		\rm -rf *_sources.txt;
	    fi
	fi
    fi


    \rm -rf *_lib;
fi

if test "x${DEBUG}" != "x" ; then
       echo "End $0 $*";
fi

