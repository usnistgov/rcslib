#! /bin/sh

if test "x${DEBUG}" != "x" ; then
    set -x;
fi

echo "Running $0 $* . . . from " `pwd`;

if test "x${DEBUG}" != "x" ; then
    echo "2: Running $0 $* . . . from " `pwd` >&2;
fi

orig_d=`pwd`;
d=`echo $0 | sed 's#build_pvjscript_jar.sh##'`;
rcslib_d=`(cd ../../; pwd )`;
if test "x${d}" != "x" -a "x${d}" != "x./" -a "${JSOURCE_REL_DIR}x" = "x" ; then
    true_d=`(cd $d ; pwd)`
    rcslib_d=`(cd $true_d; cd ../../; pwd )`;
    if test -w "${true_d}" ; then
	cd "${true_d}";
    else
	cp "${true_d}"/diag_NB_sources.txt .;
	cp -r "${true_d}/pvjscript" .;
	cp -r "${true_d}/pvjscript.resources" .;
	chmod -R a+rw *;
    fi
fi

need_build=0;
cur_dir=`pwd`;
if test "x${cur_dir}" != "x${orig_d}" ; then
    if test ! -e ${orig_d}/pvjscript.jar  ||  find pvjscript -newer ${orig_d}/pvjscript.jar | grep pvjscript ; then
      need_build=1;
    fi  
fi

if test ! -e ${JSOURCE_REL_DIR}../../plat/java/lib/pvjscript.jar  ||  find pvjscript -newer ${JSOURCE_REL_DIR}../../plat/java/lib/pvjscript.jar | grep pvjscript ; then
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
    
    before_3rd_party_cd_dir=`pwd`;
    if test "x${JSOURCE_REL_DIR}" != "x" ; then
	cd ${JSOURCE_REL_DIR};
    fi

    rel_dist_jar_dir=../../plat/java/lib;
    dist_jar_dir=`cd ${rel_dist_jar_dir}; pwd`;

    # if test -d "${rcslib_d}/3rd_party_jars" ; then 
    # 	cd ${rcslib_d};
    # 	cd 3rd_party_jars; 
	
    # 	if test "x${DEBUG}" != "x" ; then
    # 	    pwd;
    # 	fi

    # 	third_party_jar_dir=`pwd`;

    # 	for jar in *.jar ; do
    # 	    if test '!' -f "${dist_jar_dir}/${jar}" ; then
    # 		cp -p "${jar}" "${dist_jar_dir}";
    # 	    fi
    # 	    needed_jars="${needed_jars} -cp ${rel_dist_jar_dir}/${jar}";
    # 	    CLASSPATH="${CLASSPATH}:${dist_jar_dir}/${jar}";
    # 	done;

    # fi
    cd "${before_3rd_party_cd_dir}";

    if test "x${DEBUG}" != "x" ; then
	pwd;
    fi

    \rm -rf pvjscript_lib
    mkdir pvjscript_lib
    
    if test "x${DEBUG}" != "x" ; then
	pwd;
    fi

    jsources=`cat ${JSOURCE_REL_DIR}pvjscript_sources.txt | awk '{printf("%s%s"," '"${JSOURCE_REL_DIR}"'",$1);}'` 
    echo 
    echo

    if test "x${OSTYPE}" = "xcygwin" ; then
	export EXPAND_NEEDED_JARS=1;
    fi

    if test "x${OS}" = "xWindows_NT" ; then
	export EXPAND_NEEDED_JARS=1;
    fi

    if test "x${EXPAND_NEEDED_JARS}" = "x1" ; then
	\rm -rf class_dir;
	mkdir class_dir
	cd class_dir;
	for jar_file in ../${rel_dist_jar_dir}/*.jar; do
	    "${JAR}" -xf $jar_file;
	done
	cd ..

	echo
	echo


	( set -x ; "${JAVAC}" -cp class_dir  -d pvjscript_lib ${jsources} ) || exit 0


    else

	echo
	echo


	( set -x ; "${JAVAC}" -classpath "${CLASSPATH}" -d pvjscript_lib ${jsources} ) || exit 0
	
    fi

    cp ${JSOURCE_REL_DIR}pvjscript/pvjscriptJarInfo.txt pvjscript_lib
    cp ${JSOURCE_REL_DIR}pvjscript/*.{png,gif,jpg} pvjscript_lib/pvjscript/
    cp ${JSOURCE_REL_DIR}pvjscript/plotter/*.{png,gif,jpg} pvjscript_lib/pvjscript/plotter/
    cd pvjscript_lib
    echo
    echo
    "${JAR}"  -cmf0 pvjscriptJarInfo.txt pvjscript.jar pvjscript || exit 0
    chmod a+rx pvjscript.jar
    cd ..

    if test -d ${JSOURCE_REL_DIR}../../src -a -w ${JSOURCE_REL_DIR}../../src ; then
	mkdir -p ${JSOURCE_REL_DIR}../../plat/java/lib
	if test -d ${JSOURCE_REL_DIR}../../plat/java/lib -a -w ${JSOURCE_REL_DIR}../../plat/java/lib ; then
	    cp -p "${third_party_jar_dir}"/*.jar  ${JSOURCE_REL_DIR}../../plat/java/lib;
	    cp -p pvjscript_lib/pvjscript.jar ${JSOURCE_REL_DIR}../../plat/java/lib;
	fi
	mkdir -p ${JSOURCE_REL_DIR}../../bin
	if test -d ${JSOURCE_REL_DIR}../../bin -a -w ${JSOURCE_REL_DIR}../../bin ; then
	    cp -p "${third_party_jar_dir}"/*.jar  ${JSOURCE_REL_DIR}../../bin;
	    cp -p pvjscript_lib/pvjscript.jar ${JSOURCE_REL_DIR}../../bin;
	fi

    elif test -d ../../src -a -w ../../src ; then
	mkdir -p ../../plat/java/lib
	if test -d ../../plat/java/lib -a -w ../../plat/java/lib ; then
	    cp -p "${third_party_jar_dir}"/*.jar ../../plat/java/lib;
	    cp -p pvjscript_lib/pvjscript.jar ../../plat/java/lib;
	fi
	mkdir -p ../../bin
	if test -d ../../bin -a -w ../../bin ; then
	    cp -p "${third_party_jar_dir}"/*.jar ../../bin;
	    cp -p pvjscript_lib/pvjscript.jar ../../bin;
	fi

    fi

    if test "x${d}" != "x" -a "x${d}" != "x./" -a ${JSOURCE_REL_DIR}x = "x" ; then
	cur_dir=`pwd`;
	if test "x${cur_dir}" != "x${orig_d}" ; then
	    cp -p "${third_party_jar_dir}"/*.jar "${orig_d}";
	    cp -p pvjscript_lib/pvjscript.jar "${orig_d}";
	else
	    cp -p pvjscript_lib/pvjscript.jar .;
	    top_dir=`(cd ${d}; cd ../.. ; pwd )`;
	    mkdir -p "${top_dir}/plat/java/lib"
	    if test -d "${top_dir}/plat/java/lib" -a -w "${top_dir}/plat/java/lib" ; then
		cp -p "${third_party_jar_dir}"/*.jar "${top_dir}/plat/java/lib";
		cp -p pvjscript_lib/pvjscript.jar "${top_dir}/plat/java/lib";
	    fi
	    mkdir -p "${top_dir}/bin/"
	    if test -d "${top_dir}/bin" -a -w "${top_dir}/bin" ; then
		cp -p "${third_party_jar_dir}"/*.jar "${top_dir}/bin";
		cp -p pvjscript_lib/pvjscript.jar "${top_dir}/bin";
	    fi
	    if test -f "${HOME}/DO_BUILD_JARS_CLEAN" -o "x${HOME}" = "x/tmp/distcheckhome" ; then
		set -x;
		echo "Cleaning up after building jars."
		echo "2: Cleaning up after building jars." >&2
		\rm -rf pvjscript_sources.txt;
		\rm -rf *.sh;
		\rm -rf pvjscript;
		\rm -rf pvjscript_lib;
	    fi
	fi
    fi


    \rm -rf *_lib ;
    \rm -rf class_dir;

    if test -x "${HOME}/bin/sign_rcsjava.sh" -a -f "${HOME}/myKeys" ; then
	"${HOME}/bin/sign_rcsjava.sh"
    fi
fi


echo "End $0 $*"

if test "x${DEBUG}" != "x" ; then
    echo "2: End $0 $*" >&2
fi


