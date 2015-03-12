#! /bin/sh

if test "x${DEBUG}" != "x" ; then
    set -x;
fi

echo "Running $0 $* . . . from " `pwd`;

if test "x${DEBUG}" != "x" ; then
    echo "2: Running $0 $* . . . from " `pwd` >&2;
fi

orig_d=`pwd`;
d=`echo $0 | sed 's#build_diag_NB_jar.sh##'`;
rcslib_d=`(cd ../../; pwd )`;
if test "x${d}" != "x" -a "x${d}" != "x./" -a "${JSOURCE_REL_DIR}x" = "x" ; then
    true_d=`(cd $d ; pwd)`
    rcslib_d=`(cd $true_d; cd ../../; pwd )`;
    if test -w "${true_d}" ; then
	cd "${true_d}";
    else
	if test "x${PLOTTER_NB_JAR_UPTODATE}" = "x" ; then
	    cp "${true_d}"/build_plotter_NB_jar.sh .
	fi
	if test "x${RCS_JAR_UPTODATE}" = "x" ; then
	    cp "${true_d}"/build_rcs_jar.sh .
	fi
	if test "x${CodeGenCmdLine_JAR_UPTODATE}" = "x" ; then
	    cp "${true_d}"/build_CodeGenCmdLine_jar.sh .
	fi
	cp "${true_d}"/diag_NB_sources.txt .;
	cp -r "${true_d}/rcs" .;
	cp -r "${true_d}/diagapplet" .;
	chmod -R a+rw *;
    fi
fi

need_build=0;
cur_dir=`pwd`;
if test "x${cur_dir}" != "x${orig_d}" ; then
    if test ! -e ${orig_d}/rcs.jar  ||  find rcs -newer ${orig_d}/rcs.jar | grep rcs ; then
      need_build=1;
    fi  
    if test ! -e ${orig_d}/diag_NB.jar  ||  find diagapplet -newer ${orig_d}/diag_NB.jar | grep diagapplet ; then
      need_build=1;
    fi  
fi

if test ! -e ${JSOURCE_REL_DIR}../../plat/java/lib/rcs.jar  ||  find rcs -newer ${JSOURCE_REL_DIR}../../plat/java/lib/rcs.jar | grep rcs ; then
    need_build=1;
fi
if test ! -e ../../plat/java/lib/diag_NB.jar  ||  find diagapplet -newer ../../plat/java/lib/diag_NB.jar | grep diagapplet || test ! -e ../../plat/java/lib/rcs.jar  ||  find rcs -newer ../../plat/java/lib/rcs.jar | grep rcs ; then
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
    
    if test "x${RCS_JAR_UPTODATE}" = "x" ; then
	if test '!' -x ./build_rcs_jar.sh ; then
	    echo "$0 $* run from directory without build_rcs_jar.sh"
	    echo "$0 $* run from directory without build_rcs_jar.sh" >&2
	    pwd
	    pwd >&2
	    ls -l
	    ls -l >&2
	    exit 1
	fi
	./build_rcs_jar.sh
    fi

    if test "x${PLOTTER_NB_JAR_UPTODATE}" = "x" ; then
	if test '!' -x ./build_plotter_NB_jar.sh ; then
	    echo "$0 $* run from directory without build_plotter_NB_jar.sh"
	    echo "$0 $* run from directory without build_plotter_NB_jar.sh" >&2
	    pwd
	    pwd >&2
	    ls -l
	    ls -l >&2
	    exit 1
	fi
	./build_plotter_NB_jar.sh
    fi

    if test "x${CodeGenCmdLine_JAR_UPTODATE}" = "x" ; then
	if test '!' -x ./build_CodeGenCmdLine_jar.sh ; then
	    echo "$0 $* run from directory without build_CodeGenCmdLine_jar.sh"
	    echo "$0 $* run from directory without build_CodeGenCmdLine_jar.sh" >&2
	    pwd
	    pwd >&2
	    ls -l
	    ls -l >&2
	    exit 1
	fi
	./build_CodeGenCmdLine_jar.sh
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
    
    # 	needed_jars="-cp ${rel_dist_jar_dir}/rcs.jar -cp ${rel_dist_jar_dir}/plotter_NB.jar -cp ${rel_dist_jar_dir}/CodeGenCmdLine.jar";
    # 	CLASSPATH="${dist_jar_dir}/rcs.jar:${dist_jar_dir}/plotter_NB.jar:${dist_jar_dir}/CodeGenCmdLine.jar:${CLASSPATH}";
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

    \rm -rf diag_NB_lib
    mkdir diag_NB_lib
    
    if test "x${DEBUG}" != "x" ; then
	pwd;
    fi

    jsources=`cat ${JSOURCE_REL_DIR}diag_NB_sources.txt | awk '{printf("%s%s"," '"${JSOURCE_REL_DIR}"'",$1);}'` 
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


	( set -x ; "${JAVAC}" -cp class_dir  -d diag_NB_lib ${jsources} ) || exit 0


    else

	echo
	echo


	( set -x ; "${JAVAC}" -classpath "${CLASSPATH}" -d diag_NB_lib ${jsources} ) || exit 0
	
    fi

    cp ${JSOURCE_REL_DIR}diagapplet/diag_NBJarInfo.txt diag_NB_lib
    cp ${JSOURCE_REL_DIR}diagapplet/*.{png,gif,jpg} diag_NB_lib/diagapplet/
    cp ${JSOURCE_REL_DIR}diagapplet/plotter/*.{png,gif,jpg} diag_NB_lib/diagapplet/plotter/
    cd diag_NB_lib
    echo
    echo
    "${JAR}"  -cmf0 diag_NBJarInfo.txt diag_NB.jar diagapplet || exit 0
    chmod a+rx diag_NB.jar
    cd ..

    if test -d ${JSOURCE_REL_DIR}../../src -a -w ${JSOURCE_REL_DIR}../../src ; then
	mkdir -p ${JSOURCE_REL_DIR}../../plat/java/lib
	if test -d ${JSOURCE_REL_DIR}../../plat/java/lib -a -w ${JSOURCE_REL_DIR}../../plat/java/lib ; then
	    cp -p "${third_party_jar_dir}"/*.jar  ${JSOURCE_REL_DIR}../../plat/java/lib;
	    cp -p diag_NB_lib/diag_NB.jar ${JSOURCE_REL_DIR}../../plat/java/lib;
	fi
	mkdir -p ${JSOURCE_REL_DIR}../../bin
	if test -d ${JSOURCE_REL_DIR}../../bin -a -w ${JSOURCE_REL_DIR}../../bin ; then
	    cp -p "${third_party_jar_dir}"/*.jar  ${JSOURCE_REL_DIR}../../bin;
	    cp -p diag_NB_lib/diag_NB.jar ${JSOURCE_REL_DIR}../../bin;
	fi

    elif test -d ../../src -a -w ../../src ; then
	mkdir -p ../../plat/java/lib
	if test -d ../../plat/java/lib -a -w ../../plat/java/lib ; then
	    cp -p "${third_party_jar_dir}"/*.jar ../../plat/java/lib;
	    cp -p diag_NB_lib/diag_NB.jar ../../plat/java/lib;
	fi
	mkdir -p ../../bin
	if test -d ../../bin -a -w ../../bin ; then
	    cp -p "${third_party_jar_dir}"/*.jar ../../bin;
	    cp -p diag_NB_lib/diag_NB.jar ../../bin;
	fi

    fi

    if test "x${d}" != "x" -a "x${d}" != "x./" -a ${JSOURCE_REL_DIR}x = "x" ; then
	cur_dir=`pwd`;
	if test "x${cur_dir}" != "x${orig_d}" ; then
	    cp -p "${third_party_jar_dir}"/*.jar "${orig_d}";
	    cp -p diag_NB_lib/diag_NB.jar "${orig_d}";
	else
	    cp -p diag_NB_lib/diag_NB.jar .;
	    top_dir=`(cd ${d}; cd ../.. ; pwd )`;
	    mkdir -p "${top_dir}/plat/java/lib"
	    if test -d "${top_dir}/plat/java/lib" -a -w "${top_dir}/plat/java/lib" ; then
		cp -p "${third_party_jar_dir}"/*.jar "${top_dir}/plat/java/lib";
		cp -p diag_NB_lib/diag_NB.jar "${top_dir}/plat/java/lib";
	    fi
	    mkdir -p "${top_dir}/bin/"
	    if test -d "${top_dir}/bin" -a -w "${top_dir}/bin" ; then
		cp -p "${third_party_jar_dir}"/*.jar "${top_dir}/bin";
		cp -p diag_NB_lib/diag_NB.jar "${top_dir}/bin";
	    fi
	    if test -f "${HOME}/DO_BUILD_JARS_CLEAN" -o "x${HOME}" = "x/tmp/distcheckhome" ; then
		set -x;
		echo "Cleaning up after building jars."
		echo "2: Cleaning up after building jars." >&2
		\rm -rf diag_NB_sources.txt;
		\rm -rf *.sh;
		\rm -rf rcs
		\rm -rf diagapplet;
		\rm -rf diag_NB_lib;
	    fi
	fi
    fi


    \rm -rf *_lib ;
    \rm -rf class_dir;

    if test -x "${HOME}/bin/sign_rcsjava.sh" -a -f "${HOME}/myKeys"; then
	"${HOME}/bin/sign_rcsjava.sh"
    fi

fi


echo "End $0 $*"

if test "x${DEBUG}" != "x" ; then
    echo "2: End $0 $*" >&2
fi


