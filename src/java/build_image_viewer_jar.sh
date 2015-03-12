#! /bin/sh

echo "Running $0 $* . . . from " `pwd`;

if test "x${DEBUG}" != "x" ; then
    set -x;
fi

orig_d=`pwd`;
d=`echo $0 | sed 's#build_image_viewer_jar.sh##'`;
if test "x${d}" != "x" -a "x${d}" != "x./" -a "${JSOURCE_REL_DIR}x" = "x" ; then
    true_d=`(cd $d ; pwd)`
    if test -w "${true_d}" ; then
	cd "${true_d}";
    else
	cp "${true_d}"/image_viewer_sources.txt .
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


needed_jars="";



before_3rd_party_cd_dir=`pwd`;
if test "x${JSOURCE_REL_DIR}" != "x" ; then
    cd ${JSOURCE_REL_DIR};
fi

rel_dist_jar_dir=../../plat/java/lib;
dist_jar_dir=`cd ${rel_dist_jar_dir}; pwd`;

if test -d "../../3rd_party_jars" ; then
    cd ../../3rd_party_jars ; 
    
    if test "x${DEBUG}" != "x" ; then
	pwd;
    fi

    for jar in *.jar ; do
	if test '!' -f "${dist_jar_dir}/${jar}" ; then
	    cp -p $jar "${dist_jar_dir}";
	fi
	needed_jars="${needed_jars}:${dist_jar_dir}/${jar}";
    done;

fi
cd "${before_3rd_party_cd_dir}";

if test "x${DEBUG}" != "x" ; then
    pwd;
fi

\rm -rf image_viewer_lib
mkdir image_viewer_lib

if test "x${DEBUG}" != "x" ; then
    pwd;
fi

jsources=`cat ${JSOURCE_REL_DIR}image_viewer_sources.txt | awk '{printf("%s%s"," '"${JSOURCE_REL_DIR}"'",$1);}'` 
( set -x ; "${JAVAC}" -classpath "${needed_jars}" -d image_viewer_lib ${jsources} ) || exit 0
cp ${JSOURCE_REL_DIR}diagapplet/utils/image_viewerJarInfo.txt image_viewer_lib
mkdir -p image_viewer_lib/diagapplet/plotter/
cp ${JSOURCE_REL_DIR}diagapplet/plotter/*.{gif,jpg,png} image_viewer_lib/diagapplet/plotter/

cd image_viewer_lib
"${JAR}"  -cmf0 image_viewerJarInfo.txt image_viewer.jar diagapplet rcs || exit 0
chmod a+rx image_viewer.jar
cd ..


if test -d ${JSOURCE_REL_DIR}../../src -a -w ${JSOURCE_REL_DIR}../../src ; then
    mkdir -p ${JSOURCE_REL_DIR}../../plat/java/lib
    if test -d ${JSOURCE_REL_DIR}../../plat/java/lib -a -w ${JSOURCE_REL_DIR}../../plat/java/lib ; then
	cp -p image_viewer_lib/image_viewer.jar ${JSOURCE_REL_DIR}../../plat/java/lib;
    fi
elif test -d ../../src -a -w ../../src ; then
    mkdir -p ../../plat/java/lib
    if test -d ../../plat/java/lib -a -w ../../plat/java/lib ; then
	cp -p image_viewer_lib/image_viewer.jar ../../plat/java/lib;
    fi
fi

if test "x${d}" != "x" -a "x${d}" != "x./" -a ${JSOURCE_REL_DIR}x = "x" ; then
    cur_dir=`pwd`;
    if test "x${cur_dir}" != "x${orig_d}" ; then
	cp -p image_viewer_lib/image_viewer.jar "${orig_d}";
    else
	cp -p image_viewer_lib/image_viewer.jar .;
	top_dir=`(cd ${d}; cd ../.. ; pwd )`;
	mkdir -p "${top_dir}/plat/java/lib" ;
	if test -d "${top_dir}/plat/java/lib" -a -w "${top_dir}/plat/java/lib" ; then
	    cp -p image_viewer_lib/image_viewer.jar "${top_dir}/plat/java/lib";
	fi
	if test -f "${HOME}/DO_BUILD_JARS_CLEAN" -o "x${HOME}" = "x/tmp/distcheckhome" ; then
	    \rm -rf *_sources.txt;
	    \rm -rf rcs
	    \rm -rf diagapplet;
	    \rm -rf image_viewer_lib;
	fi
    fi
fi


\rm -rf *_lib;

echo "End $0 $* ... ";
