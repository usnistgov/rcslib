#! /bin/sh

set -x;

echo "Running $0 $* . . . from " `pwd`;
echo "2: Running $0 $* . . . from " `pwd` >&2;

if test "x${d}" != "x" -a "x${d}" != "x./" ; then
    echo "Removing previously copied files."
    ls
    \rm -rf rcs diagapplet *_sources.txt *.sh;
    ls
fi

orig_dir=`pwd`;
true_d=.
d=`echo $0 | sed s'#build_jars.sh##'`;
if test "x${d}" != "x" -a "x${d}" != "x./" ; then
    true_d=`(cd $d ; pwd)`
fi


"${true_d}/build_rcs_jar.sh" || exit 1
#"${true_d}/build_plotter_jar.sh" || exit 1
"${true_d}/build_CodeGenCmdLine_jar.sh" || exit 1
#"${true_d}/build_jdiag_jar.sh" || exit 1
#"${true_d}/build_diagapplet_jar.sh" || exit 1
"${true_d}/build_plotter_NB_jar.sh" || exit 1
PLOTTER_NB_JAR_UPTODATE=1;
RCS_JAR_UPTODATE=1;
CodeGenCmdLine_JAR_UPTODATE=1;
export PLOTTER_NB_JAR_UPTODATE;
export RCS_JAR_UPTODATE;
export CodeGenCmdLine_JAR_UPTODATE;
"${true_d}/build_diag_NB_jar.sh"

echo
echo "Executing NetBeans ant script  . . . ";

if test "x${NO_ANT}" = "x" ; then
    ( set -x; if test "x${true_d}" != "x" ; then cd "${true_d}" ; fi ; \
	cd ../../NetBeans/rcsjava ; env JAVA_HOME=`../../etc/jdk_dir.sh` ant jar ; env JAVA_HOME=`../../etc/jdk_dir.sh` ant javadoc )
fi

echo "Done executing NetBeans ant script. ";
echo

cur_dir=`pwd`;


true_d2=`(cd ${true_d}; pwd;)`

echo "true_d=${true_d}"
echo "true_d2=${true_d2}"
echo "cur_dir=${cur_dir}";

if test "x${true_d}" != "x" -a \
    "x${true_d}" != "x./" -a \
    "x${true_d}" != "x." -a \
    "${true_d}x" != "x${cur_dir}" -a \
    "${true_d2}x" != "x${cur_dir}" ; then
    if test -d "${true_d}" ; then
	if test -f "${HOME}/DO_BUILD_JARS_CLEAN" -o "x${HOME}" = "x/tmp/distcheckhome" ; then
	    (
		set -x ; \
		cd "${true_d}"
		\rm -rf rcs; \
		\rm -rf diagapplet; \
		\rm -f *_sources.txt; \
		\rm -rf plotter_lib; \
		\rm -rf CodeGenCmdLine_lib; \
		\rm -rf diag_NB_lib; \
		);
	fi
   fi
fi

javadoc_d=`(cd ${true_d} ; cd ../../doc/javadoc ; pwd)`;
 
javadoc -sourcepath "${true_d}" -d "${javadoc_d}" -subpackages rcs:diagapplet:rcsdesign
(cd "${javadoc_d}" ; zip ../rcslib_javadoc.zip `find . -type f | grep -v .svn` )


echo "End of $0 $* . . ."
echo "2: End of $0 $* . . ." >&2 

