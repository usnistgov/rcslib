#! /bin/bash 

if test "x${DEBUG}" != "x" ; then
    set -x;
    echo "Running $0 $*";
    pwd
fi

t=${0##*/}
d=${0%$t}
if test "x${d}" = "x" ; then
    prog=`which $t | grep $t`
    rcslib_etc_dir=${prog%/*}
else
    rcslib_etc_dir=$d;
fi
  
rcslib_dir=`(cd ${rcslib_etc_dir} ; cd .. ; pwd )`;

#echo rcslib_dir = $rcslib_dir


if test "x${JAVA_COMMAND}" = "x" ; then
	if test "x${JRE_DIR}" = "x" ; then
		JRE_DIR=`$rcslib_etc_dir/jre_dir.sh`
	fi
	if test "x${JRE_DIR}" != "x" -a -x "${JRE_DIR}/bin/java" ; then
	    JAVA_COMMAND="${JRE_DIR}/bin/java";
	else		
	    JAVA_COMMAND=java;
	fi

	if test '(' '!' -x `which ${JAVA_COMMAND}` ')'  -o '('  -d `which ${JAVA_COMMAND}` ')' ; then

	    if test "x${JDK_DIR}" = "x" ; then
		JDK_DIR=`$rcslib_etc_dir/jdk_dir.sh`
	    fi
	    
	    if test -x "${JDK_DIR}/bin/${JAVA_COMMAND}" ; then
		JAVA_COMMAND="${JDK_DIR}/bin/${JAVA_COMMAND}";
	    fi
	    
	fi
fi

if test "x${JDK_DIR}" != "x" ; then
    for jli_dir in `find -L "${JDK_DIR}" -name jli -type d` ; do
	LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:${jli_dir}"
    done
    for lib_dir in `find -L "${JDK_DIR}" -name lib -type d` ; do
	LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:${lib_dir}"
    done
    for i386_dir in `find -L "${JDK_DIR}" -name i386 -type d` ; do
	LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:${i386_dir}"
    done
fi

if test "x${JRE_DIR}" != "x" -a "x${JRE_DIR/jre/xxx}" != "x" ; then
    for jli_dir in `find -L "${JRE_DIR}" -name jli -type d` ; do
	LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:${jli_dir}"
    done
    for lib_dir in `find -L "${JRE_DIR}" -name lib -type d` ; do
	LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:${lib_dir}"
    done
    for i386_dir in `find -L "${JRE_DIR}" -name i386 -type d` ; do
	LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:${i386_dir}"
    done
fi


if test -f "${rcslib_dir}/plat/java/lib/diagapplet/CodeGen/CodeGenCmdLine.class" ; then
	CODEGEN_ARG="-classpath ${rcslib_dir}/plat/java/lib diagapplet.CodeGen.CodeGenCmdLine";
elif test -f "${rcslib_dir}/plat/java/lib/diagapplet/CodeGen/CodeGen.class" ; then	
	CODEGEN_ARG="-classpath ${rcslib_dir}/plat/java/lib diagapplet.CodeGen.CodeGen";
elif test -f "${rcslib_dir}/plat/java/lib/CodeGenCmdLine.jar" ; then
	CODEGEN_ARG="-jar ${rcslib_dir}/plat/java/lib/CodeGenCmdLine.jar";
elif test -f "${rcslib_dir}/plat/java/lib/CodeGen.jar" ; then
	CODEGEN_ARG="-jar ${rcslib_dir}/plat/java/lib/CodeGen.jar";
elif test -f "${rcslib_dir}/CodeGenCmdLine.jar" ; then
	CODEGEN_ARG="-jar ${rcslib_dir}/CodeGenCmdLine.jar";
elif test -f "${rcslib_dir}/bin/CodeGenCmdLine.jar" ; then
	CODEGEN_ARG="-jar ${rcslib_dir}/bin/CodeGenCmdLine.jar";
fi

export JAVA_COMMAND
export JDK_DIR
export JRE_DIR
export CODEGEN_ARG
export LD_LIBRARY_PATH

echo ${JAVA_COMMAND} ${CODEGEN_ARG} $*
${JAVA_COMMAND} ${CODEGEN_ARG} $*

#echo CodeGen exited.



