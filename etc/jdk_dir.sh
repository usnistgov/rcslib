#! /bin/bash

#set -x

if test "x${JDK_DIR}" = "x" ; then

if test -f "/tmp/jdk_${USER}_dir" ; then
    if date -r /tmp/jdk_${USER}_dir >/dev/null 2>/dev/null ; then
	jdate=`date +%s -r /tmp/jdk_${USER}_dir`;
	ndate=`echo $jdate | awk '{print $1+300}'`;
	cdate=`date +%s`;
	if test ${cdate} -lt ${ndate} ; then
	    JDK_DIR=`cat /tmp/jdk_${USER}_dir`;
	    if test "x${JDK_DIR}" != "x" ; then
		export JDK_DIR;
		echo "${JDK_DIR}";
		exit 0;
	    fi
	fi
   fi
fi

uname_s=`uname -s`;
uname_m=`uname -m`;
uname_r=`uname -r`;

JDK_LIST=
NEWEST_JDK=

if test -f "${HOME}"/.skip_icedtea_java ; then
    export SKIP_ICEDTEA_JAVA=1;
fi

if test -f "${HOME}"/.skip_java_1_7 ; then
    export SKIP_JAVA_1_7=1;
fi

if uname -s | grep "SunOS" >/dev/null 2>/dev/null ; then
    if test -d "/mxproj/rcslib/jdk/${uname_s}-${uname_m}-${uname_r}/" ; then 
	JDK_DIR="/mxproj/rcslib/jdk/${uname_s}-${uname_m}-${uname_r}/current_java/";
	export JDK_DIR;
	echo "${JDK_DIR}" >/tmp/jdk_${USER}_dir;
	echo "${JDK_DIR}"
	exit 0;
    fi
fi

if test -d /itl/apps ; then
    base_dir=/itl/apps
    if test "x${uname_s}" != "x" -a "x${uname_m}" != "x" -a "x${uname_r}" != "x" ; then
	for dir in "${base_dir}"/j?sdk* "${base_dir}"/jdk* end_dir_list ; do
	    if test "x${dir}" != "x"${base_dir}"/j?sdk\*" -a "x${dir}" != "x"${base_dir}"/jdk\*" -a "x${dir}" != "x" -a "x${dir}" != "xend_dir_list" ; then
		if test -x ${dir}/${uname_s}-${uname_m}-${uname_r}/bin/java -a -x ${dir}/${uname_s}-${uname_m}-${uname_r}/bin/javac ;  then
		    JDK_LIST="${JDK_LIST} ${dir}/${uname_s}-${uname_m}-${uname_r}";
		fi
	     fi
	 done
    fi
fi

PATH_LIST=`echo ${PATH} | sed 's#/bin:# #g' | sed 's#/bin/:# #g' | sed 's#/bin$# #' | sed 's#/bin/$# #'`

base_dir_list="${HOME} /usr/java /usr/lib/jvm /usr/lib /usr /usr/local /usr/share /usr/local/share /opt ${PATH_LIST}"

for base_dir in ${base_dir_list} ; do
    if test "x"${base_dir}"" != "x" ; then
	if test -d "${base_dir}"; then 
	    for dir in "${base_dir}"  "${base_dir}"/?urrent*ava "${base_dir}"/java-* "${base_dir}"/j?sdk* "${base_dir}"/jdk* end_dir_list ; do
		if test "x${dir}" != "x"${base_dir}"/j?sdk\*" -a "x${dir}" != "x"${base_dir}"/jdk\*" -a "x${dir}" != "x" -a "x${dir}" != "xend_dir_list" ; then
		    if test -x ${dir}/bin/java -a -x ${dir}/bin/javac ;  then
			JDK_LIST="${dir} ${JDK_LIST}";
		    fi
		fi
	    done
	fi
	if test -d "${base_dir}"/java; then 
	    for dir in "${base_dir}"/java  "${base_dir}"/?urrent*ava "${base_dir}"/java/j?sdk* "${base_dir}"/java/jdk*  end_dir_list ; do
		if test "x${dir}" != "x"${base_dir}"/java/j?sdk\*" -a "x${dir}" != "x"${base_dir}"x/java/jdk\*" -a "x${dir}" != "x" -a "x${dir}" != "xend_dir_list" ; then
		    if test -x ${dir}/bin/java -a -x ${dir}/bin/javac ;  then
			JDK_LIST="${dir} ${JDK_LIST}";
		    fi
		fi
	    done
	fi
    fi
done

#echo JDK_LIST=${JDK_LIST}
if test -f "/tmp/jdk_${USER}_list.txt" ; then
    rm "/tmp/jdk_${USER}_list.txt" >/dev/null 2>/dev/null;
fi
umask_orig=`umask 2>/dev/null`;
if test "x${umask_orig}" != "x" ; then
    umask 0;
fi

for dir in ${JDK_LIST} end_dir_list ; do
    if test "x${dir}" != "x" -a "x${dir}" != "xend_dir_list" ; then
	JDK_DIR=${dir};
	LAST_JAVAC_PROG="";
	JAVAC_PROG="${JDK_DIR}/bin/javac";
	while test "x${JAVAC_PROG}" != "x" -a -L "${JAVAC_PROG}"  -a "x${JAVAC_PROG}" != "x${LAST_JAVA_PROG}"; do
	   #ls -l "${JAVAC_PROG}";
	    LAST_JAVAC_PROG=${JAVAC_PROG};
	    JAVAC_PROG=`ls -l "${JAVAC_PROG}" | awk '{print $10}'`;
	   #echo "JAVAC_PROG=${JAVAC_PROG}";
	    if test "x${JAVAC_PROG%/bin/javac}" != "x" -a "x${JAVAC_PROG%/bin/javac}" != "x${JAVA_PROG}" ; then
		JDK_DIR="${JAVAC_PROG%/bin/javac}";
		#echo "JDK_DIR=${JDK_DIR}";
	    fi
	done
	if test "x${SKIP_ICEDTEA_JAVA}" != "x" -a "x${JDK_DIR}" != "x${JDK_DIR/icedtea/}"  ; then
	    continue;
	fi
	if test "x${SKIP_JAVA_1_7}" != "x" -a "x${JDK_DIR}" != "x${JDK_DIR/java-1.7/}"  ; then
	    continue;
	fi

	jdk_version=`${JDK_DIR}/bin/javac -version 2>&1 | grep javac | awk '{print $2}'`
	if test "x${SKIP_JAVA_1_7}" != "x" -a "x${jdk_version}" != "x${jdk_version/1.7/}"  ; then
	    continue;
	fi
	if test "${jdk_version}x" != "x" ; then 
	    echo "${jdk_version} ${JDK_DIR}" | awk '{printf("%s \t%s\n",$1,$2);}' >>/tmp/jdk_${USER}_list.txt
	    chmod ug+w "/tmp/jdk_${USER}_list.txt" >/dev/null 2>/dev/null;
	fi
    fi 
done		 

JDK_DIR=

if test -f "/tmp/jdk_${USER}_list.txt" ; then

    if test -f "/tmp/jdk_${USER}_sorted_list.txt" ; then
	rm "/tmp/jdk_${USER}_sorted_list.txt" >/dev/null 2>/dev/null;
    fi

    sort "/tmp/jdk_${USER}_list.txt" > "/tmp/jdk_${USER}_sorted_list.txt";
    chmod ug+w "/tmp/jdk_${USER}_sorted_list.txt" >/dev/null 2>/dev/null;

    tailcmd="tail -1"

    if tail -n 1 /tmp/tailtest.txt >/dev/null 2>/dev/null ; then

	if test -f /tmp/tailtest.txt ; then
	    rm /tmp/tailtest.txt >/dev/null 2>/dev/null;
	fi

	cat >/tmp/tailtest.txt <<EOF
1
2
3
EOF

	x=`tail -n 1`;
	if "x${x}" = "x3" ; then
	    tailcmd="tail -n 1";
	fi
    fi

    if test -f /tmp/tailtest.txt ; then
	rm /tmp/tailtest.txt >/dev/null 2>/dev/null;
    fi

    if test -f /tmp/jdk_${USER}_sorted_list.txt ; then
	NEWEST_JDK=`${tailcmd} /tmp/jdk_${USER}_sorted_list.txt | awk '{print($2);}'`
    fi

    JDK_DIR=${NEWEST_JDK}
    export JDK_DIR

fi
# if test -f /tmp/jdk_${USER}_list.txt 


fi
# if test "x${JDK_DIR}" = "x" ; then
# JDK_DIR not already set.

if test "x${JDK_DIR}" != "x" ; then 
    echo "${JDK_DIR}" >"/tmp/jdk_${USER}_dir";
    chmod ug+rw "/tmp/jdk_${USER}_dir" >/dev/null 2>/dev/null;
    chmod ug+rw "/tmp/jdk_${USER}_list.txt" >/dev/null 2>/dev/null;
    chmod ug+rw "/tmp/jdk_${USER}_sorted_list.txt" >/dev/null 2>/dev/null;
fi

if test "x${umask_orig}" != "x" ; then
    umask "${umask_orig}";
fi

echo ${JDK_DIR}

