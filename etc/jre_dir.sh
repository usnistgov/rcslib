#!/bin/bash

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

rcslib_dir=${rcslib_etc_dir%/*}

if test -f /tmp/jre_${USER}_dir ; then
    if date -r /tmp/jre_${USER}_dir >/dev/null 2>/dev/null ; then
	jdate=`date +%s -r /tmp/jre_${USER}_dir`;
	ndate=`echo $jdate | awk '{print $1+300}'`;
	cdate=`date +%s`;
	if test ${cdate} -lt ${ndate} ; then
	    JRE_DIR=`cat /tmp/jre_${USER}_dir`;
	    if test "x${JRE_DIR}" != "x" ; then
		export JRE_DIR;
		echo "${JRE_DIR}";
		exit 0;
	    fi
	fi
   fi
fi

uname_s=`uname -s`;
uname_m=`uname -m`;
uname_r=`uname -r`;

JRE_LIST=
NEWEST_JRE=

if test -f "${HOME}"/.skip_icedtea_java ; then
    export SKIP_ICEDTEA_JAVA=1;
fi

if test -f "${HOME}"/.skip_java_1_7 ; then
    export SKIP_JAVA_1_7=1;
fi

if uname -s | grep "SunOS" >/dev/null 2>/dev/null ; then

    if test -d "/mxproj/rcslib/jre/${uname_s}-${uname_m}-${uname_r}/" ; then 
	JRE_DIR="/mxproj/rcslib/jre/${uname_s}-${uname_m}-${uname_r}/current_java/";
	export JRE_DIR;
	echo "${JRE_DIR}" >/tmp/jre_${USER}_dir;
	echo "${JRE_DIR}"
	exit 0;
    fi

    if test -d /itl/apps ; then
	base_dir=/itl/apps
	if test "x${uname_s}" != "x" -a "x${uname_m}" != "x" -a "x${uname_r}" != "x" ; then
	    for dir in "${base_dir}"/jre* end_dir_list ; do
		if test "x${dir}" != "x"${base_dir}"/jre\*" -a "x${dir}" != "x" -a "x${dir}" != "xend_dir_list" ; then
		    if test -x ${dir}/${uname_s}-${uname_m}-${uname_r}/bin/java ; then
			JRE_LIST="${JRE_LIST} ${dir}/${uname_s}-${uname_m}-${uname_r}";
		    fi
		fi
	    done
	fi
    fi
fi

PATH_LIST=`echo ${PATH} | sed 's#/bin:# #g' | sed 's#/bin/:# #g' | sed 's#/bin$# #' | sed 's#/bin/$# #'`

parent_dir=`( cd .. ; pwd )`;
cur_dir=`pwd`;

base_dir_list="${cur_dir} ${parent_dir} ${HOME} /usr/java /usr/lib/jvm /usr/lib /usr /usr/local /usr/share /usr/local/share /opt ${PATH_LIST}"

for base_dir in ${base_dir_list} ; do
    if test "x"${base_dir}"" != "x" ; then
	if test -d "${base_dir}"; then 
	    for dir in "${base_dir}"  "${base_dir}"/?urrent*ava "${base_dir}"/jre* end_dir_list ; do
		if test "x${dir}" != "x"${base_dir}"/jre\*" -a "x${dir}" != "x" -a "x${dir}" != "xend_dir_list" ; then
		    if test -x ${dir}/bin/java ;  then
			JRE_LIST="${dir} ${JRE_LIST}";
		    fi
		fi
	    done
	fi
	if test -d "${base_dir}"/java; then 
	    for dir in "${base_dir}"/java  "${base_dir}"/?urrent*ava "${base_dir}"/java/j?sdk* "${base_dir}"/java/jre*  end_dir_list ; do
		if test "x${dir}" != "x"${base_dir}"/java/j?sdk\*" -a "x${dir}" != "x"${base_dir}"x/java/jre\*" -a "x${dir}" != "x" -a "x${dir}" != "xend_dir_list" ; then
		    if test -x ${dir}/bin/java ;  then
			JRE_LIST="${dir} ${JRE_LIST}";
		    fi
		fi
	    done
	fi
    fi
done

jdk_dir=`${rcslib_etc_dir}/jdk_dir.sh`;
if test "x${jdk_dir}" != "x" -a -x "${jdk_dir}/bin/java" ; then
    JRE_LIST="${jdk_dir} ${JRE_LIST} ${jdk_dir}";
fi

#echo JRE_LIST=${JRE_LIST}
if test -f /tmp/jre_${USER}_list.txt ; then
    rm /tmp/jre_${USER}_list.txt >/dev/null 2>/dev/null;
fi
umask_orig=`umask 2>/dev/null`;
if test "x${umask_orig}" != "x" ; then
    umask 0;
fi

for dir in ${JRE_LIST} end_dir_list ; do
    if test "x${dir}" != "x" -a "x${dir}" != "xend_dir_list" ; then
       JRE_DIR=${dir};
       LAST_JAVA_PROG="";
       JAVA_PROG="${JRE_DIR}/bin/java";
       while test -L "${JAVA_PROG}"  -a "x${JAVA_PROG}" != "x${LAST_JAVA_PROG}"; do
	   #ls -l "${JAVA_PROG}";
	   LAST_JAVA_PROG=${JAVA_PROG};
	   JAVA_PROG=`ls -l "${JAVA_PROG}" | awk '{print $10}'`;
	   #echo "JAVA_PROG=${JAVA_PROG}";
	   if test "x${JAVA_PROG%/bin/java}" != "x" -a "x${JAVA_PROG%/bin/java}" != "x${JAVA_PROG}" ; then
	       JRE_DIR="${JAVA_PROG%/bin/java}";
	       #echo "JRE_DIR=${JRE_DIR}";
	   fi
       done
	if test "x${SKIP_ICEDTEA_JAVA}" != "x" -a "x${JRE_DIR}" != "x${JRE_DIR/icedtea/}"  ; then
	    continue;
	fi
	if test "x${SKIP_JAVA_1_7}" != "x" -a "x${JRE_DIR}" != "x${JRE_DIR/java-1.7/}"  ; then
	    continue;
	fi 
	jre_version=`${JRE_DIR}/bin/java -version 2>&1 | grep version | grep java | sed 's#"##g' | awk '{print $3}'`

	if test "x${SKIP_JAVA_1_7}" != "x" -a "x${jre_version}" != "x${jre_version/1.7/}"  ; then
	    continue;
	fi

	JRE_DIR=`echo ${JRE_DIR} | sed 's#//#/#g'`;
       if test "${jre_version}x" != "x" ; then 
	    echo "${jre_version}0 ${JRE_DIR}" | awk '{printf("%s \t%s\n",$1,$2);}' >>/tmp/jre_${USER}_list.txt
	    chmod ug+w /tmp/jre_${USER}_list.txt >/dev/null 2>/dev/null;
       fi
    fi 
done		 

JRE_DIR=

if test -f /tmp/jre_${USER}_list.txt ; then

    if test -f /tmp/jre_${USER}_sorted_list.txt ; then
	rm /tmp/jre_${USER}_sorted_list.txt >/dev/null 2>/dev/null;
    fi

    sort /tmp/jre_${USER}_list.txt >/tmp/jre_${USER}_sorted_list.txt
    chmod ug+w /tmp/jre_${USER}_sorted_list.txt >/dev/null 2>/dev/null;

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

    if test -f /tmp/jre_${USER}_sorted_list.txt ; then
	NEWEST_JRE=`${tailcmd} /tmp/jre_${USER}_sorted_list.txt | awk '{print($2);}'`
    fi

    JRE_DIR=${NEWEST_JRE}
    export JRE_DIR

fi
# if test -f /tmp/jre_${USER}_list.txt 

if test "x${JRE_DIR}" != "x" ; then
    echo "${JRE_DIR}" >/tmp/jre_${USER}_dir;
    chmod ug+rw /tmp/jre_${USER}_dir >/dev/null 2>/dev/null;
    chmod ug+rw /tmp/jre_${USER}_list.txt >/dev/null 2>/dev/null;
    chmod ug+rw /tmp/jre_${USER}_sorted_list.txt >/dev/null 2>/dev/null;
fi

if test "x${umask_orig}" != "x" ; then
    umask "${umask_orig}";
fi

echo ${JRE_DIR}

