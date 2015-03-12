#!/bin/sh

if test "x${DEBUG}" = "x1" ; then
    set -x;
fi


d=${0%/*}
dir0=${0%/*};
if test ! -d ${dir0} ; then
    this_f=`which check_core_files.sh`
    script_dir=${this_f%/*};
    dir0=`pwd`;
    dir0_abs=`pwd`;
else
    dir0_abs=`cd $dir0 ; pwd`
fi



if test "x${script_dir}" = "x" ; then
    if test -x ./debug_core.sh ; then
	script_dir=./;
    elif test -x "${dir0_abs}/debug_core.sh" ; then
	script_dir="${dir0_abs}/";
    elif test -x "${RCSLIB_DIR}/bin/debug_core.sh" ; then
	script_dir="${RCSLIB_DIR}/bin/";
    elif test -x "${RCSLIB_DIR}/etc/debug_core.sh" ; then
	script_dir="${RCSLIB_DIR}/etc/";
    fi
fi

sudo_rm=rm
core_files=`echo core*`
if test "x${core_files}" != "x" -a "x${core_files}" != "xcore*" ; then
    echo "Core files are stored in bin directory:";
    for c in ${core_files} ; do
	ls -l $c ;
	if test -d "$c" ; then
	    continue;
	fi
	if test '!' -r $c ; then
	    sudo file $c;
	else
	    file $c;
	fi
	if test '!' -w $c ; then
	    sudo_rm="sudo rm"
	fi
	echo "Debug this core file(y/n)?"
	read confirm_debug;
	if test "x${confirm_debug}" = "xy" ; then
	    echo "${script_dir}debug_core.sh $c";
	    ${script_dir}debug_core.sh $c;
	fi
    done;
    echo "";
    echo "Delete all core files (y/n)?"
    read confirm_delete;
    if test "x${confirm_delete}" != "xy" ; then
	exit 1;
    fi
    ${sudo_rm} -f ${core_files};
fi

sudo_rm=rm
if test "x${PLAYBACK_DIR}" != "x" ; then
    core_files=`find ${PLAYBACK_DIR} -xdev -name core\*`
    if test "x${core_files}" != "x" -a "x${core_files}" != "xcore*" ; then
	echo "Core files are stored in bin directory:";
	for c in ${core_files} ; do
	    ls -l $c ;
	    if test -d "$c" ; then
		continue;
	    fi
	    if test '!' -r $c ; then
		sudo file $c;
	    else
		file $c;
	    fi
	    if test '!' -w $c ; then
		sudo_rm="sudo rm"
	    fi
	    echo "Debug this core file(y/n)?"
	    read confirm_debug;
	    if test "x${confirm_debug}" = "xy" ; then
		echo "${script_dir}debug_core.sh $c";
		${script_dir}debug_core.sh $c;
	    fi
	done;
	echo "";
	echo "Delete all core files (y/n)?"
	read confirm_delete;
	if test "x${confirm_delete}" != "xy" ; then
	    exit 1;
	fi
	${sudo_rm} -f ${core_files};
    fi
fi
