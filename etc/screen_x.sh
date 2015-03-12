#! /bin/sh

if test "x${DEBUG}" = "x1" ; then
    echo "Running $0 $* from : "  `pwd`;
    set -x;
fi


#set -x;

dir0=${0%/*};
dir0_abs=`cd $dir0 ; pwd`

if test "x${script_dir}" = "x" ; then
    if test -x ./run_cmd_and_wait.sh ; then
	script_dir=./;
    elif test -x "${dir0_abs}/run_cmd_and_wait.sh" ; then
	script_dir="${dir0_abs}/";
    elif test -x "${RCSLIB_DIR}/bin/run_cmd_and_wait.sh" ; then
	script_dir="${RCSLIB_DIR}/bin/";
    elif test -x "${RCSLIB_DIR}/etc/run_cmd_and_wait.sh" ; then
	script_dir="${RCSLIB_DIR}/etc/";
    fi
fi

name=
command=

next_arg_is_command=false
next_arg_is_name=false
first_cmd_arg=

for arg in $* ; do
    if test "x${arg}" = "x-n" ; then
	next_arg_is_name=true
    elif test "x${arg}" = "x-e" ; then
	next_arg_is_command=true;
	next_arg_is_name=false;
    elif test "x${next_arg_is_command}" = "xtrue" ; then
	if test "x${command}" != "x" ; then
	    command="${command} ${arg}";
	else
	    command="${arg}";
	    first_cmd_arg="${arg}";
	fi
	next_arg_is_name=false;
    elif test "x${next_arg_is_name}" = "xtrue" ; then
	name="${arg}";
	next_arg_is_name=false;
    fi
done

if test "x${NO_SCREEN}" != "x" ; then
    ${script_dir}run_cmd_and_wait.sh ${command} &
    exit 0;
fi

if test "x${name}" = "x" ; then
    name=`echo ${first_cmd_arg} | sed 's#^.*/##'`
fi

if test "x${command}" != "x" ; then
    export RCAW_LL_PATH="${LD_LIBRARY_PATH}";
    if test "x${name}" = "x" ; then
	screen ${SCREEN_OPTS} -d -m ${script_dir}run_cmd_and_wait.sh ${command};
    else
	screen ${SCREEN_OPTS} -S ${name} -t ${name} -d -m ${script_dir}run_cmd_and_wait.sh ${command};
    fi
else
    echo "Set the command with -e <command>.";
    exit 1;
fi


