#!/bin/sh

if test "x${DEBUG}" = "x1" ; then
    set -x;
fi

orig_dir=`pwd`;

d=${0%/*}

dir0=${0%/*};
if test ! -d ${dir0} ; then
    this_f=`which debug_core.sh`
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

if test $# -lt 1 ; then
    echo "Usage: pass a core file.";
    cfin=`ls -1 core* | head -n 1`;
else 
    cfin=$1;
fi

core_dir=${cfin%/*};
if test "x${core_dir}" != "x" -a "x${core_dir}" != "x." -a "x${core_dir}" != "x${1%%/*}" ; then
    core_dir=`(cd ${core_dir}; pwd)`;
    core_f=${cfin##*/};
    core="${core_dir}/${core_f}";
else
    core=$cfin;
    core_dir=`pwd`
    core="${core_dir}/${cfin}";
fi


execfile=`${script_dir}execfile_from_core.sh ${core}`;

if test '!' -r ${core} ; then
    echo "sudo gdb ${execfile} ${core}"
    sudo gdb --exec=${execfile} --core=${core}
else
    echo "gdb --exec=${execfile} --core=${core}"
    gdb ${execfile} ${core}
fi


