#! /bin/sh

set -x

dir=$1

if test "x${dir}" = "x" ; then 
    dir=etc/win_vc++_2008_express;
fi

if test '!' -d  etc; then
    mkdir etc;
fi

if test '!' -d  "${dir}"; then
    mkdir "${dir}";
fi

if test '!' -d  "${dir}"/rcs; then
    mkdir "${dir}"/rcs;
fi

cc_list=`find src -name \*.cc`;

for cc_file in ${cc_list} ; do
    fname=${cc_file##*/}
    base=${fname%%.cc}
    cp -f ${cc_file} "${dir}"/rcs/${base}.cpp;
done

c_list=$(find src -name \*.c)
cp -f ${c_list} "${dir}"/rcs

h_list=$(find src -name \*.h)
cp -f ${h_list} "${dir}"/rcs

hh_list=$(find src -name \*.hh)
cp -f ${hh_list} "${dir}"/rcs



