#! /bin/sh

# This script was contributed by Rashmi Patel, of GDRS 
# it should be used with the project/workspace files 
# under etc/winvnet and Microsoft Visual C++ .net
# for older Visual Studio C++ 6.0 see cc_to_cpp.sh

set -x;

if test '!' -d  plat; then
    mkdir plat;
fi

if test '!' -d  plat/win32; then
    mkdir plat/win32;
fi

if test '!' -d  plat/win32/include; then
    mkdir plat/win32/include;
fi

if test '!' -d  plat/win32/src; then
    mkdir plat/win32/src;
fi

if test '!' -d  plat/win32/lib; then
    mkdir plat/win32/lib;
fi

cc_list=`find src -name \*.cc`;

for cc_file in ${cc_list} ; do
    fname=${cc_file##*/}
    base=${fname%%.cc}
    cp -f ${cc_file} plat/win32/src/${cc_file};
done

c_list=$(find src -name \*.c)
cp -f ${c_list} plat/win32/src

h_list=$(find src -name \*.h)
cp -f ${h_list} plat/win32/include

hh_list=$(find src -name \*.hh)
cp -f ${hh_list} plat/win32/include



