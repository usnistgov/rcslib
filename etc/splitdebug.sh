#/bin/sh


set -x; 

for i in $* ; do  
    ls -l "${i}"*
    if objdump -h "${i}" | grep debug ; then
	objcopy --only-keep-debug "${i}" "${i}.debug";
	objcopy --strip-debug "${i}";
	objcopy --add-gnu-debuglink="${i}.debug" "${i}";
    fi
    ls -l "${i}"*
done

