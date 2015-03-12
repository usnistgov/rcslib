#!/bin/sh

libtoolize -f -c &&  exit 0

for i in /usr/local/share/libtool /usr/share/libtool /usr/local/libtool ; do 
    if [ -d $i -a -f $i/ltmain.sh -a -f $i/ltconfig -a -x $i/ltconfig  ] ; then
	echo Getting libtool files from  from $i
	\rm -f  config.* lt*
 	cp $i/config.* .
	cp $i/lt* .
	./ltconfig ./ltmain.sh
	./libtool --version
  	exit 0
    fi
done
