#! /bin/sh

set -x

if [ ! -d etc -o ! -d src -o ! -f configure.ac -o ! -x etc/findsources.sh  -o ! -f src/rcsvers.hh ] ; then
    echo "This script is being run from the wrong directory"
    exit -1
fi

if [ ! -w . -o ! -w etc ] ; then
    echo "You need write permission to run this script."
fi

#find etc -lname \*.def -exec rm {} \; 

# etc/findsources.sh && \

aclocal && autoheader && automake && \
autoconf && ./configure && make
