#! /bin/sh

set -x 
pwd

if test -x rcs/buildme.sh ; then
    (set -x ; cd rcs ; ./buildme.sh ) ;
fi

if test -x diagapplet/buildme.sh ; then
    (set -x ; cd diagapplet; ./buildme.sh ) ;
fi


