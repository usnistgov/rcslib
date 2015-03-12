#!/bin/sh


if test "x${RTLINUXDIR}" != "x" ; then
    echo -n ${RTLINUXDIR}
    exit 0
fi

if test ! -f .rtlinuxdir_guess_warn -a ! -f ~/.rtlinuxdir_guess_warn ; then
    echo "Trying to guess where  RTLINUX is installed." 1>&2
    echo "Set the environment variable RTLINUXDIR to override." 1>&2
    touch .rtlinuxdir_guess_warn >/dev/null 2>/dev/null
    touch ~/.rtlinuxdir_guess_warn >/dev/null 2>/dev/null
fi

if [ -f ~/.debug_scripts ] ; then
    set -x;
fi

if [ -d /usr/rtlinux -o -L /usr/rtlinux ] ; then
    echo -n /usr/rtlinux
    exit 0
fi

if [ -d /usr/src/rtlinux -o -L  /usr/src/rtlinux ] ; then
    echo -n /usr/src/rtlinux
    exit 0
fi

if [ -d /usr/src/rtl -o -L /usr/src/rtl ] ; then
    echo -n /usr/src/rtl
    exit 0
fi

if ls -d /usr/rtl* >/dev/null 2>/dev/null ; then
    echo -n `find /usr -name rtl\* | head -n 1`
    exit 0
fi

if ls -d /usr/src/rtl* >/dev/null 2>/dev/null ; then
    echo -n `find /usr/src -name rtl\* | head -n 1`
    exit 0
fi

exit -1


