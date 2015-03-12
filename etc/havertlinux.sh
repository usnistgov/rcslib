#!/bin/sh


if test "x${HAVE_RTLINUX}" != "x" ; then
    echo ${HAVE_RTLINUX}
    exit 0
fi
UNAME_A=`uname -a`
FULL_UNAME=`echo "${UNAME_A}_${CC}" | sed 'y# #_#' | sed 'y#/#_#' | sed 'y#-#_#' | sed 'y#:#_#' 2>/dev/null` 2>/dev/null

if test "x${FULL_UNAME}" != "x" ; then
    if test -f ~/.have_rtlinux.${FULL_UNAME} ; then
	HAVE_RTLINUX=`head -n 1 ~/.have_rtlinux.${FULL_UNAME} 2>/dev/null` 2>/dev/null
	export HAVE_RTLINUX;
    fi
fi

if test "x${HAVE_RTLINUX}" != "x" ; then 
    echo ${HAVE_RTLINUX}
    exit 0
fi

if test ! -f .rtlinux_guess_warn -a ! -f ~/.rtlinux_guess_warn ; then
    echo "Trying to determine if RTLINUX is installed." 1>&2
    echo "Set the environment variable HAVE_RTLINUX to YES or NO to override." 1>&2
    touch .rtlinux_guess_warn >/dev/null 2>/dev/null
    touch ~/.rtlinux_guess_warn >/dev/null 2>/dev/null
fi



    

ECHO_CMD=echo
# I would like to make this echo -n which eliminates annecessary and troublesome
# newline unfortunately on some platforms this causes the whole line to be lost.
if [ -e ~/.no_rtl -o -e ./.no_rtl ] ; then
    NO_RTL=1
    export NO_RTL;
fi

if [ -e ~/.no_realtime -o -e ./.no_realtime ] ; then
    NO_REALTIME=1
    export NO_REALTIME;
fi

if [ "x${NO_RTL}" != "x" ] ; then
    $ECHO_CMD "NO"
    exit 0
fi

if [ "x${NO_REALTIME}" != "x" ] ; then
    $ECHO_CMD "NO"
    exit 0
fi



if uname -a | grep Linux > /dev/null 2> /dev/null  ; then 
    if test ! -x /sbin/ksyms  ; then
	HAVE_RTLINUX=NO;
    elif  /sbin/ksyms -a | grep rtlinux > /dev/null 2> /dev/null; then 
	HAVE_RTLINUX=YES    
    elif  /sbin/ksyms -a | grep rtl_get_soft_irq > /dev/null 2> /dev/null; then 
	HAVE_RTLINUX=YES
    else 
	HAVE_RTLINUX=NO
    fi
else
    HAVE_RTLINUX="NO  -- NOT Linux"
fi

export HAVE_RTLINUX

if test "x${FULL_UNAME}" != "x" ; then
    \rm -f ~/.have_rtlinux.* 2>/dev/null  ;
    echo ${HAVE_RTLINUX} > ~/.have_rtlinux.${FULL_UNAME} 2>/dev/null ;
fi

${ECHO_CMD} ${HAVE_RTLINUX}
exit 0

