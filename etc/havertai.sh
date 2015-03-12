#!/bin/sh

if test "x${HAVE_RTAI}" != "x" ; then
    echo ${HAVE_RTAI}
    exit 0
fi

UNAME_A=`uname -a`
FULL_UNAME=`echo "${UNAME_A}_${CC}" | sed 'y# #_#' | sed 'y#/#_#' | sed 'y#-#_#' | sed 'y#:#_#' 2>/dev/null` 2>/dev/null

if test "x${FULL_UNAME}" != "x" ; then
    if test -f ~/.have_rtai.${FULL_UNAME} ; then
	HAVE_RTAI=`head -n 1 ~/.have_rtai.${FULL_UNAME} 2>/dev/null` 2>/dev/null
	export HAVE_RTAI;
    fi
fi

if test "x${HAVE_RTAI}" != "x" ; then 
    echo ${HAVE_RTAI}
    exit 0
fi

if test ! -f .rtai_guess_warn -a ! -f ~/.rtai_guess_warn ; then
    echo "Trying to determine if RTAI is installed." 1>&2
    echo "Set the environment variable HAVE_RTAI to YES or NO to override." 1>&2
    touch .rtai_guess_warn >/dev/null 2>/dev/null
    touch ~/.rtai_guess_warn >/dev/null 2>/dev/null
fi

ECHO_CMD=echo
# I would like to make this echo -n which eliminates annecessary and troublesome
# newline unfortunately on some platforms this causes the whole line to be lost.

if [ -e ~/.no_rtai -o -e ./.no_rtai ] ; then
    NO_RTAI=1
    export NO_RTAI;
fi

if [ -e ~/.no_realtime -o -e ./.no_realtime ] ; then
    NO_REALTIME=1
    export NO_REALTIME;
fi

if [ "x${NO_RTAI}" != "x" ] ; then
    $ECHO_CMD "NO"
    \rm -f ~/.have_rtai.* 2>/dev/null  ;
    exit 0
fi

if [ "x${NO_REALTIME}" != "x" ] ; then
    $ECHO_CMD "NO"
    \rm -f ~/.have_rtai.* 2>/dev/null  ;
    exit 0
fi


if uname -a | grep Linux > /dev/null 2> /dev/null  ; then 
    if  test -x /sbin/ksyms && /sbin/ksyms -a | grep rthal > /dev/null 2> /dev/null; then 
	HAVE_RTAI=YES
    else 
	HAVE_RTAI=NO
    fi

else
    HAVE_RTAI=NO
fi

if test "x${FULL_UNAME}" != "x" ; then
    \rm -f ~/.have_rtai.* 2>/dev/null  ;
    echo ${HAVE_RTAI} > ~/.have_rtai.${FULL_UNAME} 2>/dev/null ;
fi

${ECHO_CMD} ${HAVE_RTAI}
exit 0




