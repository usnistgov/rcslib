#!/bin/sh


if test "x${TCLTKINCFLAGS}" != "x" ; then
    echo ${TCLTKINCFLAGS}
    exit 0
fi

if test ! -f .tcltkincflags_guess_warn -a ! ~.tcltkincflags_guess_warn ; then 
    echo "Trying to guess what flags need to be used to compile C programs that interact with tcl/tk." 1>&2
    echo "Set the environment variable TCLTKINCFLAGS to override this." 1>&2
    touch  .tcltkincflags_guess_warn >/dev/null 2>/dev/null
    touch  ~/.tcltkincflags_guess_warn >/dev/null 2>/dev/null
fi


if [ -f ~/.debug_scripts ] ; then
 set -x
fi


FINDRESULTS=`find /usr/include -name tcl.h 2>/dev/null`
TCL_IFLAGS= 
if [ "x${FINDRESULTS}" != "x" ] ; then
    TCL_IFLAGS=-I`ls -1t $FINDRESULTS | grep -v private | head -n 1 | sed s#/tcl.h##`
else
    echo "Could not find tcl.h. You might need to install tcl and/or tcl-dev packages." 1>&2
fi

FINDRESULTS=`find /usr/include -name tclExtend.h 2>/dev/null`
if [ "x${FINDRESULTS}" = "x" ] ; then
  FINDRESULTS=`find /usr/local/include -name tclExtend.h 2>/dev/null`
fi
TCL_EXTEND_IFLAGS= 
if [ "x${FINDRESULTS}" != "x" ] ; then
    TCL_EXTEND_IFLAGS=-I`ls -1t $FINDRESULTS | grep -v private | head -n 1 | sed s#/tclExtend.h##` 
else
    echo "Could not find tclExtend.h. You might need to install tclx and/or tclx-dev packages." 1>&2
fi


FINDRESULTS=`find /usr/include -name tk.h 2>/dev/null`
if [ "x${FINDRESULTS}" = "x" ] ; then
  FINDRESULTS=`find /usr/local/include -name tk.h 2>/dev/null`
fi
TK_IFLAGS= 
if [ "x${FINDRESULTS}" != "x" ] ; then
    TK_IFLAGS=-I`ls -1t $FINDRESULTS | grep -v private | head -n 1 | sed s#/tk.h##`
else
    echo "Could not find tk.h. You might need to install tk and/or tk-dev packages." 1>&2
fi

if [ 0$TCL_IFLAGS = 0-I/usr/include ] ; then
    TCL_IFLAGS=
fi

if [ 0$TCL_EXTEND_IFLAGS = 0$TCL_IFLAGS ] ; then
    TCL_EXTEND_IFLAGS=
fi

if [ 0$TCL_EXTEND_IFLAGS = 0-I/usr/include ] ; then
    TCL_EXTEND_IFLAGS=
fi

if [ 0$TK_IFLAGS = 0$TCL_IFLAGS ] ; then
    TK_IFLAGS=
fi

if [ 0$TK_IFLAGS = 0-I/usr/include ] ; then
    TK_IFLAGS=
fi

TCLTKINCFLAGS="$TCL_IFLAGS $TCL_EXTEND_IFLAGS $TK_IFLAGS"
export TCLTKINCFLAGS

echo $TCL_IFLAGS $TCL_EXTEND_IFLAGS $TK_IFLAGS 


