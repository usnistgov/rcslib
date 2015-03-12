#!/bin/sh


if test "x${TCLTKLIBFLAGS}" != "x" ; then
    echo ${TCLTKLIBFLAGS}
    exit 0
fi

if test ! -f .tcltklibflags_guess_warn -a ! ~/.tcltklibflags_guess_warn ; then 
    echo "Trying to guess what flags need to be used to link C programs that interact with tcl/tk." 1>&2
    echo "Set the environment variable TCLTKLIBFLAGS to override this." 1>&2
    touch  .tcltklibflags_guess_warn >/dev/null 2>/dev/null
    touch  ~/.tcltklibflags_guess_warn >/dev/null 2>/dev/null
fi

# set -x

TCL_LFLAG=-l`ls -1t /lib/libtcl*.{so,a}* /usr/lib/libtcl*.{so,a}* /usr/X11R6/lib/libtcl*.{so,a}*  2> /dev/null | grep -v tclx |  head -n 1 | sed s#^.\*tcl#tcl# | sed s/.so.\*\$// | sed s/.a.\*\$//`

if [ "x${TCL_LFLAG}" = "x-l" ] ; then
    TCL_LFLAG=""
fi

TCLX_LFLAG=-l`ls -1t /lib/libtclx*.{so,a}* /usr/lib/libtclx*.{so,a}* /usr/X11R6/lib/libtclx*.{so,a}*  2> /dev/null | head -n 1 | sed s#^.\*tclx#tclx# | sed s/.so.\*\$// | sed s/.a.\*\$//`

if [ "x${TCLX_LFLAG}" = "x-l" ] ; then
    TCLX_LFLAG=""
fi

TK_LFLAG=-l`ls -1t /lib/libtk*.{so,a}* /usr/lib/libtk*.{so,a}* /usr/X11R6/lib/libtk*.{so,a}*  2> /dev/null | grep -v tkx | head -n 1 | sed s#^.\*tk#tk# | sed s/.so.\*\$// | sed s/.a.\*\$//`

if [ "x${TK_LFLAG}" = "x-l" ] ; then
    TK_LFLAG=""
fi

TKX_LFLAG=-l`ls -1t /lib/libtkx*.{so,a}* /usr/lib/libtkx*.{so,a}* /usr/X11R6/lib/libtkx*.{so,a}*  2> /dev/null | head -n 1 | sed s#^.\*tkx#tkx# | sed s/.so.\*\$// | sed s/.a.\*\$//`

if [ "x${TKX_LFLAG}" = "x-l" ] ; then
    TKX_LFLAG=""
fi

TCLTKLIBFLAGS="$TCL_LFLAG $TCLX_LFLAG  $TK_LFLAG  $TKX_LFLAG"
export TCLTKLIBFLAGS

echo $TCL_LFLAG $TCLX_LFLAG  $TK_LFLAG  $TKX_LFLAG


