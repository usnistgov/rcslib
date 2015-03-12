AC_DEFUN([AC_HAVE_RCSLIB],
	[AC_MSG_CHECKING([for RCS library])]
	RCSLIB_DIR=`echo $RCSLIB_DIR`
	if test x$RCSLIB_DIR = x ; then
	for dir in /usr/local/rcslib /usr/rcslib /local/rcslib ; do
	if test -d $dir -o -L $dir ; then RCSLIB_DIR=$dir ; break ; fi
	done
	fi
	if test x$RCSLIB_DIR = x ; then
	[AC_MSG_ERROR([not found, set RCSLIB_DIR environment variable and try again])]
	else
# put HAVE_RCSLIB in config.h
	[AC_DEFINE(HAVE_RCSLIB,
		1, [Define non-zero if you have the RCS Library.])]
# put RCSLIB_DIR in Makefile
	[AC_SUBST(RCSLIB_DIR)]
	[AC_MSG_RESULT([$RCSLIB_DIR])]
	fi
	)
