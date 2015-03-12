AC_INIT(skel, 1.0)
AC_CONFIG_AUX_DIR([config])
AM_INIT_AUTOMAKE(skel, 1.0)
AC_CONFIG_SRCDIR([src/supShell.cc])
AC_CONFIG_HEADERS([config.h])

# Checks for programs.
AC_PROG_CC
AC_PROG_CXX
AC_PROG_RANLIB
AC_CHECK_PROGS(JAVA, java)
AM_CONDITIONAL(HAVE_JAVA, test x$JAVA != x)

# Compute the canonical host-system type variable, 'host', and its
# three individual parts 'host_cpu', 'host_vendor', and 'host_os'.
AC_CANONICAL_HOST
if test x$host_os = xcygwin ; then
   AC_DEFINE(HAVE_CYGWIN, 1, [Define non-zero if you have Cygwin.])
fi

# Checks for RCS library.
AC_HAVE_RCSLIB

AC_CONFIG_FILES([Makefile bin/Makefile lib/Makefile etc/Makefile src/Makefile])

AC_OUTPUT
