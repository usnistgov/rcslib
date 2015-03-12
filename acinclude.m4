

AC_DEFUN([AC_CXX_HAVE_STD],AC_DEFUN([AC_CXX_NAMESPACES],
[AC_CACHE_CHECK(whether the compiler implements namespaces,
ac_cv_cxx_namespaces,
[AC_LANG_SAVE
 AC_LANG_CPLUSPLUS
 AC_TRY_COMPILE([namespace Outer { namespace Inner { int i = 0; }}],
                [using namespace Outer::Inner; return i;],
 ac_cv_cxx_namespaces=yes, ac_cv_cxx_namespaces=no)
 AC_LANG_RESTORE
])
if test "$ac_cv_cxx_namespaces" = yes; then
  AC_DEFINE(HAVE_NAMESPACES,,[define if the compiler implements namespaces])
fi
])


[AC_CACHE_CHECK(whether the compiler supports ISO C++ standard library,
ac_cv_cxx_have_std,
[AC_REQUIRE([AC_CXX_NAMESPACES])
 AC_LANG_SAVE
 AC_LANG_CPLUSPLUS
 AC_TRY_COMPILE([#include <iostream>
#include <map>
#include <iomanip>
#include <cmath>
#ifdef HAVE_NAMESPACES
using namespace std;
#endif],[return 0;],
 ac_cv_cxx_have_std=yes, ac_cv_cxx_have_std=no)
 AC_LANG_RESTORE
])
if test "$ac_cv_cxx_have_std" = yes; then
  AC_DEFINE(HAVE_STD,,[define if the compiler supports ISO C++ standard library])
fi
])


AC_DEFUN([AC_TRY_LINUX_KERNEL_VERSION_HEADER],
[
#AC_MSG_NOTICE([$1])
#AC_MSG_NOTICE([KSD = ${KERNEL_SOURCE_DIR}])
if test x${KERNEL_SOURCE_DIR} = x ; then 
rev=`uname -r`	
#AC_MSG_NOTICE([rev = ${rev}])
if test -f $1 ; then	
	if grep UTS_RELEASE $1 >/dev/null 2>/dev/null ; then 
		#AC_MSG_NOTICE([UTS_RELEASE found])
	        irev=`grep UTS_RELEASE $1 2>/dev/null | head -n 1 | gawk '{print @S|@3}' | sed 's.@<:@" @:>@..g'`
		#AC_MSG_NOTICE([irev = $irev])
		if test  ${#irev} -gt 0  ; then
		    if test "$irev" = "$rev" ; then
			   KERNEL_SOURCE_DIR=`echo $1 | sed s%/include/linux/version.h%%`
				#AC_MSG_NOTICE([KSD = ${KERNEL_SOURCE_DIR}])
		    fi
	         fi
	    fi
fi
fi
])

AC_DEFUN([AC_CV_LINUX_KERNEL_DIR],
[AC_CACHE_CHECK(for the linux kernel source directory,
ac_cv_linux_kernel_dir,
[
if test -f /usr/src/linux/include/linux/version.h ; then
	if test "x${KERNEL_SOURCE_DIR}" = x ; then 
		AC_TRY_LINUX_KERNEL_VERSION_HEADER(/usr/src/linux/include/linux/version.h)
	fi
fi

# try locate first it is ussually faster
if test -x `which locate 2>/dev/null` ; then
    verfiles=`locate '/usr/src/*/include/linux/version.h'`

    rev=`uname -r`
    for j in $verfiles ; do
	if test x${KERNEL_SOURCE_DIR} = x ; then 
		AC_TRY_LINUX_KERNEL_VERSION_HEADER($j)
	fi
    done
fi


# Locate doesn't exist or failed.
if test x${KERNEL_SOURCE_DIR} = x ; then 
verfiles=`find /usr/src/ -name version.h 2>/dev/null`
for j in $verfiles ; do
   if test x${KERNEL_SOURCE_DIR} = x ; then 
	AC_TRY_LINUX_KERNEL_VERSION_HEADER($j)
   fi
done
fi

if test x${KERNEL_SOURCE_DIR} = x ; then 
	KERNEL_SOURCE_DIR=/usr/src/linux
fi
ac_cv_linux_kernel_dir=${KERNEL_SOURCE_DIR}
])
])
