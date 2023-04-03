%#
#  $Id: configure.ac 2314 2014-05-14 19:51:25Z shackle $
#

# Process this file with autoconf to produce a configure script.
AC_INIT(Real-Time Control System Library, 2017.07.19, shackle@nist.gov, rcslib)
AM_INIT_AUTOMAKE([subdir-objects])
AC_CONFIG_SRCDIR([src/rcs.hh])
AC_CONFIG_HEADERS([rcs_config.h])
AM_MAINTAINER_MODE
AC_CONFIG_MACRO_DIR([m4])

AC_MSG_NOTICE([PLEASE READ THE INSTALL FILE])
AC_MSG_NOTICE([ CPPFLAGS = ${CPPFLAGS} ])
AC_MSG_NOTICE([ CFLAGS = ${CFLAGS} ])
AC_MSG_NOTICE([ CXXFLAGS = ${CXXFLAGS} ])
AC_MSG_NOTICE([ LDFLAGS = ${LDFLAGS} ])
AC_MSG_NOTICE([ LIBS = ${LIBS} ])
AC_MSG_NOTICE([ CC = ${CC} ])
AC_MSG_NOTICE([ CXX = ${CXX} ])
AC_MSG_NOTICE([ AR = ${AR} ])

if echo "x${CFLAGS}" | grep -- '-Werror' >/dev/null 2>/dev/null ; then
   AC_MSG_NOTICE([CFLAGS contains -Werror : this frequently causes problems with configure])
   AC_MSG_NOTICE([add this option to CFLAGS= on the make command line instead.])   
   CFLAGS=`echo " ${CFLAGS}" | sed s/-Werror//`
fi

if echo "x${CXXFLAGS}" | grep -- '-Werror' >/dev/null 2>/dev/null ; then 
   AC_MSG_NOTICE([CXXFLAGS contains -Werror : this frequently causes problems with configure])
   AC_MSG_NOTICE([add this option to CXXFLAGS= on the make command line instead.])   
   CXXFLAGS=`echo " ${CXXFLAGS}" | sed s/-Werror//`
fi

AC_MSG_NOTICE([ CFLAGS = ${CFLAGS} ])
AC_MSG_NOTICE([ CXXFLAGS = ${CXXFLAGS} ])

if echo "x${LDFLAGS} | grep 'L~' ; then
   AC_MSG_WARN([LDFLAGS=${LDFLAGS} : tilde expansion for this variable is not fully supported. -- it is recommended that you rerun configure with only full absolute paths in LDFLAGS])
fi

if echo "x${CPPFLAGS} | grep 'I~' ; then
   AC_MSG_WARN([CPPFLAGS=${CPPFLAGS} : tilde expansion for this variable is not fully supported. -- it is recommended that you rerun configure with only full absolute paths in CPPFLAGS])
fi
 
CONFIGURED_PROGRAMS='nmltest$(EXEEXT) nmlcfg$(EXEEXT) nmlclean$(EXEEXT) nml_packed_file_to_csv$(EXEEXT) nmlperf$(EXEEXT) perfsvr$(EXEEXT)'
#CONFIGURED_STATIC_LIBRARIES=""
CONFIGURED_SUBDIRS=
CONFIGURED_TESTS=
CONFIGURED_EXTRA_INCLUDES=
RCSLIB_EXTRA_OBJS=
RCSRTAILIB_EXTRA_OBJS=
RCSRTLLIB_EXTRA_OBJS=

# Checks for programs.
AC_PROG_CXX(QCC g++ c++ CC cl cxx KCC cc++ egcs gcc cc)
AC_PROG_AWK
AC_PROG_CC(qcc gcc cc egcs cl)
AC_PROG_CPP
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
AC_PROG_LIBTOOL
AM_PROG_CC_C_O

AC_ARG_WITH(posix_ipc,
	[AC_HELP_STRING([--with-posix-ipc],
	[Use POSIX IPC if both POSIX and System V are available (default=no)])],
	[with_posix_ipc=$withval;
	AC_MSG_NOTICE([with_posix_ipc=${with_posix_ipc}])],
	[with_posix_ipc=no])

AC_ARG_WITH(snprintf,
	[AC_HELP_STRING([--with-snprintf],
	[Use snprintf instead of sprintf. (default=yes)])],
	[with_snprintf=$withval;
	AC_MSG_NOTICE([with_snprintf=${with_snprintf}])],
	[with_snprintf=yes])

AC_ARG_WITH(filio,
	[AC_HELP_STRING([--with-filio],
	[Include filio.h (default=yes)])],
	[with_filio=$withval;
	AC_MSG_NOTICE([with_filio=${with_filio}])],
	[with_filio=yes])

AC_ARG_WITH(winsock,
	[AC_HELP_STRING([--with-winsock],
	[Use winsock if available (default=yes)])],
	[with_winsock=$withval;
	AC_MSG_NOTICE([with_winsock=${with_winsock}])],
	[with_winsock=yes])


AC_ARG_WITH(windows,
	[AC_HELP_STRING([--with-windows],
	[Use MS Windows headers and functions if available (default=yes)])],
	[with_windows=$withval;
	AC_MSG_NOTICE([with_windows=${with_windows}])],
	[with_windows=yes])

AC_ARG_WITH(winsock2,
	[AC_HELP_STRING([--with-winsock2],
	[Use winsock2 if available (default=yes)])],
	[with_winsock2=$withval;
	AC_MSG_NOTICE([with_winsock2=${with_winsock2}])],
	[with_winsock2=yes])

AC_ARG_WITH(iostream,
	[AC_HELP_STRING([--with-iostream],
	[Use iostreams for printing posemath objects.(default=yes)])],
	[with_iostream=$withval;
	AC_MSG_NOTICE([with_iostream=${with_iostream}])],
	[with_iostream=yes])

AC_ARG_WITH(sys_v_ipc,
	[AC_HELP_STRING([--with-sys-v-ipc],
	[Use System V IPC if both POSIX and System V are available (default=yes)])],
	[with_sys_v_ipc=$withval;
	AC_MSG_NOTICE([with_sys_v_ipc=${with_sys_v_ipc}])],
	[with_sys_v_ipc=yes])

#AC_ARG_WITH(rtl_rtai,
#	[AC_HELP_STRING([--with-rtl-rtai],
#	[Check for rtlinux or rtai and provide support if found. (default=no)])],
#	[with_rtl_rtai=$withval;
#	AC_MSG_NOTICE([with_rtl_rtai=${with_rtl_rtai}])],
#	[with_rtl_rtai=default_to_no])

AC_ARG_WITH(librt,
	[AC_HELP_STRING([--with-librt],
	[Check for librt and use if found. On some systems it provides clock_gettime() which may be more accurate than gettimeofday(). (default=yes)])],
	[with_librt=$withval;
	AC_MSG_NOTICE([with_librt=${with_librt}])],
	[with_librt=yes])


AC_ARG_WITH(posix_threads,
	[AC_HELP_STRING([--with-posix_threads],
	[Check for POSIX threads and use if found. (default=yes)])],
	[with_posix_threads=$withval;
	AC_MSG_NOTICE([with_posix_threads=${with_posix_threads}])],
	[with_posix_threads=yes]
	)


AC_ARG_ENABLE(ALL,
	[AC_HELP_STRING([--enable-ALL],
	[Enable/Disable all the other features in one command. (default=no)])],
	[
	enable_all=${enableval};
	AC_MSG_NOTICE([set_--enable-ALL = ${enableval}])
	],
	[enable_all=notset])


AC_ARG_ENABLE(ALLJAVA,
	[AC_HELP_STRING([--enable-ALLJAVA],
	[Enable/Disable all java related features in one command. (default=no)])],
	[
	enable_alljava=${enableval};
	AC_MSG_NOTICE([set_--enable-ALLJAVA = ${enable_alljava}])
	],
	[enable_alljava=notset])


AC_ARG_ENABLE(codegen_jar,
	[AC_HELP_STRING([--enable-codegen-jar],
	[Build the CodeGenCmdLine.jar and  rcs.jar. It requires java SDK.(default=no)])],
	[enable_codegen_jar=$enableval;
	AC_MSG_NOTICE([set_--enable-codegen_jar=$enableval])],
	[enable_codegen_jar=default_to_no])


AC_ARG_ENABLE(rcsdesign_jar,
	[AC_HELP_STRING([--enable-rcsdesign-jar],
	[Build the rcsDesign.jar. It requires java SDK.(default=no)])],
	[enable_rcsdesign_jar=$enableval;
	AC_MSG_NOTICE([set_--enable-rcsdesign_jar=$enableval])],
	[enable_rcsdesign_jar=default_to_no])


AC_ARG_ENABLE(force_ipc_rm,
	[AC_HELP_STRING([--enable-force_ipc_rm],
	[Enable code to force IPC resources to be removed, during cleanup.(default=no)])],
	[enable_force_ipc_rm=$enableval;
	AC_MSG_NOTICE([set_--enable-force_ipc_rm=$enableval])],
	[enable_force_ipc_rm=yes])


if test -f jtest.java ; then
    rm jtest.java
fi

if test -f jtest.class ; then
    rm jtest.class
fi

cat >jtest.java <<EOF
public class jtest
{
    static public void main(String args[[]])
    {
	try
	    {
		java.util.Properties p = System.getProperties();
		if(p == null)
		    {
		    	System.err.println("System.getProperties() returned null");
			System.exit(1);
		    }
		String jv = p.getProperty("java.version");
		if(jv == null)
		    {
			System.err.println("System.getProperties().getProperty(\"java.version\") returned null");
			System.exit(2);
		    }
		System.out.println("java.version="+jv);
		// No longer checking if it is > 1.4 since that is so ancient it is not worth it. Previously mistakenly though 10+ were too old since pattern didn't match.
		System.exit(0);
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
	System.exit(10);
    }
}
EOF

if test "x${ac_cv_exeext}" != "x" ; then
      enable_tty_default=no;
else

enable_tty_default=no;

if ( uname -s | grep -i linux ) >/dev/null 2>/dev/null ; then
   enable_tty_default=yes;
fi

fi
#  test "x${ac_cv_exeext}" != "x" ; then

java_cmd=`which java`
AC_MSG_NOTICE([java_cmd=${java_cmd}])

javac_cmd=`which javac`
AC_MSG_NOTICE([javac_cmd=${javac_cmd}])

if javac jtest.java && java jtest ; then
   enable_java_jars_default=default_to_yes;
else
   pwd
   javac jtest.java >javac_jtest.java.log 2>javac_jtest.java.err
   cat javac_jtest.java.log
   cat javac_jtest.java.err
   java jtest >java_jtest.log 2>java_jtest.err
   cat java_jtest.log
   cat java_jtest.err
   enable_java_jars_default=default_to_no;
fi


if test -f jtest.java ; then
    rm jtest.java
fi

if test -f jtest.class ; then
    rm jtest.class
fi

#echo enable_codegencmdline_jar_default=$enable_codegencmdline_jar_default

AC_MSG_NOTICE([enable_java_jars_default=${enable_java_jars_default}])


AC_ARG_ENABLE(codegencmdline_jar,
	[AC_HELP_STRING([--enable-codegencmdline-jar],
	[Build the CodeGenCmdLine.jar. It requires java SDK.])],
	[enable_codegencmdline_jar=$enableval;
	AC_MSG_NOTICE([set_--enable-codegencmdline_jar=$enableval])],
	[enable_codegencmdline_jar=${enable_java_jars_default}])

#AC_ARG_ENABLE(diagapplet_jar,
#	[AC_HELP_STRING([--enable-diagapplet-jar],
#	[Build the diagapplet.jar, jdiag.jar, CodeGenCmdLine.jar, plotter.jar and  rcs.jar. It requires java SDK.])],
#	[enable_diagapplet_jar=$enableval;
#	AC_MSG_NOTICE([set_--enable-diagapplet_jar=$enableval])],
#	[enable_diagapplet_jar=${enable_java_jars_default}])

AC_ARG_ENABLE(diag_NB_jar,
	[AC_HELP_STRING([--enable-diag_NB-jar],
	[Build the diag_NB.jar, CodeGenCmdLine.jar, plotter_NB.jar and  rcs.jar. It requires java SDK.])],
	[enable_diag_NB_jar=$enableval;
	AC_MSG_NOTICE([set_--enable-diag_NB_jar=$enableval])],
	[enable_diag_NB_jar=${enable_java_jars_default}])

AC_ARG_ENABLE(rcs_jar,
	[AC_HELP_STRING([--enable-rcs-jar],
	[Build the rcs.jar. It requires java SDK.])],
	[enable_rcs_jar=$enableval;
	AC_MSG_NOTICE([set_--enable-rcs_jar=$enableval])],
	[enable_rcs_jar=${enable_java_jars_default}])

AC_ARG_ENABLE(shmem,
	[AC_HELP_STRING([--enable-shmem],
	[Add shared memory support to CMS (default=yes)])],
	[enable_shmem=$enableval;
	AC_MSG_NOTICE([set_--enable-shmem=$enableval])],
	[enable_shmem=default_to_yes])

# GLOBMEM is usually built only for VXWORKS using the old makefiles that
# do not use autoconf/configure, the only reason for the option here is for 
# testing on Linux. In production only SHMEM should likely be used on 
# linux. 

AC_ARG_ENABLE(globmem,
	[AC_HELP_STRING([--enable-globmem],
	[Add shared memory support to CMS (default=no)])],
	[enable_globmem=$enableval;
	AC_MSG_NOTICE([set_--enable-globmem=$enableval])],
	[enable_globmem=default_to_no])

AC_ARG_ENABLE(filemem,
	[AC_HELP_STRING([--enable-filemem],
	[Add filemem support to CMS (default=no)])],
	[enable_filemem=$enableval;
	AC_MSG_NOTICE([set_--enable-filemem=$enableval])],
	[enable_filemem=default_to_no])

AC_ARG_ENABLE(rtlmem,
	[AC_HELP_STRING([--enable-rtlmem],
	[Add rtlmem support to CMS (default=no)])],
	[enable_rtlmem=$enableval;
	AC_MSG_NOTICE([set_--enable-rtlmem=$enableval])],
	[enable_rtlmem=default_to_no])

AC_ARG_ENABLE(locmem,
	[AC_HELP_STRING([--enable-locmem],
	[Add locmem support to CMS (default=no)])],
	[enable_locmem=$enableval;
	AC_MSG_NOTICE([set_--enable-locmem=$enableval])],
	[enable_locmem=default_to_no])

AC_ARG_ENABLE(phantom,
	[AC_HELP_STRING([--enable-phantom],
	[Add phantom support to CMS (default=no)])],
	[enable_phantom=$enableval;
	AC_MSG_NOTICE([set_--enable-phantom=$enableval])],
	[enable_phantom=default_to_no])

AC_ARG_ENABLE(stcp,
	[AC_HELP_STRING([--enable-stcp],
	[Add stcp support to CMS (default=yes)])],
	[enable_stcp=$enableval;
	AC_MSG_NOTICE([set_--enable-stcp=$enableval])],
	[enable_stcp=default_to_yes])

AC_ARG_ENABLE(tcp,
	[AC_HELP_STRING([--enable-tcp],
	[Add tcp support to CMS (default=yes)])],
	[enable_tcp=$enableval;
	AC_MSG_NOTICE([set_--enable-tcp=$enableval])],
	[enable_tcp=default_to_yes])

AC_ARG_ENABLE(udp,
	[AC_HELP_STRING([--enable-udp],
	[Add udp support to CMS (default=yes)])],
	[enable_udp=$enableval;
	AC_MSG_NOTICE([set_--enable-udp=$enableval])],
	[enable_udp=default_to_yes])

AC_ARG_ENABLE(gdrs_im,
	[AC_HELP_STRING([--enable-gdrs_im],
	[Add gdrs_im support to CMS (default=yes)])],
	[enable_gdrs_im=$enableval;
	AC_MSG_NOTICE([set_--enable-gdrs_im=$enableval])],
	[enable_gdrs_im=default_to_no])

AC_ARG_ENABLE(tty,
	[AC_HELP_STRING([--enable-tty],
	[Add tty (serial port) support to CMS])],
	[enable_tty=$enableval;
	AC_MSG_NOTICE([set_--enable-tty=$enableval])],
	[enable_tty=${enable_tty_default}])

AC_ARG_ENABLE(http,
	[AC_HELP_STRING([--enable-http],
	[Add http support to CMS, also enables XML (default=yes)])],
	[enable_http=$enableval;
	AC_MSG_NOTICE([set_--enable-http=$enableval])],
	[enable_http=default_to_no])

AC_ARG_ENABLE(xml,
	[AC_HELP_STRING([--enable-xml],
	[Add XML support to CMS (default=yes)])],
	[enable_xml=$enableval;
	AC_MSG_NOTICE([set_--enable-xml=$enableval])],
	[enable_xml=default_to_yes])

AC_ARG_ENABLE(xdr,
	[AC_HELP_STRING([--enable-xdr],
	[Add XDR (eXternal Data Representation) support to CMS (default=no)])],
	[enable_xdr=$enableval;
	AC_MSG_NOTICE([set_--enable-xdr=$enableval])],
	[enable_xdr=default_to_no])

AC_ARG_ENABLE(packed,
	[AC_HELP_STRING([--enable-packed],
	[Add PACKED (a custom neutral data representation) support to CMS (default=yes)])],
	[enable_packed=$enableval;
	AC_MSG_NOTICE([set_--enable-packed=$enableval])],
	[enable_packed=default_to_yes])

AC_ARG_ENABLE(packedl64,
	[AC_HELP_STRING([--enable-packedl64],
	[Add PACKEDL64 (a custom neutral data representation using 64bits for longs/unsigned longs) support to CMS (default=yes)])],
	[enable_packedl64=$enableval;
	AC_MSG_NOTICE([set_--enable-packedl64=$enableval])],
	[enable_packedl64=default_to_yes])

AC_ARG_ENABLE(long_long,
	[AC_HELP_STRING([--enable-long_long],
	[Add LONG_LONG (a custom neutral data representation) support to CMS (default=yes)])],
	[enable_long_long=$enableval;
	AC_MSG_NOTICE([set_--enable-long_long=$enableval])],
	[enable_long_long=default_to_yes])

AC_ARG_ENABLE(disp,
	[AC_HELP_STRING([--enable-disp],
	[Add disp support to CMS, allows the use of the comma seperated ascii format and the msg2str and str2msg functions. (default=yes)])],
	[enable_disp=$enableval;
	AC_MSG_NOTICE([set_--enable-disp=$enableval])],
	[enable_disp=default_to_yes])

AC_ARG_ENABLE(diag,
	[AC_HELP_STRING([--enable-diag],
	[Add some low level diagnostics support to CMS (default=no)])],
	[enable_diag=$enableval;
	AC_MSG_NOTICE([set_--enable-diag=$enableval])],
	[enable_diag=default_to_no])

AC_ARG_ENABLE(print,
	[AC_HELP_STRING([--enable-print],
	[Include the RCS print support, without it no debug or error messages will be printed or logged. (default=yes)])],
	[enable_print=$enableval;
	AC_MSG_NOTICE([set_--enable-print=$enableval])],
	[enable_print=default_to_yes])


AC_ARG_ENABLE(debug_print,
	[AC_HELP_STRING([--enable-debug_print],
	[Include the RCS debug print support, without it no debug  messages will be printed or logged errors are still printed. (default=no)])],
	[enable_debug_print=$enableval;
	AC_MSG_NOTICE([set_--enable-debug_print=$enableval])],
	[enable_debug_print=default_to_no])

AC_ARG_ENABLE(nmlqr,
	[AC_HELP_STRING([--enable-nmlqr],
	[Add some NML Query/Reply features.(default=no)])],
	[enable_nmlqr=$enableval;
	AC_MSG_NOTICE([set_--enable-nmlqr=$enableval])],
	[enable_nmlqr=default_to_no])

AC_ARG_ENABLE(cms_mrpq,
	[AC_HELP_STRING([--enable-cms_mrpq],
	[Add support for multireader and priority queues.(default=no)])],
	[enable_cms_mrpq=$enableval;
	AC_MSG_NOTICE([set_--enable-cms_mrpq=$enableval])],
	[enable_cms_mrpq=default_to_no])

AC_ARG_ENABLE(crypt,
	[AC_HELP_STRING([--enable-crypt],
	[Add some simple cryptography support, needed for any NML server using passwords. (default=no)])],
	[enable_crypt=$enableval;
	AC_MSG_NOTICE([set_--enable-crypt=$enableval])],
	[enable_crypt=default_to_no])

AC_ARG_ENABLE(server,
	[AC_HELP_STRING([--enable-server],
	[ADD support for building/spawning NML servers. (default=yes)])],
	[enable_server=$enableval;
	AC_MSG_NOTICE([set_--enable-server=$enableval])],
	[enable_server=default_to_yes])

AC_ARG_ENABLE(sokintrf,
	[AC_HELP_STRING([--enable-sokintrf],
	[Add some simple socket support support, needed for TCP,UDP,HTTP etc. (default=yes)])],
	[enable_sokintrf=$enableval;
	AC_MSG_NOTICE([set_--enable-sokintrf=${enableval}])],
	[enable_sokintrf=default_to_yes])

AC_ARG_ENABLE(oe_intrf,
	[AC_HELP_STRING([--enable-oe_intrf],
	[Add support for using US Army WSTAWG OE interface. (NOTES: This is not enabled by --enable-ALL and must be explicitly enabled. This is experimental. It uses a C++ binding with TARDEC/DCS extensions. It is incompatible with enabling shared library versions of librcs.)(default=no)])],
	[enable_oe_intrf=$enableval;
	AC_MSG_NOTICE([set_--enable-oe_intrf=${enableval}])
	],
	[enable_oe_intrf=default_to_no])

AC_ARG_ENABLE(inifile,
	[AC_HELP_STRING([--enable-inifile],
	[Add some simple .ini file reading support.(default=yes)])],
	[enable_inifile=$enableval;
	AC_MSG_NOTICE([set_--enable-inifile=${enableval}])],
	[enable_inifile=default_to_yes])

AC_ARG_ENABLE(c_binding,
	[AC_HELP_STRING([--enable-c_binding],
	[Add support for pure C binding.(default=yes)])],
	[enable_c_binding=$enableval;
	AC_MSG_NOTICE([set_--enable-c_binding=${enableval}])],
	[enable_c_binding=default_to_yes])

AC_ARG_ENABLE(posemath_in_rcslib,
	[AC_HELP_STRING([--enable-posemath_in_rcslib],
	[Include the objects for posemath in the rcslib for backwards compatibility and simpler linking, even with this disabled they still get built and placed in libposemath. This is not affected by --enable-ALL or --disable-ALL. (default=no)])],
	[enable_posemath_in_rcslib=$enableval;
	AC_MSG_NOTICE([set_--enable-posemath_in_rcslib=$enableval])],
	[enable_posemath_in_rcslib=no])

AC_ARG_ENABLE(nmlmod,
	[AC_HELP_STRING([--enable-nmlmod],
	[Include the objects for NML_MODULE, RCS_CMD_MSG, and RCS_STAT_MSG needed by programs built with the RCS Design tool.(default=yes)])],
	[enable_nmlmod=$enableval;
	AC_MSG_NOTICE([set_--enable-nmlmod=$enableval])],
	[enable_nmlmod=default_to_yes])

AC_ARG_ENABLE(xsd2nmlh,
	[AC_HELP_STRING([--enable-xsd2nmlh],
	[Build the xsd2nmlh program. (default=yes)])],
	[enable_xsd2nmlh=$enableval;
	AC_MSG_NOTICE([set_--enable-xsd2nmlh=$enableval])],
	[enable_xsd2nmlh=default_to_yes])

AC_ARG_ENABLE(nmlcfgsvr,
	[AC_HELP_STRING([--enable-nmlcfgsvr],
	[Build the nmlcfgsvr program. (default=yes)])],
	[enable_nmlcfgsvr=$enableval;
	AC_MSG_NOTICE([set_--enable-nmlcfgsvr=$enableval])],
	[enable_nmlcfgsvr=default_to_yes])

AC_ARG_ENABLE(oedm_xml2nmlh,
	[AC_HELP_STRING([--enable-oedm_xml2nmlh],
	[Build the oedm_xml2nmlh program. (default=no)])],
	[enable_oedm_xml2nmlh=$enableval;
	AC_MSG_NOTICE([set_--enable-oedm_xml2nmlh=$enableval])],
	[enable_oedm_xml2nmlh=default_to_no])

#if test "x${with_rtl_rtai}" = "xdefault_to_yes" ; then
#	if ( uname -s | grep -i linux ) >/dev/null 2>/dev/null ; then
#	   if  ( test "x${host}" = "x" ) || (echo ${host} | grep -i linux ) ; then 
#		with_rtl_rtai=yes;
#	   fi
#	fi
#fi

#if test "x${with_rtl_rtai}" = "xdefault_to_yes" ; then
#	with_rtl_rtai=no;
#fi

#if test "x${with_rtl_rtai}" = "xdefault_to_no" ; then
#	with_rtl_rtai=no;
#fi

if test  '(' "x${enable_rcs_jar}" = "xdefault_to_no" -o \
   "x${enable_rcs_jar}" = "xdefault_to_yes"  ')' -a \
    '(' "x${enable_alljava}" = "xyes"  -o "x${enable_alljava}" = "xno" ')' ; then
   enable_rcs_jar=${enable_alljava};
fi

if test  '(' "x${enable_codegen_jar}" = "xdefault_to_no" -o \
   "x${enable_codegen_jar}" = "xdefault_to_yes"  ')' -a \
     '(' "x${enable_alljava}" = "xyes"  -o "x${enable_alljava}" = "xno" ')' ; then
   enable_codegen_jar=${enable_alljava};
fi

if test  '(' "x${enable_rcsdesign_jar}" = "xdefault_to_no" -o \
   "x${enable_rcsdesign_jar}" = "xdefault_to_yes"  ')' -a \
     '(' "x${enable_alljava}" = "xyes"  -o "x${enable_alljava}" = "xno" ')' ; then
   enable_rcsdesign_jar=${enable_alljava};
fi

if test  '(' "x${enable_codegencmdline_jar}" = "xdefault_to_no" -o \
   "x${enable_codegencmdline_jar}" = "xdefault_to_yes"  ')' -a \
     '(' "x${enable_alljava}" = "xyes"  -o "x${enable_alljava}" = "xno" ')' ; then
   enable_codegencmdline_jar=${enable_alljava};
fi

#if test  '(' "x${enable_diagapplet_jar}" = "xdefault_to_no" -o \
#   "x${enable_diagapplet_jar}" = "xdefault_to_yes"  ')' -a \
#    '(' "x${enable_alljava}" = "xyes"  -o "x${enable_alljava}" = "xno" ')' ; then
#   enable_diagapplet_jar=${enable_alljava};
#fi

if test  '(' "x${enable_diag_NB_jar}" = "xdefault_to_no" -o \
   "x${enable_diag_NB_jar}" = "xdefault_to_yes"  ')' -a \
    '(' "x${enable_alljava}" = "xyes"  -o "x${enable_alljava}" = "xno" ')' ; then
   enable_diag_NB_jar=${enable_alljava};
fi

if test "x${enable_rcs_jar}" = "xdefault_to_no" -a "x${enable_all}" = "xyes" ; then
   enable_rcs_jar=yes;
fi

if test "x${enable_codegen_jar}" = "xdefault_to_no" -a "x${enable_all}" = "xyes" ; then
   enable_codegen_jar=yes;
fi

if test "x${enable_rcsdesign_jar}" = "xdefault_to_no" -a "x${enable_all}" = "xyes" ; then
   enable_rcsdesign_jar=yes;
fi

if test "x${enable_codegencmdline_jar}" = "xdefault_to_no" -a "x${enable_all}" = "xyes" ; then
   enable_codegencmdline_jar=yes;
fi

#if test "x${enable_diagapplet_jar}" = "xdefault_to_no" -a "x${enable_all}" = "xyes" ; then
#   enable_diagapplet_jar=yes;
#fi

if test "x${enable_diag_NB_jar}" = "xdefault_to_no" -a "x${enable_all}" = "xyes" ; then
   enable_diag_NB_jar=yes;
fi

if test "x${enable_filemem}" = "xdefault_to_no" -a "x${enable_all}" = "xyes" ; then
   enable_filemem=yes;
fi

if test "x${enable_xml}" = "xdefault_to_no" -a "x${enable_all}" = "xyes" ; then
   enable_xml=yes;
fi

if test "x${enable_xdr}" = "xdefault_to_no" -a "x${enable_all}" = "xyes" ; then
   enable_xdr=yes;
fi

if test "x${enable_packed}" = "xdefault_to_no" -a "x${enable_all}" = "xyes" ; then
   enable_packed=yes;
fi

if test "x${enable_long_long}" = "xdefault_to_no" -a "x${enable_all}" = "xyes" ; then
   enable_long_long=yes;
fi

if test "x${enable_packedl64}" = "xdefault_to_no" -a "x${enable_all}" = "xyes" ; then
   enable_packedl64=yes;
fi

if test "x${enable_disp}" = "xdefault_to_no" -a "x${enable_all}" = "xyes" ; then
   enable_disp=yes;
fi

if test "x${enable_diag}" = "xdefault_to_no" -a "x${enable_all}" = "xyes" ; then
   enable_diag=yes;
fi

if test "x${enable_print}" = "xdefault_to_no" -a "x${enable_all}" = "xyes" ; then
   enable_print=yes;
fi

if test "x${enable_debug_print}" = "xdefault_to_no" -a "x${enable_all}" = "xyes" ; then
   enable_debug_print=yes;
fi

if test "x${enable_nmlqr}" = "xdefault_to_no" -a "x${enable_all}" = "xyes" ; then
   enable_nmlqr=yes;
fi

if test "x${enable_cms_mrpq}" = "xdefault_to_no" -a "x${enable_all}" = "xyes" ; then
   enable_cms_mrpq=yes;
fi

if test "x${enable_xsd2nmlh}" = "xdefault_to_no" -a "x${enable_all}" = "xyes" ; then
   enable_xsd2nmlh=yes;
fi

if test "x${enable_nmlcfgsvr}" = "xdefault_to_no" -a "x${enable_all}" = "xyes" ; then
   enable_nmlcfgsvr=yes;
fi

if test "x${enable_oedm_xml2nmlh}" = "xdefault_to_no" -a "x${enable_all}" = "xyes" ; then
   enable_oedm_xml2nmlh=yes;
fi

if test "x${enable_locmem}" = "xdefault_to_no" -a "x${enable_all}" = "xyes" ; then
   enable_locmem=yes;
fi

if test "x${enable_phantom}" = "xdefault_to_no" -a "x${enable_all}" = "xyes" ; then
   enable_phantom=yes;
fi

if test "x${enable_shmem}" = "xdefault_to_no" -a "x${enable_all}" = "xyes" ; then
   enable_shmem=yes;
fi

if test "x${enable_globmem}" = "xdefault_to_no" -a "x${enable_all}" = "xyes" ; then
   enable_globmem=yes;
fi

if test "x${enable_stcp}" = "xdefault_to_no" -a "x${enable_all}" = "xyes" ; then
   enable_stcp=yes;
fi

if test "x${enable_tcp}" = "xdefault_to_no" -a "x${enable_all}" = "xyes" ; then
   enable_tcp=yes;
fi

if test "x${enable_udp}" = "xdefault_to_no" -a "x${enable_all}" = "xyes" ; then
   enable_udp=yes;
fi

if test "x${enable_tty}" = "xdefault_to_no" -a "x${enable_all}" = "xyes" ; then
   enable_tty=yes;
fi

if test "x${enable_crypt}" = "xdefault_to_no" -a "x${enable_all}" = "xyes" ; then
   enable_crypt=yes;
fi

if test "x${enable_http}" = "xdefault_to_no" -a "x${enable_all}" = "xyes" ; then
   enable_http=yes;
fi


if test "x${enable_nmlmod}" = "xdefault_to_no" -a "x${enable_all}" = "xyes" ; then
   enable_nmlmod=yes;
fi

if test "x${enable_server}" = "xdefault_to_no" -a "x${enable_all}" = "xyes" ; then
   enable_server=yes;
fi

if test "x${enable_sokintrf}" = "xdefault_to_no" -a "x${enable_all}" = "xyes" ; then
   enable_sokintrf=yes;
fi

if test "x${enable_oe_intrf}" = "xdefault_to_no" -a "x${enable_all}" = "xyes" ; then
   enable_oe_intrf=no;
fi

if test "x${enable_rtlmem}" = "xdefault_to_no" -a "x${enable_all}" = "xyes" ; then
   enable_rtlmem=yes;
fi

if test "x${enable_inifile}" = "xdefault_to_no" -a "x${enable_all}" = "xyes" ; then
   enable_inifile=yes;
fi

if test "x${enable_c_binding}" = "xdefault_to_no" -a "x${enable_all}" = "xyes" ; then
   enable_c_binding=yes;
fi

if test "x${enable_rcs_jar}" = "xdefault_to_yes" -a "x${enable_all}" = "xno" ; then
   enable_rcs_jar=no;
fi

if test "x${enable_codegen_jar}" = "xdefault_to_yes" -a "x${enable_all}" = "xno" ; then
   enable_codegen_jar=no;
fi

if test "x${enable_rcsdesign_jar}" = "xdefault_to_yes" -a "x${enable_all}" = "xno" ; then
   enable_rcsdesign_jar=no;
fi

if test "x${enable_codegencmdline_jar}" = "xdefault_to_yes" -a "x${enable_all}" = "xno" ; then
   enable_codegencmdline_jar=no;
fi

#if test "x${enable_diagapplet_jar}" = "xdefault_to_yes" -a "x${enable_all}" = "xno" ; then
#   enable_diagapplet_jar=no;
#fi

if test "x${enable_diag_NB_jar}" = "xdefault_to_yes" -a "x${enable_all}" = "xno" ; then
   enable_diag_NB_jar=no;
fi

if test "x${enable_filemem}" = "xdefault_to_yes" -a "x${enable_all}" = "xno" ; then
   enable_filemem=no;
fi

if test "x${enable_xml}" = "xdefault_to_yes" -a "x${enable_all}" = "xno" ; then
   enable_xml=no;
fi

if test "x${enable_xdr}" = "xdefault_to_yes" -a "x${enable_all}" = "xno" ; then
   enable_xdr=no;
fi

if test "x${enable_packed}" = "xdefault_to_yes" -a "x${enable_all}" = "xno" ; then
   enable_packed=no;
fi

if test "x${enable_long_long}" = "xdefault_to_yes" -a "x${enable_all}" = "xno" ; then
   enable_long_long=no;
fi

if test "x${enable_packedl64}" = "xdefault_to_yes" -a "x${enable_all}" = "xno" ; then
   enable_packedl64=no;
fi

if test "x${enable_disp}" = "xdefault_to_yes" -a "x${enable_all}" = "xno" ; then
   enable_disp=no;
fi

if test "x${enable_diag}" = "xdefault_to_yes" -a "x${enable_all}" = "xno" ; then
   enable_diag=no;
fi

if test "x${enable_print}" = "xdefault_to_yes" -a "x${enable_all}" = "xno" ; then
   enable_print=no;
fi

if test "x${enable_debug_print}" = "xdefault_to_yes" -a "x${enable_all}" = "xno" ; then
   enable_debug_print=no;
fi

if test "x${enable_nmlqr}" = "xdefault_to_yes" -a "x${enable_all}" = "xno" ; then
   enable_nmlqr=no;
fi

if test "x${enable_cms_mrpq}" = "xdefault_to_yes" -a "x${enable_all}" = "xno" ; then
   enable_cms_mrpq=no;
fi

if test "x${enable_xsd2nmlh}" = "xdefault_to_yes" -a "x${enable_all}" = "xno" ; then
   enable_xsd2nmlh=no;
fi

if test "x${enable_nmlcfgsvr}" = "xdefault_to_yes" -a "x${enable_all}" = "xno" ; then
   enable_nmlcfgsvr=no;
fi

if test "x${enable_oedm_xml2nmlh}" = "xdefault_to_yes" -a "x${enable_all}" = "xno" ; then
   enable_oedm_xml2nmlh=no;
fi

if test "x${enable_locmem}" = "xdefault_to_yes" -a "x${enable_all}" = "xno" ; then
   enable_locmem=no;
fi

if test "x${enable_phantom}" = "xdefault_to_yes" -a "x${enable_all}" = "xno" ; then
   enable_phantom=no;
fi

if test "x${enable_shmem}" = "xdefault_to_yes" -a "x${enable_all}" = "xno" ; then
   enable_shmem=no;
fi

if test "x${enable_globmem}" = "xdefault_to_yes" -a "x${enable_all}" = "xno" ; then
   enable_globmem=no;
fi

if test "x${enable_stcp}" = "xdefault_to_yes" -a "x${enable_all}" = "xno" ; then
   enable_stcp=no;
fi

if test "x${enable_tcp}" = "xdefault_to_yes" -a "x${enable_all}" = "xno" ; then
   enable_tcp=no;
fi

if test "x${enable_udp}" = "xdefault_to_yes" -a "x${enable_all}" = "xno" ; then
   enable_udp=no;
fi

if test "x${enable_tty}" = "xdefault_to_yes" -a "x${enable_all}" = "xno" ; then
   enable_tty=no;
fi

if test "x${enable_crypt}" = "xdefault_to_yes" -a "x${enable_all}" = "xno" ; then
   enable_crypt=no;
fi

if test "x${enable_http}" = "xdefault_to_yes" -a "x${enable_all}" = "xno" ; then
   enable_http=no;
fi


if test "x${enable_nmlmod}" = "xdefault_to_yes" -a "x${enable_all}" = "xno" ; then
   enable_nmlmod=no;
fi

if test "x${enable_server}" = "xdefault_to_yes" -a "x${enable_all}" = "xno" ; then
   enable_server=no;
fi

if test "x${enable_sokintrf}" = "xdefault_to_yes" -a "x${enable_all}" = "xno" ; then
   enable_sokintrf=no;
fi

if test "x${enable_oe_intrf}" = "xdefault_to_yes" -a "x${enable_all}" = "xno" ; then
   enable_oe_intrf=no;
fi

if test "x${enable_rtlmem}" = "xdefault_to_yes" -a "x${enable_all}" = "xno" ; then
   enable_rtlmem=no;
fi

if test "x${enable_inifile}" = "xdefault_to_yes" -a "x${enable_all}" = "xno" ; then
   enable_inifile=yes;
fi

if test "x${enable_c_binding}" = "xdefault_to_yes" -a "x${enable_all}" = "xno" ; then
   enable_c_binding=yes;
fi

if test "x${enable_rcs_jar}" = "xdefault_to_yes" ; then
   enable_rcs_jar=yes;
fi

if test "x${enable_codegen_jar}" = "xdefault_to_yes" ; then
   enable_codegen_jar=yes;
fi

if test "x${enable_rcsdesign_jar}" = "xdefault_to_yes" ; then
   enable_rcsdesign_jar=yes;
fi

if test "x${enable_codegencmdline_jar}" = "xdefault_to_yes" ; then
   enable_codegencmdline_jar=yes;
fi

#if test "x${enable_diagapplet_jar}" = "xdefault_to_yes" ; then
#   enable_diagapplet_jar=yes;
#fi

if test "x${enable_diag_NB_jar}" = "xdefault_to_yes" ; then
   enable_diag_NB_jar=yes;
fi

if test "x${enable_filemem}" = "xdefault_to_yes" ; then
   enable_filemem=yes;
fi

if test "x${enable_xml}" = "xdefault_to_yes" ; then
   enable_xml=yes;
fi

if test "x${enable_xdr}" = "xdefault_to_yes" ; then
   enable_xdr=yes;
fi

if test "x${enable_packed}" = "xdefault_to_yes" ; then
   enable_packed=yes;
fi

if test "x${enable_packedl64}" = "xdefault_to_yes" ; then
   enable_packedl64=yes;
fi

if test "x${enable_long_long}" = "xdefault_to_yes" ; then
   enable_long_long=yes;
fi

if test "x${enable_disp}" = "xdefault_to_yes" ; then
   enable_disp=yes;
fi

if test "x${enable_diag}" = "xdefault_to_yes" ; then
   enable_diag=yes;
fi

if test "x${enable_print}" = "xdefault_to_yes" ; then
   enable_print=yes;
fi

if test "x${enable_debug_print}" = "xdefault_to_yes" ; then
   enable_debug_print=yes;
fi

if test "x${enable_nmlqr}" = "xdefault_to_yes" ; then
   enable_nmlqr=yes;
fi

if test "x${enable_cms_mrpq}" = "xdefault_to_yes" ; then
   enable_cms_mrpq=yes;
fi

if test "x${enable_xsd2nmlh}" = "xdefault_to_yes" ; then
   enable_xsd2nmlh=yes;
fi

if test "x${enable_nmlcfgsvr}" = "xdefault_to_yes" ; then
   enable_nmlcfgsvr=yes;
fi

if test "x${enable_oedm_xml2nmlh}" = "xdefault_to_yes" ; then
   enable_oedm_xml2nmlh=yes;
fi

if test "x${enable_locmem}" = "xdefault_to_yes" ; then
   enable_locmem=yes;
fi

if test "x${enable_phantom}" = "xdefault_to_yes" ; then
   enable_phantom=yes;
fi

if test "x${enable_shmem}" = "xdefault_to_yes" ; then
   enable_shmem=yes;
fi

if test "x${enable_globmem}" = "xdefault_to_yes" ; then
   enable_globmem=yes;
fi

if test "x${enable_stcp}" = "xdefault_to_yes" ; then
   enable_stcp=yes;
fi

if test "x${enable_tcp}" = "xdefault_to_yes" ; then
   enable_tcp=yes;
fi

if test "x${enable_udp}" = "xdefault_to_yes" ; then
   enable_udp=yes;
fi

if test "x${enable_tty}" = "xdefault_to_yes" ; then
   enable_tty=yes;
fi

if test "x${enable_crypt}" = "xdefault_to_yes" ; then
   enable_crypt=yes;
fi

if test "x${enable_http}" = "xdefault_to_yes" ; then
   enable_http=yes;
fi

if test "x${enable_posemath_in_rcslib}" = "xdefault_to_yes" ; then
   enable_posemath_in_rcslib=yes;
fi

if test "x${enable_nmlmod}" = "xdefault_to_yes" ; then
   enable_nmlmod=yes;
fi

if test "x${enable_server}" = "xdefault_to_yes" ; then
   enable_server=yes;
fi

if test "x${enable_sokintrf}" = "xdefault_to_yes" ; then
   enable_sokintrf=yes;
fi

if test "x${enable_oe_intrf}" = "xdefault_to_yes" ; then
   enable_oe_intrf=yes;
fi

if test "x${enable_rtlmem}" = "xdefault_to_yes" ; then
   enable_rtlmem=yes;
fi

if test "x${enable_inifile}" = "xdefault_to_yes" ; then
   enable_inifile=yes;
fi

if test "x${enable_c_binding}" = "xdefault_to_yes" ; then
   enable_c_binding=yes;
fi

if test "x${enable_rcs_jar}" = "xdefault_to_no" ; then
   enable_rcs_jar=no;
fi

if test "x${enable_codegen_jar}" = "xdefault_to_no" ; then
   enable_codegen_jar=no;
fi

if test "x${enable_rcsdesign_jar}" = "xdefault_to_no" ; then
   enable_rcsdesign_jar=no;
fi

if test "x${enable_codegencmdline_jar}" = "xdefault_to_no" ; then
   enable_codegencmdline_jar=no;
fi

#if test "x${enable_diagapplet_jar}" = "xdefault_to_no" ; then
#   enable_diagapplet_jar=no;
#fi

if test "x${enable_diag_NB_jar}" = "xdefault_to_no" ; then
   enable_diag_NB_jar=no;
fi

if test "x${enable_filemem}" = "xdefault_to_no" ; then
   enable_filemem=no;
fi

if test "x${enable_xml}" = "xdefault_to_no" ; then
   enable_xml=no;
fi

if test "x${enable_xdr}" = "xdefault_to_no" ; then
   enable_xdr=no;
fi

if test "x${enable_packed}" = "xdefault_to_no" ; then
   enable_packed=no;
fi

if test "x${enable_packedl64}" = "xdefault_to_no" ; then
   enable_packedl64=no;
fi

if test "x${enable_long_long}" = "xdefault_to_no" ; then
   enable_long_long=no;
fi

if test "x${enable_disp}" = "xdefault_to_no" ; then
   enable_disp=no;
fi

if test "x${enable_diag}" = "xdefault_to_no" ; then
   enable_diag=no;
fi

if test "x${enable_print}" = "xdefault_to_no" ; then
   enable_print=no;
fi

if test "x${enable_debug_print}" = "xdefault_to_no" ; then
   enable_debug_print=no;
fi

if test "x${enable_nmlqr}" = "xdefault_to_no" ; then
   enable_nmlqr=no;
fi

if test "x${enable_cms_mrpq}" = "xdefault_to_no" ; then
   enable_cms_mrpq=no;
fi

if test "x${enable_xsd2nmlh}" = "xdefault_to_no" ; then
   enable_xsd2nmlh=no;
fi

if test "x${enable_nmlcfgsvr}" = "xdefault_to_no" ; then
   enable_nmlcfgsvr=no;
fi

if test "x${enable_oedm_xml2nmlh}" = "xdefault_to_no" ; then
   enable_oedm_xml2nmlh=no;
fi

if test "x${enable_locmem}" = "xdefault_to_no" ; then
   enable_locmem=no;
fi

if test "x${enable_phantom}" = "xdefault_to_no" ; then
   enable_phantom=no;
fi

if test "x${enable_shmem}" = "xdefault_to_no" ; then
   enable_shmem=no;
fi

if test "x${enable_globmem}" = "xdefault_to_no" ; then
   enable_globmem=no;
fi

if test "x${enable_stcp}" = "xdefault_to_no" ; then
   enable_stcp=no;
fi

if test "x${enable_tcp}" = "xdefault_to_no" ; then
   enable_tcp=no;
fi

if test "x${enable_udp}" = "xdefault_to_no" ; then
   enable_udp=no;
fi

if test "x${enable_tty}" = "xdefault_to_no" ; then
   enable_tty=no;
fi

if test "x${enable_crypt}" = "xdefault_to_no" ; then
   enable_crypt=no;
fi

if test "x${enable_http}" = "xdefault_to_no" ; then
   enable_http=no;
fi

if test "x${enable_posemath_in_rcslib}" = "xdefault_to_no" ; then
   enable_posemath_in_rcslib=no;
fi

if test "x${enable_nmlmod}" = "xdefault_to_no" ; then
   enable_nmlmod=no;
fi

if test "x${enable_server}" = "xdefault_to_no" ; then
   enable_server=no;
fi

if test "x${enable_sokintrf}" = "xdefault_to_no" ; then
   enable_sokintrf=no;
fi

if test "x${enable_oe_intrf}" = "xdefault_to_no" ; then
   enable_oe_intrf=no;
fi

if test "x${enable_rtlmem}" = "xdefault_to_no" ; then
   enable_rtlmem=no;
fi

if test "x${enable_inifile}" = "xdefault_to_no" ; then
   enable_inifile=no;
fi

if test "x${enable_c_binding}" = "xdefault_to_no" ; then
   enable_c_binding=no;
fi

if test "x${enable_print}" != "xyes" ; then
   enable_xsd2nmlh=no;
   AC_MSG_NOTICE([Disabling xs2dnmlh since enable_print not set.])
fi

if test "x${enable_xsd2nmlh}" = "xyes" ; then
   CONFIGURED_PROGRAMS="${CONFIGURED_PROGRAMS} "'xsd2nmlh$(EXEEXT)'
   AC_MSG_NOTICE([CONFIGURED_PROGRAMS=${CONFIGURED_PROGRAMS}])
fi

## Do we have pkg-config?
## If we're cross-compiling, pretend we don't have pkg-config, because it
## will do the wrong thing (i.e., identify packages on the build systems
## instead of the host system).
if test "$host" = "$build"; then
  AC_CHECK_PROG(have_pkg_config,pkg-config,yes,no)
else
  have_pkg_config=no
fi
if test "x$have_pkg_config" != "xno"; then
  ## Also, add /usr/local/lib/pkgconfig, because on
  ## some platforms, pkg-config won't look there by default.
  export PKG_CONFIG_PATH=$PREFIX/lib/pkgconfig:/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH
fi

if test "x${enable_nmlcfgsvr}" = "xyes" ; then
   CONFIGURED_PROGRAMS="${CONFIGURED_PROGRAMS} "'nmlcfgsvr$(EXEEXT)'
   EXTRA_CHECK_PROGRAMS="${EXTRA_CHECK_PROGRAMS} "nml_test_nmlset'$(EXEEXT)'
   ## AC_MSG_NOTICE([CONFIGURED_PROGRAMS=${CONFIGURED_PROGRAMS}])
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/cms/nmlcfgsvr_clntcalls.lo"
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/cms/nmlset.lo"
   ## AC_MSG_NOTICE([RCSLIB_EXTRA_OBJS : ${RCSLIB_EXTRA_OBJS}])
   AC_DEFINE(ENABLE_RCS_NMLCFGSVR,1,[Define to 1 if nmlcfgsvr is enabled.])
   if test "x${enable_sokintrf}" != "xyes" ; then
      enable_sokintrf=yes
      AC_MSG_NOTICE([sokintrf had to be enabled to support nmlcfgsvr])
   fi
   if test "x${enable_stcp}" != "xyes" ; then
      enable_stcp=yes
      AC_MSG_NOTICE([sokintrf had to be enabled to support stcp])
   fi
   CONFIGURED_TESTS="${CONFIGURED_TESTS} \$(srcdir)/etc/check_testnml_nmlcfgsvr.sh"
fi

if test "x${enable_oedm_xml2nmlh}" = "xyes" ; then
   CONFIGURED_PROGRAMS="${CONFIGURED_PROGRAMS} "'oedm_xml2nmlh$(EXEEXT)'
   ## AC_MSG_NOTICE([CONFIGURED_PROGRAMS=${CONFIGURED_PROGRAMS}])
fi

if test "x${enable_filemem}" = "xyes" ; then
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/cms/filemem.lo"
   ## AC_MSG_NOTICE([RCSLIB_EXTRA_OBJS : ${RCSLIB_EXTRA_OBJS}])
   AC_DEFINE(ENABLE_RCS_FILEMEM,1,[Define to 1 if filemem is enabled.])
   CONFIGURED_TESTS="${CONFIGURED_TESTS} \$(srcdir)/etc/check_testnml_filemem.sh"
fi

if test "x${enable_shmem}" = "xyes" ; then
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/cms/shmem.lo"
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/os_intf/_shm.lo"
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/os_intf/shm.lo"
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/os_intf/sem.lo"
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/cms/memsem.lo"
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/cms/autokey.lo"
   ## AC_MSG_NOTICE([RCSLIB_EXTRA_OBJS : ${RCSLIB_EXTRA_OBJS}])
   AC_DEFINE(ENABLE_RCS_SHMEM,1,[Define to 1 if shmem is enabled.])
   CONFIGURED_TESTS="${CONFIGURED_TESTS} \$(srcdir)/etc/check_testnml_shmem.sh"
fi

# GLOBMEM is usually built only for VXWORKS using the old makefiles that
# do not use autoconf/configure, the only reason for the option here is for 
# testing on Linux. In production only SHMEM should likely be used on 
# linux. 
if test "x${enable_globmem}" = "xyes" ; then
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS}  src/cms/globmem.lo"
   ## AC_MSG_NOTICE([RCSLIB_EXTRA_OBJS : ${RCSLIB_EXTRA_OBJS}])
   AC_DEFINE(ENABLE_RCS_GLOBMEM,1,[Define to 1 if globmem is enabled.])
   #   CONFIGURED_TESTS="${CONFIGURED_TESTS} \$(srcdir)/etc/check_testnml_globmem.sh"
fi

CONFIGURED_TESTS="${CONFIGURED_TESTS} \$(srcdir)/etc/check_testnml_extras.sh"

if test "x${enable_posemath_in_rcslib}" = "xyes" ; then
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS}  src/posemath/_posemath.lo"
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS}  src/posemath/mathprnt.lo"
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS}  src/posemath/sincos.lo"
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS}  src/posemath/posemath.lo"
   ## AC_MSG_NOTICE([RCSLIB_EXTRA_OBJS : ${RCSLIB_EXTRA_OBJS}])
fi

if test "x${enable_nmlmod}" = "xyes" ; then
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/node/nml_mod.lo"
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/node/cmd_msg.lo"
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/node/stat_msg.lo"
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/node/nml_oi.lo"
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/node/stat_msg_v2_n.lo"
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/node/timetracker_n.lo"
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/node/timetracker_c_n.lo"
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/node/timetracker.lo"
   ## AC_MSG_NOTICE([RCSLIB_EXTRA_OBJS : ${RCSLIB_EXTRA_OBJS}])
   AC_DEFINE(ENABLE_RCS_NMLMOD,1,[Define to 1 if nmlmod is enabled.])
fi


if test "x${enable_inifile}" = "xyes" ; then
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/os_intf/inifile.lo"
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/os_intf/_inifile.lo"
   ## AC_MSG_NOTICE([RCSLIB_EXTRA_OBJS : ${RCSLIB_EXTRA_OBJS}])
   AC_DEFINE(ENABLE_RCS_INIFILE,1,[Define to 1 if inifile support in rcslib is enabled.])
fi

if test "x${enable_c_binding}" = "xyes" ; then
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/cms/nmlcms_c.lo"
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/cms/posemath_c_n.lo"
   ## AC_MSG_NOTICE([RCSLIB_EXTRA_OBJS : ${RCSLIB_EXTRA_OBJS}])
   AC_DEFINE(ENABLE_RCS_C_BINDING,1,[Define to 1 if pure C binding support in rcslib is enabled.])
fi

if test "x${enable_http}" = "xyes" ; then
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/cms/httpnml.lo"
   if test "x${enable_server}" = "xyes" ; then
      RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/cms/http_srv.lo"
   fi
   if test "x${enable_sokintrf}" != "xyes" ; then
      enable_sokintrf=yes
      AC_MSG_NOTICE([sokintrf had to be enabled to support http])
   fi
   ## AC_MSG_NOTICE([RCSLIB_EXTRA_OBJS : ${RCSLIB_EXTRA_OBJS}])
   AC_DEFINE(ENABLE_RCS_HTTP,1,[Define to 1 if http support in rcslib is enabled.])
   if test "x${enable_xml}" != "xyes" ; then
      enable_xml=yes
      AC_MSG_NOTICE([xml had to be enabled to support http])
   fi
fi

if test "x${enable_tty}" = "xyes" ; then
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/cms/ttymem.lo"
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/cms/tty_srv.lo"
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/cms/ttyintf.lo"
   ## AC_MSG_NOTICE([RCSLIB_EXTRA_OBJS : ${RCSLIB_EXTRA_OBJS}])
   AC_DEFINE(ENABLE_RCS_TTY,1,[Define to 1 if tty support in rcslib is enabled.])
   CONFIGURED_PROGRAMS="${CONFIGURED_PROGRAMS} "'ttytest$(EXEEXT)'
fi

if test "x${enable_locmem}" = "xyes" ; then
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/cms/locmem.lo"
   ## AC_MSG_NOTICE([RCSLIB_EXTRA_OBJS : ${RCSLIB_EXTRA_OBJS}])
   AC_DEFINE(ENABLE_RCS_LOCMEM,1,[Define to 1 if locmem support in rcslib is enabled.])
   if test "x${enable_tcp}" = "xyes" -a "x${enable_packed}" = "xyes" ; then
      CONFIGURED_TESTS="${CONFIGURED_TESTS} \$(srcdir)/etc/check_testnml_locmem_tcp_packed.sh"
   fi
fi


if test "x${enable_phantom}" = "xyes" ; then
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/cms/phantom.lo"
   ## AC_MSG_NOTICE([RCSLIB_EXTRA_OBJS : ${RCSLIB_EXTRA_OBJS}])
   AC_DEFINE(ENABLE_RCS_PHANTOMMEM,1,[Define to 1 if phantommem support in rcslib is enabled.])
fi


if test "x${enable_stcp}" = "xyes" ; then
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/cms/stcpmem.lo"
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/cms/sendline.lo"
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/cms/recvline.lo"
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/cms/stcpopts.lo"
   if test "x${enable_server}" = "xyes" ; then
      RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/cms/stcpsvr.lo"
   fi
   if test "x${enable_sokintrf}" != "xyes" ; then
      enable_sokintrf=yes
      AC_MSG_NOTICE([sokintrf had to be enabled to support stcp])
   fi
   if test "x${enable_disp}" != "xyes" ; then
      enable_disp=yes
      AC_MSG_NOTICE([disp had to be enabled to support stcp])
   fi
   ## AC_MSG_NOTICE([RCSLIB_EXTRA_OBJS : ${RCSLIB_EXTRA_OBJS}])
   AC_DEFINE(ENABLE_RCS_STCP,1,[Define to 1 if stcp support in rcslib is enabled.])
   CONFIGURED_TESTS="${CONFIGURED_TESTS} \$(srcdir)/etc/check_testnml_stcpdisp.sh"
#   if test "x${enable_xml}" = "xyes" ; then
#      CONFIGURED_TESTS="${CONFIGURED_TESTS} \$(srcdir)/etc/check_testnml_stcpxml.sh"
#   fi
fi



if test "x${enable_tcp}" = "xyes" ; then
   CONFIGURED_PROGRAMS="${CONFIGURED_PROGRAMS} "'nmlGetRemoteTcpServerBufferList$(EXEEXT)'
   CONFIGURED_PROGRAMS="${CONFIGURED_PROGRAMS} "'nmlGetRemoteTcpServerBufferInfo$(EXEEXT)'
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/cms/tcpmem.lo"
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/cms/sendn.lo"
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/cms/recvn.lo"
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/cms/tcp_opts.lo"
   if test "x${enable_server}" = "xyes" ; then
      RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/cms/tcp_srv.lo"
   fi
   if test "x${enable_sokintrf}" != "xyes" ; then
      enable_sokintrf=yes
      AC_MSG_NOTICE([sokintrf had to be enabled to support tcp])
   fi
   ## AC_MSG_NOTICE([RCSLIB_EXTRA_OBJS : ${RCSLIB_EXTRA_OBJS}])
   AC_DEFINE(ENABLE_RCS_TCP,1,[Define to 1 if tcp support in rcslib is enabled.])
   CONFIGURED_TESTS="${CONFIGURED_TESTS} \$(srcdir)/etc/check_testnml_tcpraw.sh"
   if test "x${enable_xdr}" = "xyes" ; then
      CONFIGURED_TESTS="${CONFIGURED_TESTS} \$(srcdir)/etc/check_testnml_tcpxdr.sh"
   fi
   if test "x${enable_packed}" = "xyes" ; then
      CONFIGURED_TESTS="${CONFIGURED_TESTS} \$(srcdir)/etc/check_testnml_tcppacked.sh"
   fi
   if test "x${enable_xml}" = "xyes" ; then
      CONFIGURED_TESTS="${CONFIGURED_TESTS} \$(srcdir)/etc/check_testnml_tcpxml.sh"
   fi
fi


if test "x${enable_udp}" = "xyes" ; then
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/cms/udpmem.lo"
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/cms/sendmsgt.lo"
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/cms/recvmsgt.lo"
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/cms/udp_opts.lo"
   if test "x${enable_server}" = "xyes" ; then
      RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/cms/udp_srv.lo"
   fi
   if test "x${enable_sokintrf}" != "xyes" ; then
      enable_sokintrf=yes
      AC_MSG_NOTICE([sokintrf had to be enabled to support udp])
   fi
   AC_DEFINE(ENABLE_RCS_UDP,1,[Define to 1 if udp support in rcslib is enabled.])
   if test "x${enable_xdr}" = "xyes" ; then
      CONFIGURED_TESTS="${CONFIGURED_TESTS} \$(srcdir)/etc/check_testnml_udpxdr.sh"
   fi
   if test "x${enable_packed}" = "xyes" ; then
      CONFIGURED_TESTS="${CONFIGURED_TESTS} \$(srcdir)/etc/check_testnml_udppacked.sh"
   fi
fi


if test "x${enable_gdrs_im}" = "xyes" ; then
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/cms/gdrs_im_clnt.lo"
   if test "x${enable_server}" = "xyes" ; then
      RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/cms/gdrs_im_srv.lo"
   fi
   if test "x${enable_sokintrf}" != "xyes" ; then
      enable_sokintrf=yes
      AC_MSG_NOTICE([sokintrf had to be enabled to support gdrs_im])
   fi
   AC_DEFINE(ENABLE_RCS_GDRS_IM,1,[Define to 1 if gdrs_im support in rcslib is enabled.])
   if test "x${enable_xdr}" = "xyes" ; then
      CONFIGURED_TESTS="${CONFIGURED_TESTS} \$(srcdir)/etc/check_testnml_gdrs_im_xdr.sh"
   fi
   if test "x${enable_packed}" = "xyes" ; then
      CONFIGURED_TESTS="${CONFIGURED_TESTS} \$(srcdir)/etc/check_testnml_gdrs_im_packed.sh"
   fi
fi

if test "x${enable_crypt}" = "xyes" ; then
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/cms/crypt2.lo"
   AC_DEFINE(ENABLE_RCS_CRYPT2,1,[Define to 1 if crypt2 support in rcslib is enabled.])
fi

if test "x${enable_xml}" = "xyes" ; then
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/cms/cms_xml_up.lo"
   ## AC_MSG_NOTICE([RCSLIB_EXTRA_OBJS : ${RCSLIB_EXTRA_OBJS}])
   AC_DEFINE(ENABLE_RCS_XML,1,[Define to 1 if xml support in rcslib is enabled.])
fi

if test "x${enable_xdr}" = "xyes" ; then
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/cms/cms_xup.lo"
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/cms/extxdrintf.lo"
   ## AC_MSG_NOTICE([RCSLIB_EXTRA_OBJS : ${RCSLIB_EXTRA_OBJS}])
   AC_DEFINE(ENABLE_RCS_XDR,1,[Define to 1 if xdr support in rcslib is enabled.])
fi

if test "x${enable_packed}" = "xyes" ; then
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/cms/cms_pup.lo"
   ## AC_MSG_NOTICE([RCSLIB_EXTRA_OBJS : ${RCSLIB_EXTRA_OBJS}])
   AC_DEFINE(ENABLE_PACKED_UP,1,[Define to 1 if packed encoding support in rcslib is enabled.])

   if test "x${enable_packedl64}" = "xyes" ; then
      ## AC_MSG_NOTICE([RCSLIB_EXTRA_OBJS : ${RCSLIB_EXTRA_OBJS}])
      AC_DEFINE(ENABLE_RCS_PACKEDL64,1,[Define to 1 if packedl64 encoding support in rcslib is enabled.])
   fi

fi

if test "x${enable_long_long}" = "xyes" ; then
   ## AC_MSG_NOTICE([RCSLIB_EXTRA_OBJS : ${RCSLIB_EXTRA_OBJS}])
   AC_DEFINE(ENABLE_LONG_LONG_UP,1,[Define to 1 if long long update support in rcslib is enabled.])
fi


if test "x${enable_disp}" = "xyes" ; then
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/cms/cms_dup.lo"
   ## AC_MSG_NOTICE([RCSLIB_EXTRA_OBJS : ${RCSLIB_EXTRA_OBJS}])
   AC_DEFINE(ENABLE_RCS_DISP,1,[Define to 1 if disp support in rcslib is enabled.])
fi

if test "x${enable_diag}" = "xyes" ; then
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/cms/cmsdiag.lo"
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/cms/nmldiag.lo"
   ## AC_MSG_NOTICE([RCSLIB_EXTRA_OBJS : ${RCSLIB_EXTRA_OBJS}])
   AC_DEFINE(ENABLE_RCS_DIAG,1,[Define to 1 if diag support in rcslib is enabled.]) 
fi


if test "x${enable_print}" = "xyes" ; then
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/print/rcs_prnt.lo"
   ## AC_MSG_NOTICE([RCSLIB_EXTRA_OBJS : ${RCSLIB_EXTRA_OBJS}])
   AC_DEFINE(ENABLE_RCS_PRNT,1,[Define to 1 if rcs_prnt support in rcslib is enabled.]) 
else
   CPPFLAGS="${CPPFLAGS} -DDISABLE_RCS_PRINT"
   ## AC_MSG_NOTICE([CPPFLAGS : ${CPPFLAGS}])
fi

AC_MSG_NOTICE([enable_debug_print : ${enable_debug_print}])

if test "x${enable_debug_print}" != "xyes" ; then
   CPPFLAGS="${CPPFLAGS} -DDISABLE_RCS_DEBUG_PRINT"
   ## AC_MSG_NOTICE([CPPFLAGS : ${CPPFLAGS}])
fi

if test "x${enable_nmlqr}" = "xyes" ; then
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/cms/nmlqr.lo"
   ## AC_MSG_NOTICE([RCSLIB_EXTRA_OBJS : ${RCSLIB_EXTRA_OBJS}])
   AC_DEFINE(ENABLE_RCS_NMLQR,1,[Define to 1 if nmlqr support in rcslib is enabled.]) 
fi

if test "x${enable_cms_mrpq}" = "xyes" ; then
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/cms/cms_mrpq.lo"
   ## AC_MSG_NOTICE([RCSLIB_EXTRA_OBJS : ${RCSLIB_EXTRA_OBJS}])
   AC_DEFINE(ENABLE_RCS_CMS_MRPQ,1,[Define to 1 if nmlqr support in rcslib is enabled.]) 
fi

if test "x${enable_server}" = "xyes" ; then
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/cms/cms_srv.lo"
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/cms/nml_srv.lo"
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/cms/cmssvrp.lo"
   ## AC_MSG_NOTICE([RCSLIB_EXTRA_OBJS : ${RCSLIB_EXTRA_OBJS}])
   AC_DEFINE(ENABLE_RCS_SERVER,1,[Define to 1 if server support in rcslib is enabled.]) 
   if test "x${enable_sokintrf}" != "xyes" ; then
      enable_sokintrf=yes
      AC_MSG_NOTICE([sokintrf had to be enabled to support server])
   fi
fi
   
if test "x${enable_sokintrf}" = "xyes" ; then
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/cms/sokintrf.lo"
   ## AC_MSG_NOTICE([RCSLIB_EXTRA_OBJS : ${RCSLIB_EXTRA_OBJS}])
   AC_DEFINE(ENABLE_RCS_SOKINTRF,1,[Define to 1 if sokintrf support in rcslib is enabled.]) 
fi

if test "x${enable_oe_intrf}" = "xyes" ; then
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/cms/oemem.lo"
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/cms/oemque.lo"
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/cms/oedsto.lo"
   ## AC_MSG_NOTICE([RCSLIB_EXTRA_OBJS : ${RCSLIB_EXTRA_OBJS}])
   AC_DEFINE(ENABLE_RCS_OE_INTRF,1,[Define to 1 if sokintrf support in rcslib is enabled.]) 
   CONFIGURED_TESTS="${CONFIGURED_TESTS} \$(srcdir)/etc/check_testnml_oemem.sh"
fi

if test "x${enable_diag_NB_jar}" = "xyes" ; then
   CONFIGURED_PROGRAMS="${CONFIGURED_PROGRAMS} "' diag_NB.jar$(EXEEXT) plotter_NB.jar$(EXEEXT) CodeGenCmdLine.jar$(EXEEXT) rcs.jar$(EXEEXT) pvjscript.jar$(EXEEXT)';
   ## AC_MSG_NOTICE([CONFIGURED_PROGRAMS : ${CONFIGURED_PROGRAMS}])

#elif test "x${enable_diagapplet_jar}" = "xyes" ; then
#   CONFIGURED_PROGRAMS="${CONFIGURED_PROGRAMS} "' diagapplet.jar$(EXEEXT) jdiag.jar$(EXEEXT) plotter.jar$(EXEEXT) CodeGenCmdLine.jar$(EXEEXT) rcs.jar$(EXEEXT)';
#   ## AC_MSG_NOTICE([CONFIGURED_PROGRAMS : ${CONFIGURED_PROGRAMS}])

elif test "x${enable_codegen_jar}" = "xyes" ; then
   CONFIGURED_PROGRAMS="${CONFIGURED_PROGRAMS} "' CodeGenCmdLine.jar$(EXEEXT) rcs.jar$(EXEEXT)';
   ## AC_MSG_NOTICE([CONFIGURED_PROGRAMS : ${CONFIGURED_PROGRAMS}])
elif test "x${enable_codegencmdline_jar}" = "xyes" ; then
   CONFIGURED_PROGRAMS="${CONFIGURED_PROGRAMS} "' CodeGenCmdLine.jar$(EXEEXT) rcs.jar$(EXEEXT)';
   ## AC_MSG_NOTICE([CONFIGURED_PROGRAMS : ${CONFIGURED_PROGRAMS}])

elif test "x${enable_rcs_jar}" = "xyes" ; then
   CONFIGURED_PROGRAMS="${CONFIGURED_PROGRAMS} "' rcs.jar$(EXEEXT)';
   ## AC_MSG_NOTICE([CONFIGURED_PROGRAMS : ${CONFIGURED_PROGRAMS}])

fi

if test "x${enable_rcsdesign_jar}" = "xyes" ; then
   CONFIGURED_PROGRAMS="${CONFIGURED_PROGRAMS} "' rcsDesign.jar$(EXEEXT)';
   ## 
fi

AC_MSG_NOTICE([CONFIGURED_PROGRAMS : ${CONFIGURED_PROGRAMS}])


AC_HEADER_STDC
AC_CHECK_HEADERS([stdio.h stdarg.h stdlib.h stddef.h math.h errno.h time.h sys/errno.h ctype.h limits.h values.h float.h signal.h memory.h string.h strings.h])
AC_HEADER_DIRENT

if test "x${CC}" = "xgcc" ; then 
   if echo "x${CFLAGS}" | grep -v -- '-O' >/dev/null 2>/dev/null ; then
      CFLAGS="${CFLAGS} -O2"
   fi
fi

if test "x${CXX}" = "xg++" ; then 
   if echo "x${CXXFLAGS}" | grep -v -- '-O' >/dev/null 2>/dev/null ; then 
      CXXFLAGS="${CXXFLAGS} -O2"
   fi
fi

if test "x${enable_oe_intrf}" = "xyes" -a "x${enable_shared}" = "xyes" ; then
   AC_MSG_ERROR([Building shared libraries must be disabled with --disable-shared when the OE interface is enabled.])
fi

if test "x${enable_oe_intrf}" = "xyes" -a "x${enable_static}" != "xyes" ; then
   AC_MSG_ERROR([Building static libraries must be enabled with --enable-static when the OE interface is enabled.])
fi

check_for_windows_stuff=no

AC_MSG_NOTICE([with_windows : ${with_windows}])

if test "x${with_windows}" = "xyes" ; then
   AC_CHECK_HEADER([windows.h],[check_for_windows_stuff=yes])   
fi



cat >putenv_test.cc <<EOF

#include <stdlib.h>

int main(int argc,const char **argv)
{
putenv((char *)"ENV_VAR=ENV_VAL");
}
EOF


if g++ -Wall -Werror putenv_test.cc > /dev/null ; then
   AC_DEFINE(PUTENV_OK_IN_CPLUSPLUS,1,[Define to 1 if putenv ok to use in c++ source.]) 
   \rm -f putenv_test.cc   
else
   (set -x ; g++ -Wall -Werror putenv_test.cc )
fi



cat >snprintf_test.cc <<EOF

#include <stdio.h>
#include <string.h>
#include <stddef.h>
#include <stdlib.h>

int main(int argc,char **argv)
{
	char *buf=(char *) malloc(200);
	snprintf(buf,200,"stuff");
}
EOF


if g++ -Wall -Werror snprintf_test.cc > /dev/null ; then
   AC_DEFINE(SNPRINTF_OK_IN_CPLUSPLUS,1,[Define to 1 if snprintf ok to use in c++ source.]) 
   \rm -f snprintf_test.cc   
else
   (set -x ; g++ -Wall -Werror snprintf_test.cc )
   CPPFLAGS="${CPPFLAGS} -DNO_SNPRINTF=1";
   export CPPFLAGS;
fi

AC_MSG_NOTICE([check_for_windows_stuff : ${check_for_windows_stuff}])


if test "x${check_for_windows_stuff}" = "xyes" ; then
   AC_DEFINE(HAVE_WINDOWS_H,1,[Define to 1 if you have windows.h])   

   if test "x${enable_sokintrf}" = "xyes" ; then
   
	ORIG_LIBS=${LIBS}

	AC_MSG_NOTICE([with_winsock2 : ${with_winsock2}])

	if test "x${with_winsock2}" = "xyes" ; then

	if test "x${build_os}" = "xcygwin" ; then
	   ac_cv_have_winsock2=yes;
	   LIBS="${LIBS} -lws2_32";
	   ac_cv_can_link_ws2_32=yes;
	   # the test for having winsock2.h will fail because of stupid
	   # unistd problem but we need it anyway.
	else   	
		AC_CHECK_HEADER([winsock2.h],
			    [	
			    LIBS="${LIBS} -lws2_32"
			    AC_CACHE_CHECK([can link ws2_32],
						ac_cv_can_link_ws2_32,
						[
						AC_TRY_LINK(
			    [
			    #include <windows.h>
			    #include <winsock2.h>
			    ],
			    [
				short winsock_version;
				WSADATA startup_data;
				winsock_version = MAKEWORD (2, 0);
				WSAStartup(winsock_version, &startup_data);
				WSAGetLastError();
				socket (AF_INET, SOCK_STREAM, 0);
				],
				[
				ac_cv_can_link_ws2_32=yes;
				],
				[
				ac_cv_can_link_ws2_32=no;
				]
				)
				ac_cv_have_winsock2=${ac_cv_can_link_ws2_32};
				])	
			],
			[ac_cv_have_winsock2=no]
		)
	fi

	else
		ac_cv_have_winsock2=no;

	fi
	# if test "${with_winsock2}" = "xyes" ; then

	## AC_MSG_NOTICE([ac_cv_have_winsock2=${ac_cv_have_winsock2}])


	if test "x${ac_cv_have_winsock2}" = "xyes" ; then
	   AC_DEFINE(HAVE_WINSOCK2_H,1,[Define to 1 you have a working winsock2.h  and ws2_32.lib system.])
	   ## AC_MSG_NOTICE([LIBS=${LIBS}])
	else

	   AC_MSG_NOTICE([with_winsock : ${with_winsock}])
		
	   if test "x${with_winsock}" = "xyes" ; then
           
           LIBS=${ORIG_LIBS}

	   AC_CHECK_HEADER([winsock.h],
			       [	
			       LIBS="${LIBS} -lwsock32"
			       AC_TRY_LINK(
			       [
			       #include <winsock.h>
			       #include <windows.h>
			       ],
			       [
					short winsock_version;
					WSADATA startup_data;

					winsock_version = MAKEWORD (2, 0);
					WSAStartup(winsock_version, &startup_data);
					WSAGetLastError();
					socket (AF_INET, SOCK_STREAM, 0);
				],
				[ac_cv_have_winsock=yes],
				[
				ac_cv_have_winsock=no;
				]
				)
				],
				[ac_cv_have_winsock=no]
			)


		if test "x${ac_cv_have_winsock}" = "xyes" ; then
		   AC_DEFINE(HAVE_WINSOCK_H,1, [Define to 1 you have a working winsock.h and wsock32.lib system.])
		else
		   LIBS=${ORIG_LIBS};
		fi


		fi
		#  test "${with_winsock}" = "xyes"

	
	fi
	# if test "x${ac_cv_have_winsock2}" = "xyes" 


   fi

	   ## AC_MSG_NOTICE([LIBS=${LIBS}])

   AC_CHECK_HEADER([winver.h],
		[ac_cv_have_winver_h=yes],
		[ac_cv_have_winver_h=no],
		[
			#include <windows.h>
		])
	
   if test "x${ac_cv_have_winver_h}" = "xyes" ; then
      AC_DEFINE(HAVE_WINVER_H,1,[Define to 1 if there is a working version of winver.h])
   fi
		
   AC_CHECK_HEADERS([toolhelp.h dos.h io.h])

   AC_CACHE_CHECK([for GetTickCount],
   ac_cv_have_gettickcount,
	[
	AC_TRY_LINK([
	      #include <windows.h>
	      #include <winbase.h>
	      ],
	      [
		GetTickCount();
	      ],
	     [
		ac_cv_have_gettickcount=yes
		],
		[ac_cv_have_gettickcount=no])
	]
    )

    if test "x${ac_cv_have_gettickcount}" = "xyes" ; then
	AC_DEFINE(HAVE_GETTICKCOUNT,1,[Define to 1 if you have the GetTickCount function.])
   fi

   AC_CACHE_CHECK([for QueryPerformanceCounter],
   ac_cv_have_queryperformancecounter,
	[
	AC_TRY_LINK([
	      #include <windows.h>
	      #include <winbase.h>
	      ],
	      [
		    BOOL frequency_ok = 0;
		    BOOL counter_ok = 0;
		    LARGE_INTEGER frequency;
		    LARGE_INTEGER counter;

		  frequency_ok = QueryPerformanceFrequency (&frequency);
		  counter_ok = QueryPerformanceCounter (&counter);
	      ],
	     [
	     ac_cv_have_queryperformancecounter=yes
	     ],
	     [ac_cv_have_queryperformancecounter=no]
	     )
	 ]
    )


    if test "x${ac_cv_have_queryperformancecounter}" = "xyes" ; then
       AC_DEFINE(HAVE_QUERYPERFORMANCEFREQUENCY,1, [Define to 1 if you have the QueryPerformanceFrequency  function.])
       AC_DEFINE(HAVE_QUERYPERFORMANCECOUNTER,1,[Define to 1 if you have the QueryPerformanceCounter  function.])
    fi

   AC_CACHE_CHECK([for SleepEx],
   ac_cv_have_sleepex,
            [
	    AC_TRY_LINK([
	      #include <windows.h>
	      #include <winbase.h>
	      ],
	      [
		SleepEx(1000,FALSE);
	      ],
	     [
		ac_cv_have_sleepex=yes
		],
		[ac_cv_have_sleepex=no]
	     )
	   ]
   )

   if test "x${ac_cv_have_sleepex}" = "xyes" ; then
      AC_DEFINE(HAVE_SLEEPEX,1,[Define to 1 if you have the SleepEx function.])
   fi

   AC_CACHE_CHECK([for Yield],
   ac_cv_have_yield,
            [
	    AC_TRY_LINK([
	      #include <windows.h>
	      #include <winbase.h>
	      ],
	      [
		Yield();
	      ],
	     [
		ac_cv_have_yield=yes
		],
		[ac_cv_have_yield=no]
	     )
	    ]
	 )

    if test "x${ac_cv_have_yield}" = "xyes" ; then
   	AC_DEFINE(HAVE_YIELD,1,[Define to 1 if you have the Yield function.])
    fi


      ##AC_SEARCH_LIBS(_beginthread,[msvcrt,crtdll])

	   AC_CHECK_HEADER([process.h],	   
	   [
	   AC_TRY_LINK([		   
	      #include <windows.h>
	      #if HAVE_WINBASE_H
	      #include <winbase.h>
	      #endif
	      #include <process.h>
	      ],
	      [
	       _beginthread (0,0,0);
	       TerminateThread ((HANDLE) 0, 0);
	      ],
	      [have_windows_thread_funcs=yes])
	   ])

	   if test "x${have_windows_thread_funcs}" = "xyes" ; then
	      AC_DEFINE(HAVE_PROCESS_H,1,[Define to 1 if you have the process.h file])
	      AC_DEFINE(HAVE__BEGINTHREAD,1,[Define to 1 if you have the _beginthread function.])
	      AC_DEFINE(HAVE_TERMINATETHREAD,1,[Define to 1 if you have the TerminateThread function])
	      AC_MSG_NOTICE([Using Microsoft Windows Thread API.])
	      with_posix_threads=no

	   fi

	   AC_TRY_LINK([		   
	      #include <windows.h>
	      #if HAVE_WINBASE_H
	      #include <winbase.h>
	      #endif
	      #if HAVE_PROCESS_H
	      #include <process.h>
	      #endif
	      ],
	      [
		DWORD pid = GetCurrentProcessId();
		DWORD tid = GetCurrentThreadId();
	      ],
	      [have_getcurrentprocessid=yes])
	   
	   if test "x${have_getcurrentprocessid}" = "xyes" ; then
	      AC_DEFINE(HAVE_GET_CURRENT_PROCESS_ID,1,[Define to 1 if you have the GetCurrentProcessId function])
	      AC_DEFINE(HAVE_GET_CURRENT_THREAD_ID,1,[Define to 1 if you have the GetCurrentThreadId function])
	   fi 

else
# if test "x${check_for_windows_stuff}" = "xyes" ; then

# Checks for libraries.

# Checks for header files.
AC_HEADER_SYS_WAIT

if test "x${enable_sokintrf}" = "xyes" -a \
   "x${ac_cv_have_winsock}" != "xyes" -a \
   "x${ac_cv_have_winsock2}" != "xyes" ; then
   
AC_CHECK_HEADER([arpa/inet.h],
	[AC_DEFINE(HAVE_ARPA_INET_H,1,[Define to 1 if you have <arpa/inet.h>])])
AC_CHECK_HEADER([fcntl.h],
		  [AC_DEFINE(HAVE_FCNTL_H,1,[Define to 1 if you have < fcntl.h>])])

AC_CHECK_HEADER([netdb.h],
		  [AC_DEFINE(HAVE_NETDB_H,1,[Define to 1 if you have < netdb.h>])])

AC_CHECK_HEADER([netinet/in.h],
		  [AC_DEFINE(HAVE_NETINET_IN_H,1,[Define to 1 if you have < netinet/in.h>])])


AC_CHECK_HEADER([linux/sockios.h],
		  [AC_DEFINE(HAVE_LINUX_SOCKIOS_H,1,[Define to 1 if you have < linux/sockios.h>])])

AC_CHECK_HEADER([inttypes.h],
		  [AC_DEFINE(HAVE_INTTYPES_H,1,[Define to 1 if you have <inttypes.h>])])

AC_CHECK_HEADER([sys/file.h],
		   [AC_DEFINE(HAVE_SYS_FILE_H,1,[Define to 1 if you have <  sys/file.h>])])

AC_CHECK_HEADER([ioctl.h],
		  [AC_DEFINE(HAVE_IOCTL_H,1,[Define to 1 if you have < ioctl.h>])])

AC_CHECK_HEADER([sys/ioctl.h],
		  [AC_DEFINE(HAVE_SYS_IOCTL_H,1,[Define to 1 if you have < sys/ioctl.h>])])

AC_CHECK_HEADER([sys/param.h],
		  [AC_DEFINE(HAVE_SYS_PARAM_H,1,[Define to 1 if you have < sys/param.h>])])

AC_CHECK_HEADER([uio.h],
		  [AC_DEFINE(HAVE_UIO_H,1,[Define to 1 if you have < uio.h>])])

AC_CHECK_HEADER([sys/uio.h],
		  [AC_DEFINE(HAVE_SYS_UIO_H,1,[Define to 1 if you have < sys/uio.h>])])

AC_CHECK_HEADER([net/uio.h],
		  [AC_DEFINE(HAVE_NET_UIO_H,1,[Define to 1 if you have < net/uio.h>])])

AC_CHECK_HEADER([socket.h],
		  [AC_DEFINE(HAVE_SOCKET_H,1,[Define to 1 if you have < socket.h>])])

AC_CHECK_HEADER([sys/socket.h],
		  [AC_DEFINE(HAVE_SYS_SOCKET_H,1,[Define to 1 if you have < sys/socket.h>])])

# Jan-10-2012 moving to after sys/sockt.h in hopes of fixing MAC problem.
# Nov-10-2011 causes error message on MAC. 
# Not sure if needed for anything else
AC_CHECK_HEADER([net/if.h],
		  [AC_DEFINE(HAVE_NET_IF_H,1,[Define to 1 if you have < net/if.h>])],
	[],
	[
#if HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#if HAVE_SOCKET_H
#include <socket.h>
#endif
#if HAVE_STANDARDS_H
#include <standards.h>
#endif
])

AC_CHECK_HEADER([sys/utsname.h],
		  [AC_DEFINE(HAVE_SYS_UTSNAME_H,1,[Define to 1 if you have < sys/utsname.h>])])

AC_CHECK_HEADER([sys/systeminfo.h],
		  [AC_DEFINE(HAVE_SYS_SYSTEMINFO_H,1,[Define to 1 if you have < sys/systeminfo.h>])])

AC_CHECK_HEADER([sys/time.h],
		  [AC_DEFINE(HAVE_SYS_TIME_H,1,[Define to 1 if you have < sys/time.h>])])

AC_CHECK_HEADER([sched.h],
		  [AC_DEFINE(HAVE_SCHED_H,1,[Define to 1 if you have < sched.h >])])

AC_CHECK_HEADER([syslog.h],
		  [AC_DEFINE(HAVE_SYSLOG_H,1,[Define to 1 if you have < syslog.h>])])

AC_CHECK_HEADER([termios.h],
		  [AC_DEFINE(HAVE_TERMIOS_H,1,[Define to 1 if you have < termios.h>])])

AC_CHECK_HEADER([unistd.h],
		  [AC_DEFINE(HAVE_UNISTD_H,1,[Define to 1 if you have < unistd.h>])])

if test "x${with_filio}" = "xyes" ; then

   AC_CHECK_HEADER([filio.h],
			  [AC_DEFINE(HAVE_FILIO_H,1,[Define to 1 if you have < filio.h>])])
    
   AC_CHECK_HEADER([sys/filio.h],
			  [AC_DEFINE(HAVE_SYS_FILIO_H,1,[Define to 1 if you have < sys/filio.h>])])

fi


AC_CHECK_HEADER([standards.h],
	[AC_DEFINE(HAVE_STANDARDS_H,1,[Define to 1 if you have <standards.h>])])

AC_CHECK_HEADER([netinet/tcp.h],
		  [AC_DEFINE(HAVE_NETINET_TCP_H,1,[Define to 1 if you have <netinet/tcp.h>])],
		  [
AC_CHECK_HEADER(netinet/tcp.h,
	[AC_DEFINE(HAVE_NETINET_TCP_H_2,1,[Define to 1 if you have the <netinet/tcp.h> header file])],
	[],
	[
#if HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#if HAVE_SOCKET_H
#include <socket.h>
#endif
#if HAVE_STANDARDS_H
#include <standards.h>
#endif
])
	])


AC_CHECK_HEADERS([sys/shm.h])

fi
# if test "x${enable_sokintrf}" = "xyes" -a "x${ac_cv_have_winsock}" != "xyes" -a "x${ac_cv_have_winsock2}" != "xyes"


fi
# if test "x${check_for_windows_stuff}" = "xyes" 


if test "x${build_os}" = "xcygwin"  -a "x${enable_xdr}" = "xyes" ; then

   AC_MSG_NOTICE([Using nt_xdr directory for cygwin.])

   ac_cv_have_xdr_h=yes;
   CPPFLAGS="${CPPFLAGS} -I${srcdir}/src/nt_xdr -DCYGWIN=1";
   
   AC_MSG_NOTICE([CPPFLAGS : ${CPPFLAGS} ] )

   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS}  src/nt_xdr/xdr.lo"
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS}  src/nt_xdr/xdr_arra.lo"
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS}  src/nt_xdr/xdr_floa.lo"
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS}  src/nt_xdr/xdr_mem.lo"
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS}  src/nt_xdr/xdr_rec.lo"
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS}  src/nt_xdr/xdr_refe.lo"
   RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS}  src/nt_xdr/xdr_stdi.lo";
   AC_MSG_NOTICE([RCSLIB_EXTRA_OBJS : ${RCSLIB_EXTRA_OBJS} ] )


elif test "x${enable_xdr}" = "xyes" ; then

   AC_CHECK_HEADER([rpc/rpc.h],
		       [ac_cv_have_rpc_rpc_h=yes],
		       [ac_cv_have_rpc_rpc_h=no])

   if test "x${ac_cv_have_rpc_rpc_h}" = "xyes" ; then
      AC_DEFINE(HAVE_RPC_RPC_H,1,[Define to 1 if you have the <rpc/rpc.h> header file.])
   else 

	AC_CHECK_HEADER([rpc/types.h],
			[ac_cv_have_rpc_types_h=yes],
			[ac_cv_have_rpc_types_h=no])

	if test "x${ac_cv_have_rpc_types_h}" = "xyes" ; then
	   AC_DEFINE(HAVE_RPC_TYPES_H,1,[Define to 1 if you have the <rpc/types.h> header file.])
	fi

	AC_CHECK_HEADER([rpc/xdr.h],
		[ac_cv_have_rpc_xdr_h=yes],
		[ac_cv_have_rpc_xdr_h=no],
		[
#  if HAVE_RPC_TYPES_H
#   include <rpc/types.h>
#  endif
		])

	if test "x${ac_cv_have_rpc_xdr_h}" = "xyes" ; then
	   AC_DEFINE(HAVE_RPC_XDR_H,1,[Define to 1 if you have the <rpc/xdr.h> header file.])
	fi

	AC_CHECK_HEADER([xdr.h],
			[ac_cv_have_xdr_h=yes],
			[ac_cv_have_xdr_h=no])

       if test "x${ac_cv_have_xdr_h}" = "xno" -a "x${ac_cv_have_rpc_rpc_h}" = "xno" -a  "x${ac_cv_have_rpc_xdr_h}" = "xno" -a "x${check_for_windows_stuff}" = "xyes" ; then

	  ac_cv_have_xdr_h=yes
	  CPPFLAGS="${CPPFLAGS} -I${srcdir}/src/nt_xdr";
	  ## AC_MSG_NOTICE([CPPFLAGS : ${CPPFLAGS}])
	  ##RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS}  xdr.lo xdr_arra.lo xdr_floa.lo xdr_mem.lo xdr_rec.lo xdr_refe.lo xdr_stdi.lo";
	  RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS}  src/nt_xdr/xdr.lo"
          RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS}  src/nt_xdr/xdr_arra.lo"
          RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS}  src/nt_xdr/xdr_floa.lo"
          RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS}  src/nt_xdr/xdr_mem.lo"
          RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS}  src/nt_xdr/xdr_rec.lo"
          RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS}  src/nt_xdr/xdr_refe.lo"
          RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS}  src/nt_xdr/xdr_stdi.lo";
          ## AC_MSG_NOTICE([RCSLIB_EXTRA_OBJS : ${RCSLIB_EXTRA_OBJS}])
      fi

    
    # -- end of if -- test "x${ac_cv_have_rpc_rpc_h}" = "xyes" ;
    fi

fi

# // if test  test "x${enable_xdr}" = "xyes"

if test "x${ac_cv_have_xdr_h}" = "xyes" ; then
   AC_DEFINE(HAVE_XDR_H,1,[Define to 1 if you have the <xdr.h> header file.])
fi

AC_CHECK_HEADERS([sys/fcntl.h sys/stat.h sys/mman.h  sys/sysinfo.h])


if test "x${with_iostream}" = "xyes" ; then

   AC_LANG_PUSH(C++)
   AC_CHECK_HEADERS(iostream.h)
   AC_LANG_POP

else
   
   AC_DEFINE(NO_IOSTREAM,1,[Disable support for printing posemath stuff with iostreams])

fi

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_C_INLINE
AC_TYPE_MODE_T
AC_TYPE_PID_T
AC_TYPE_SIZE_T
AC_HEADER_TIME
AC_STRUCT_TM
AC_TYPE_UID_T

AC_CHECK_TYPE([socklen_t],
	[AC_DEFINE(HAVE_SOCKLEN_T_TYPE,1,[Define to 1 if you have the socklen_t type.])])


AC_CHECK_TYPE([key_t],
	[AC_DEFINE(HAVE_KEY_T_TYPE,1,[Define to 1 if you have the key_t type.])],,
	[

#if HAVE_WINSOCK2_H && HAVE_WINDOWS_H
#include <winsock2.h>
#include <windows.h>
#endif

#if HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif
#if HAVE_SYS_STAT_H
# include <sys/stat.h>
#endif
#if STDC_HEADERS
# include <stdlib.h>
# include <stddef.h>
#else
# if HAVE_STDLIB_H
#  include <stdlib.h>
# endif
#endif
#if HAVE_STRING_H
# if !STDC_HEADERS && HAVE_MEMORY_H
#  include <memory.h>
# endif
# include <string.h>
#endif
#if HAVE_STRINGS_H
# include <strings.h>
#endif
#if HAVE_INTTYPES_H
# include <inttypes.h>
#else
# if HAVE_STDINT_H
#  include <stdint.h>
# endif
#endif

#if ! HAVE_WINSOCK2_H
#if HAVE_UNISTD_H
# include <unistd.h>
#endif
#if HAVE_SYS_UIO_H
# include <sys/uio.h>
#else
#if HAVE_NET_UIO_H
# include <net/uio.h>
#endif
#endif
#if HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif

#endif
/*  HAVE_WINSOCK2_H && HAVE_WINDOWS_H */
	]
)


AC_CHECK_TYPE([caddr_t],
	[AC_DEFINE(HAVE_CADDR_T_TYPE,1,[Define to 1 if you have the caddr_t type.])],,
	[

#if HAVE_WINSOCK2_H && HAVE_WINDOWS_H
#include <winsock2.h>
#include <windows.h>
#endif

#if HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif
#if HAVE_SYS_STAT_H
# include <sys/stat.h>
#endif
#if STDC_HEADERS
# include <stdlib.h>
# include <stddef.h>
#else
# if HAVE_STDLIB_H
#  include <stdlib.h>
# endif
#endif
#if HAVE_STRING_H
# if !STDC_HEADERS && HAVE_MEMORY_H
#  include <memory.h>
# endif
# include <string.h>
#endif
#if HAVE_STRINGS_H
# include <strings.h>
#endif
#if HAVE_INTTYPES_H
# include <inttypes.h>
#else
# if HAVE_STDINT_H
#  include <stdint.h>
# endif
#endif

#if ! HAVE_WINSOCK2_H
#if HAVE_UNISTD_H
# include <unistd.h>
#endif
#if HAVE_SYS_UIO_H
# include <sys/uio.h>
#else
#if HAVE_NET_UIO_H
# include <net/uio.h>
#endif
#endif
#if HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif

#endif
/*  HAVE_WINSOCK2_H && HAVE_WINDOWS_H */
	]
)


AC_CHECK_TYPE([struct iovec],
	[AC_DEFINE(HAVE_STRUCT_IOVEC_TYPE,1,[Define to 1 if you have the struct iovec type.])],,
	[
#if HAVE_WINSOCK2_H && HAVE_WINDOWS_H
#include <winsock2.h>
#include <windows.h>
#endif

#if HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif
#if HAVE_SYS_STAT_H
# include <sys/stat.h>
#endif
#if STDC_HEADERS
# include <stdlib.h>
# include <stddef.h>
#else
# if HAVE_STDLIB_H
#  include <stdlib.h>
# endif
#endif
#if HAVE_STRING_H
# if !STDC_HEADERS && HAVE_MEMORY_H
#  include <memory.h>
# endif
# include <string.h>
#endif
#if HAVE_STRINGS_H
# include <strings.h>
#endif
#if HAVE_INTTYPES_H
# include <inttypes.h>
#else
# if HAVE_STDINT_H
#  include <stdint.h>
# endif
#endif

#if ! HAVE_WINSOCK2_H
#if HAVE_UNISTD_H
# include <unistd.h>
#endif
#if HAVE_SYS_UIO_H
# include <sys/uio.h>
#else
#if HAVE_NET_UIO_H
# include <net/uio.h>
#endif
#endif
#if HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif	

#endif
/*  HAVE_WINSOCK2_H && HAVE_WINDOWS_H */
	]
)

AC_CHECK_TYPE([struct msghdr],
	[AC_DEFINE(HAVE_STRUCT_MSGHDR_TYPE,1,[Define to 1 if you have the struct msghdr type.])],,
	[
#if HAVE_WINSOCK2_H && HAVE_WINDOWS_H
#include <winsock2.h>
#include <windows.h>
#endif

#if HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif
#if HAVE_SYS_STAT_H
# include <sys/stat.h>
#endif
#if STDC_HEADERS
# include <stdlib.h>
# include <stddef.h>
#else
# if HAVE_STDLIB_H
#  include <stdlib.h>
# endif
#endif
#if HAVE_STRING_H
# if !STDC_HEADERS && HAVE_MEMORY_H
#  include <memory.h>
# endif
# include <string.h>
#endif
#if HAVE_STRINGS_H
# include <strings.h>
#endif
#if HAVE_INTTYPES_H
# include <inttypes.h>
#else
# if HAVE_STDINT_H
#  include <stdint.h>
# endif
#endif

#if ! HAVE_WINSOCK2_H

#if HAVE_UNISTD_H
# include <unistd.h>
#endif
#if HAVE_SYS_UIO_H
# include <sys/uio.h>
#else
#if HAVE_NET_UIO_H
# include <net/uio.h>
#endif
#endif
#if HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif	

#endif
/*  HAVE_WINSOCK2_H && HAVE_WINDOWS_H */
	]
)



AC_CHECK_TYPE([struct sockaddr_in],
	[AC_DEFINE(HAVE_STRUCT_SOCKADDR_IN_TYPE,1,[Define to 1 if you have the struct msghdr type.])],,
	[
#if HAVE_WINSOCK2_H && HAVE_WINDOWS_H
#include <winsock2.h>
#include <windows.h>
#endif

#if HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif
#if HAVE_SYS_STAT_H
# include <sys/stat.h>
#endif
#if STDC_HEADERS
# include <stdlib.h>
# include <stddef.h>
#else
# if HAVE_STDLIB_H
#  include <stdlib.h>
# endif
#endif
#if HAVE_STRING_H
# if !STDC_HEADERS && HAVE_MEMORY_H
#  include <memory.h>
# endif
# include <string.h>
#endif
#if HAVE_STRINGS_H
# include <strings.h>
#endif
#if HAVE_INTTYPES_H
# include <inttypes.h>
#else
# if HAVE_STDINT_H
#  include <stdint.h>
# endif
#endif

#if ! HAVE_WINSOCK2_H

#if HAVE_UNISTD_H
# include <unistd.h>
#endif
#if HAVE_SYS_UIO_H
# include <sys/uio.h>
#else
#if HAVE_NET_UIO_H
# include <net/uio.h>
#endif
#endif
#if HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif	
#if HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#endif
/*  HAVE_WINSOCK2_H && HAVE_WINDOWS_H */
	]
)

AC_CHECK_TYPE([struct sockaddr_storage],
	[AC_DEFINE(HAVE_STRUCT_SOCKADDR_STORAGE_TYPE,1,[Define to 1 if you have the struct msghdr type.])],,
	[
#if HAVE_WINSOCK2_H && HAVE_WINDOWS_H
#include <winsock2.h>
#include <windows.h>
#endif

#if HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif
#if HAVE_SYS_STAT_H
# include <sys/stat.h>
#endif
#if STDC_HEADERS
# include <stdlib.h>
# include <stddef.h>
#else
# if HAVE_STDLIB_H
#  include <stdlib.h>
# endif
#endif
#if HAVE_STRING_H
# if !STDC_HEADERS && HAVE_MEMORY_H
#  include <memory.h>
# endif
# include <string.h>
#endif
#if HAVE_STRINGS_H
# include <strings.h>
#endif
#if HAVE_INTTYPES_H
# include <inttypes.h>
#else
# if HAVE_STDINT_H
#  include <stdint.h>
# endif
#endif

#if ! HAVE_WINSOCK2_H

#if HAVE_UNISTD_H
# include <unistd.h>
#endif
#if HAVE_SYS_UIO_H
# include <sys/uio.h>
#else
#if HAVE_NET_UIO_H
# include <net/uio.h>
#endif
#endif
#if HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif	
#if HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#endif
/*  HAVE_WINSOCK2_H && HAVE_WINDOWS_H */
	]
)


AC_CHECK_TYPE([struct sockaddr_in6],
	[AC_DEFINE(HAVE_STRUCT_SOCKADDR_IN6_TYPE,1,[Define to 1 if you have the struct sockaddr_in6 type.])],,
	[
#if HAVE_WINSOCK2_H && HAVE_WINDOWS_H
#include <winsock2.h>
#include <windows.h>
#endif

#if HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif
#if HAVE_SYS_STAT_H
# include <sys/stat.h>
#endif
#if STDC_HEADERS
# include <stdlib.h>
# include <stddef.h>
#else
# if HAVE_STDLIB_H
#  include <stdlib.h>
# endif
#endif
#if HAVE_STRING_H
# if !STDC_HEADERS && HAVE_MEMORY_H
#  include <memory.h>
# endif
# include <string.h>
#endif
#if HAVE_STRINGS_H
# include <strings.h>
#endif
#if HAVE_INTTYPES_H
# include <inttypes.h>
#else
# if HAVE_STDINT_H
#  include <stdint.h>
# endif
#endif

#if ! HAVE_WINSOCK2_H

#if HAVE_UNISTD_H
# include <unistd.h>
#endif
#if HAVE_SYS_UIO_H
# include <sys/uio.h>
#else
#if HAVE_NET_UIO_H
# include <net/uio.h>
#endif
#endif
#if HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif	
#if HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#endif
/*  HAVE_WINSOCK2_H && HAVE_WINDOWS_H */
	]
)

AC_CHECK_TYPE([struct addrinfo],
	[AC_DEFINE(HAVE_STRUCT_ADDRINFO_TYPE,1,[Define to 1 if you have the struct addrinfo.])],,
	[
#if HAVE_WINSOCK2_H && HAVE_WINDOWS_H
#include <winsock2.h>
#include <windows.h>
#endif

#if HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif
#if HAVE_SYS_STAT_H
# include <sys/stat.h>
#endif
#if STDC_HEADERS
# include <stdlib.h>
# include <stddef.h>
#else
# if HAVE_STDLIB_H
#  include <stdlib.h>
# endif
#endif
#if HAVE_STRING_H
# if !STDC_HEADERS && HAVE_MEMORY_H
#  include <memory.h>
# endif
# include <string.h>
#endif
#if HAVE_STRINGS_H
# include <strings.h>
#endif
#if HAVE_INTTYPES_H
# include <inttypes.h>
#else
# if HAVE_STDINT_H
#  include <stdint.h>
# endif
#endif

#if ! HAVE_WINSOCK2_H

#if HAVE_UNISTD_H
# include <unistd.h>
#endif
#if HAVE_SYS_UIO_H
# include <sys/uio.h>
#else
#if HAVE_NET_UIO_H
# include <net/uio.h>
#endif
#endif
#if HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif	
#if HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#if HAVE_NETDB_H
#include <netdb.h>
#endif

#endif
/*  HAVE_WINSOCK2_H && HAVE_WINDOWS_H */
	]
)


AC_CHECK_TYPE([struct ifreq],
	[AC_DEFINE(HAVE_STRUCT_IFREQ_TYPE,1,[Define to 1 if you have the struct ifreq type.])],,
	[
#if HAVE_WINSOCK2_H && HAVE_WINDOWS_H
#include <winsock2.h>
#include <windows.h>
#endif

#if HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif
#if HAVE_SYS_STAT_H
# include <sys/stat.h>
#endif
#if STDC_HEADERS
# include <stdlib.h>
# include <stddef.h>
#else
# if HAVE_STDLIB_H
#  include <stdlib.h>
# endif
#endif
#if HAVE_STRING_H
# if !STDC_HEADERS && HAVE_MEMORY_H
#  include <memory.h>
# endif
# include <string.h>
#endif
#if HAVE_STRINGS_H
# include <strings.h>
#endif
#if HAVE_INTTYPES_H
# include <inttypes.h>
#else
# if HAVE_STDINT_H
#  include <stdint.h>
# endif
#endif

#if ! HAVE_WINSOCK2_H

#if HAVE_UNISTD_H
# include <unistd.h>
#endif
#if HAVE_SYS_UIO_H
# include <sys/uio.h>
#else
#if HAVE_NET_UIO_H
# include <net/uio.h>
#endif
#endif
#if HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif	
#if HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#if HAVE_NET_IF_H
#include <net/if.h>
#endif
#if HAVE_LINUX_SOCKIOS_H
#include <linux/sockios.h>
#endif

#endif
/*  HAVE_WINSOCK2_H && HAVE_WINDOWS_H */
	]
)


if test "x${check_for_windows_stuff}" = "xyes" ; then


   AC_CHECK_TYPE([u_int],
		       [ac_cv_have_u_int=yes],
		       [ac_cv_have_u_int=no],
		       [
		       #include <windows.h>
		       #if HAVE_WINSOCK2_H
		       #include <winsock2.h>
		       #endif
		       ])

     if test "x${ac_cv_have_u_int}" = "xyes" ; then	
	AC_DEFINE(HAVE_U_INT,1,[Define to 1 if you have the u_int type])
     fi

       AC_CHECK_TYPE([u_short],
		       [ac_cv_have_u_short=yes],
		       [ac_cv_have_u_short=no],
		       [
		       #include <windows.h>
		       #if HAVE_WINSOCK2_H
		       #include <winsock2.h>
		       #endif
		       ])
     
     if test "x${ac_cv_have_u_short}" = "xyes" ; then	
	AC_DEFINE(HAVE_U_SHORT,1,[Define to 1 if you have the u_short type])
     fi


     AC_CHECK_TYPE([u_long],
		       [ac_cv_have_u_long=yes],
		       [ac_cv_have_u_long=no],
		       [
		       #include <windows.h>
		       #if HAVE_WINSOCK2_H
		       #include <winsock2.h>
		       #endif
		       ])

     if test "x${ac_cv_have_u_long}" = "xyes" ; then	
	AC_DEFINE(HAVE_U_LONG,1,[Define to 1 if you have the u_int type])
     fi

else 
     AC_CHECK_TYPE([u_int])
     AC_CHECK_TYPE([u_long])
     AC_CHECK_TYPE([u_short])

fi


# Checks for library functions.
AC_PROG_GCC_TRADITIONAL

if test "x${enable_xdr}" = "xyes" ; then
   AC_SEARCH_LIBS(xdrmem_create,[rpc rpcsvc xdr nsl rpcS])
   AC_SEARCH_LIBS(xdr_short,[rpc rpcsvc xdr nsl rpcS])
fi

if test "x${enable_sokintrf}" = "xyes" ; then
   AC_SEARCH_LIBS(socket,[socket xnet ws2_32 winsock])
   AC_CHECK_LIB(m,fabs)
fi

AC_SEARCH_LIBS(gethostbyname,[socket nsl resolv xnet])

if test "x${with_librt}" = "xyes" ; then

   AC_SEARCH_LIBS(clock_gettime,[rt posix4] )
   AC_SEARCH_LIBS(shm_open,[rt posix4])

else

   AC_SEARCH_LIBS(clock_gettime,[posix4] )
   AC_SEARCH_LIBS(shm_open,[posix4])

fi

AC_CHECK_FUNCS([clock_gettime getcwd getenv setenv putenv gethostbyaddr gethostbyname gethostname gettimeofday getwd inet_ntoa inet_aton inet_ntop inet_pton getaddrinfo gai_strerror getnameinfo select socket sqrt strchr strcspn strdup strerror strpbrk strrchr strstr strtol strtoul toupper uname sysinfo sysconf sleep usleep nanosleep setitimer sincos fstat stat opendir readdir gethostbyname_r time localtime getsockopt setsockopt raise abort sync sched_yield kill fork send sendto sendmsg recv recvfrom recvmsg])

if test "x${enable_force_ipc_rm}" != "xyes" ; then
   AC_DEFINE(NEVER_FORCE_IPC_RM,1,[Never forcibly delete IPC resources.])
fi

if test "x${with_snprintf}" = "xyes" ; then
   AC_CHECK_FUNCS(snprintf _snprintf vsnprintf _vsnprintf)
else
   CPPFLAGS="${CPPFLAGS} -DNO_SNPRINTF=1";
   export CPPFLAGS;
fi

if test "x${ac_cv_have_winsock}" != "xyes" -a "x${ac_cv_have_winsock2}" != "xyes" ;  then

   AC_CHECK_FUNC([socketpair],
   [AC_DEFINE(HAVE_SOCKETPAIR,1,[Define to 1 if you have the socketpair function])])

fi

if test "x${with_posix_threads}" = "xyes" ; then

   AC_MSG_NOTICE([checking POSIX threads])

   AC_CHECK_HEADER([pthread.h],
		[AC_DEFINE(HAVE_PTHREAD_H,1,[Define to 1 if you have the pthread.h file])])

    AC_CHECK_LIB(pthread,pthread_create)

    AC_CHECK_FUNC(pthread_create,
    [AC_CHECK_FUNC(pthread_kill,
    [AC_CHECK_FUNC(pthread_join,
    [AC_CHECK_FUNC(pthread_exit,
		[AC_DEFINE(POSIX_THREADS,1,[if we should use POSIX threads])
		use_pthreads=yes
		]
    )
    # AC_CHECK_FUNC pthread_exit
    ])
    # AC_CHECK_FUNC pthread_join
    ])
    # AC_CHECK_FUNC pthread_kill
    ])
    # AC_CHECK_FUNC pthread_create

# with_posix_threads
fi

if test "x${use_pthreads}" != "xyes" ; then

	if test "x${have_windows_thread_funcs}" != "xyes" ; then

	   AC_CHECK_FUNC(fork,
	   [AC_CHECK_FUNC(kill,	
	   [AC_CHECK_FUNC(waitpid,
		[can_use_fork=yes])
	    ])])
	 
	   if test "x${can_use_fork}" = "xyes" ; then	 
		AC_DEFINE(NO_THREADS,1,[if no thread system is available])
	   else
		AC_MSG_ERROR([NO Threads implementation seems to be available
and even fork will not work.])
           fi
        fi
fi

if test "x${enable_shmem}" = "xyes" ; then

windows_shared_memory_available=no;
sys_v_shared_memory_available=no;
posix_shared_memory_available=no;

if test "x${ac_cv_have_winsock}" != "xyes" -a "x${ac_cv_have_winsock2}" != "xyes" ;  then

   AC_MSG_NOTICE([checking System V shared memory])

   AC_CHECK_HEADER([ipc.h],
		[AC_DEFINE(HAVE_IPC_H,1,[Define to 1 if you have <ipc.h>])])

   AC_CHECK_HEADER([shm.h],
	[AC_DEFINE(HAVE_SHM_H,1,[Define to 1 if you have <shm.h>])])


   AC_CHECK_HEADER([sys/ipc.h],
		[AC_DEFINE(HAVE_SYS_IPC_H,1,[Define to 1 if you have <sys/ipc.h>])])

   AC_CHECK_HEADER([sys/shm.h],
	[AC_DEFINE(HAVE_SYS_SHM_H,1,[Define to 1 if you have <sys/shm.h>])])

	AC_CHECK_FUNC(shmget,
[AC_CHECK_FUNC(shmctl,
[AC_CHECK_FUNC(shmat,
[AC_CHECK_FUNC(shmdt,
[
	sys_v_shared_memory_available=yes
	AC_DEFINE(SYS_V_SHAREDMEM_AVAILABLE,1,[Can we use SystemV shared memory?])
])
#shmdt
])
#shmat
])
#shmctl
])
#shmget


AC_MSG_NOTICE([checking POSIX shared memory])

AC_CHECK_FUNC(shm_open,
[AC_CHECK_FUNC(shm_unlink,
[AC_CHECK_FUNC(ftruncate,
[AC_CHECK_FUNC(mmap,
[AC_CHECK_FUNC(munmap,
	[AC_MSG_CHECKING([can use POSIX shared memory with mmap])
	AC_TRY_RUN([
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#if HAVE_FCNTL_H
#include <fcntl.h>
#endif
#if HAVE_ERRNO_H
#include <errno.h>
#endif
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#if HAVE_LIMITS_H
#include <limits.h>
#endif
#if HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif

#define SHMEM_TEST_NAME "/confshmtest"

int main( int argc, char** argv )
{
    int fd, fd2;
    unsigned* addr;
    unsigned* addr2;

    /* Create a new memory object */
    fd = shm_open( SHMEM_TEST_NAME , O_RDWR | O_CREAT, 0777 );
    if( fd == -1 ) {
	perror("shm_open");
	exit(1);
    }
    
    /* Set the memory object's size */
    if( ftruncate( fd, (off_t)4 ) == -1 ) {
	perror("ftruncate");
    }

    /* Map the memory object */
    addr = mmap( 0, sizeof( *addr ),
            PROT_READ | PROT_WRITE,
            MAP_SHARED, fd, 0 );
#ifdef MAP_FAILED
    if( addr == MAP_FAILED ) {
    	perror("mmap");
	exit(1);
	}
#endif
    if(addr == 0)
    {
    	perror("mmap");
        exit(1);
    }


    /* Write to shared memory */
    *addr = 12;

    /* Create a new memory object */	
    fd2 = shm_open( SHMEM_TEST_NAME , O_RDWR, 0777 );
    if( fd2 == -1 ) {
	perror("shm_open");
	exit(1);
    }
    
    /* Set the memory object's size */
    if( ftruncate( fd2, (off_t)4 ) == -1 ) {
	perror("ftruncate");
    }

    /* Map the memory object */
    addr2 = mmap( 0, sizeof( *addr2 ),
            PROT_READ | PROT_WRITE,
            MAP_SHARED, fd2, 0 );
#ifdef MAP_FAILED
    if( addr2 == MAP_FAILED ) {
    	perror("mmap");
	exit(1);
	}
#endif
    if(addr2 == 0)
    {
    	perror("mmap");
        exit(1);
    }
    
    /* Test to see that it has the value it is supposed to. */
    if(*addr2 != *addr)
    {
	fprintf(stderr,"memory areas do NOT match\n");
	exit(1);
	}

    /*
     * The memory object remains in
     * the system after the close
     */
    close( fd );
    close(fd2);

    /*
     * To remove a memory object
     * you must unlink it like a file.
     *
     * This may be done by another process.
     */
    shm_unlink( SHMEM_TEST_NAME );

    exit(0);
}
],
	[posix_shared_memory_available=yes
	AC_DEFINE(POSIX_SHAREDMEM_AVAILABLE,1,[Can we use POSIX shared memory?])
	AC_MSG_RESULT(yes)],
	[AC_TRY_RUN([
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#if HAVE_FCNTL_H
#include <fcntl.h>
#endif
#if HAVE_ERRNO_H
#include <errno.h>
#endif
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#if HAVE_LIMITS_H
#include <limits.h>
#endif
#if HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif

#define SHMEM_TEST_NAME "/tmp/confshmtest"

int main( int argc, char** argv )
{
    int fd, fd2;
    unsigned* addr;
    unsigned* addr2;

    /* Create a new memory object */
    fd = shm_open( SHMEM_TEST_NAME , O_RDWR | O_CREAT, 0777 );
    if( fd == -1 ) {
	perror("shm_open");
	exit(1);
    }
    
    /* Set the memory object's size */
    if( ftruncate( fd, sizeof( *addr ) ) == -1 ) {
	perror("ftruncate");
	exit(1);
    }

    /* Map the memory object */
    addr = mmap( 0, sizeof( *addr ),
            PROT_READ | PROT_WRITE,
            MAP_SHARED, fd, 0 );
#ifdef MAP_FAILED
    if( addr == MAP_FAILED ) {
	perror("mmap");
	exit(1);
	}
#endif
    if(addr == 0)
    {
	perror("mmap");
        exit(1);
    }


    /* Write to shared memory */
    *addr = 12;

/* Create a new memory object */	
    fd2 = shm_open( SHMEM_TEST_NAME , O_RDWR, 0777 );
    if( fd2 == -1 ) {
	perror("shm_open");
	exit(1);
    }
    
    /* Set the memory object's size */
    if( ftruncate( fd2, sizeof( *addr ) ) == -1 ) {
	perror("ftruncate");
	exit(1);
    }

    /* Map the memory object */
    addr2 = mmap( 0, sizeof( *addr2 ),
            PROT_READ | PROT_WRITE,
            MAP_SHARED, fd2, 0 );
#ifdef MAP_FAILED
    if( addr2 == MAP_FAILED ) {
    	perror("mmap");
	exit(1);
	}
#endif
    if(addr2 == 0)
    {
    	perror("mmap");
        exit(1);
    }
    
    /* Test to see that it has the value it is supposed to. */
    if(*addr2 != *addr)
    {
	fprintf(stderr,"memory areas do NOT match\n");
	exit(1);
	}

    /*
     * The memory object remains in
     * the system after the close
     */
    close( fd );
    close(fd2);

    /*
     * To remove a memory object
     * you must unlink it like a file.
     *
     * This may be done by another process.
     */
    shm_unlink( SHMEM_TEST_NAME );

    exit(0);
}
],	
		[AC_DEFINE_UNQUOTED(POSIX_SHMEM_NAME_PREFIX,[\"/tmp\"],
			[prefix all shmem names with this string])
		posix_shared_memory_available=yes
		AC_DEFINE(POSIX_SHAREDMEM_AVAILABLE,1,[Can we use POSIX shared memory?])
		AC_MSG_RESULT([yes -- needs /tmp prefix])],
		[AC_MSG_RESULT(no)])],
	[posix_shared_memory_available=yes
	AC_DEFINE(POSIX_SHAREDMEM_AVAILABLE,1,[Can we use POSIX shared memory?])
	AC_MSG_RESULT([not tested])])
# AC_TRY_RUN
])
# AC_TRY_FUNC munmap
])
# AC_TRY_FUNC mmap
])
# AC_TRY_FUNC ftruncate
])
# AC_TRY_FUNC shm_unlink
])
# AC_TRY_FUNC shm_open

fi
# "x${ac_cv_have_winsock}" != "xyes" -a "x${ac_cv_have_winsock2}" != "xyes"


if test "x${posix_shared_memory_available}" = "xyes" -a  "x${sys_v_shared_memory_available}" = "xyes" ; then

   AC_MSG_NOTICE([Both System V and Posix Shared_Memory are available])

   if test x$with_posix_ipc = xyes -o x$with_sys_v_ipc = xno ; then
      AC_MSG_NOTICE([Using POSIX Shared_Memory])
      AC_DEFINE(USE_POSIX_SHAREDMEM,1,[Should we use POSIX Shared Memory])
      CONFIGURED_PROGRAMS="${CONFIGURED_PROGRAMS} "'posix_shm_unlink$(EXEEXT)'
   else
      AC_MSG_NOTICE([Using System V Shared_Memory])
      AC_DEFINE(SYS_V_SHARED_MEMORY,1,[Should we use System V Shared Memory])	
   fi

else

    if test x$posix_shared_memory_available = xyes ; then
       AC_MSG_NOTICE([Using POSIX Shared_Memory]) 
       AC_DEFINE(USE_POSIX_SHAREDMEM,1,[Should we use POSIX Shared Memory])
      CONFIGURED_PROGRAMS="${CONFIGURED_PROGRAMS} "'posix_shm_unlink$(EXEEXT)'
    else
	if test x$sys_v_shared_memory_available = xyes ; then
	   AC_MSG_NOTICE([Using System V Shared_Memory])
	   AC_DEFINE(SYS_V_SHARED_MEMORY,1,[Should we use POSIX Shared_Memory])
	else
	   if test "x${ac_cv_have_winsock}" != "xyes" -a "x${ac_cv_have_winsock2}" != "xyes" ; then
	       AC_MSG_NOTICE([Neither POSIX nor System V shared memory seem to be useable]);
	   fi

	   AC_MSG_NOTICE([Checking for windows shared memory.])
	   windows_check_failed=false;
	   if test "x${check_for_windows_stuff}" != "xyes" ;then
	      windows_check_failed=true
	   fi
	   if test "x${windows_check_failed}" != "xtrue" ; then
	   	   AC_CHECK_HEADER([winbase.h],,windows_check_failed=true,[#include<windows.h>])
           fi
	   if test "x${windows_check_failed}" != "xtrue" ; then
	      AC_TRY_LINK([
	      #include <windows.h>
	      #include <winbase.h>
	      ],
	      [
	        SECURITY_ATTRIBUTES sa;
		SECURITY_DESCRIPTOR sd;
		size_t size=0x1000;
		HANDLE hFileMap;
		const char *name="config_test_shm";
		void *addr;

		InitializeSecurityDescriptor (&sd, SECURITY_DESCRIPTOR_REVISION);
		sa.nLength = sizeof (SECURITY_ATTRIBUTES);
		sa.lpSecurityDescriptor = &sd;
		sa.bInheritHandle = TRUE;

		hFileMap = CreateFileMapping ((HANDLE) (0xFFFFFFFF), &sa, PAGE_READWRITE, 0,size, name);
		addr=MapViewOfFile (hFileMap, FILE_MAP_ALL_ACCESS, 0, 0, size);		
		UnmapViewOfFile(addr);
		CloseHandle(hFileMap);
		],
		[
		AC_DEFINE(HAVE_INITIALIZESECURITYDESCRIPTOR,1,[Define to 1 if you have the InitializeSecurityDescriptor function.])
		AC_DEFINE(HAVE_CREATEFILEMAPPING,1,[Define to 1 if you have the CreateFileMapping function.])
		AC_DEFINE(HAVE_MAPVEIWOFFILE,1,[Define to 1 if you have the MapViewOfFile function.])
		AC_DEFINE(HAVE_UNMAPVEIWOFFILE,1,[Define to 1 if you have the UnmapViewOfFile function.])
		AC_DEFINE(HAVE_CLOSEHANDLE,1,[Define to 1 if you have the CloseHandle function.])
		]
		,[windows_check_failed=true])
           fi

	   if test "x${windows_check_failed}" != "xtrue" ; then

	      AC_MSG_NOTICE([Using windows shared memory.])
	      windows_shared_memory_available=yes
	      AC_DEFINE(MS_WINDOWS_API,1,[Define to 1 if you should use the Microsoft Windows API for threads/sockets/IPC etc.])
	   else
		AC_MSG_ERROR([NO Shared Memory seems go be available. Consider --disable-shmem])
	   fi
	fi
    fi

fi

sys_v_semaphores_available=no
windows_semaphores_available=no
posix_semaphores_available=no

if test "x${ac_cv_have_winsock}" != "xyes" -a "x${ac_cv_have_winsock2}" != "xyes" ; then

AC_MSG_NOTICE([checking System V semaphores])

AC_CHECK_HEADER([sem.h],
	[AC_DEFINE(HAVE_SEM_H,1,[Define to 1 if you have <sem.h>.])])

AC_CHECK_HEADER([sys/sem.h],
	[AC_DEFINE(HAVE_SYS_SEM_H,1,[Define to 1 if you have <sys/sem.h>.])])

AC_CHECK_FUNC(semget,
[AC_CHECK_FUNC(semop,
[AC_CHECK_FUNC(semctl,
[
	sys_v_semaphores_available=yes
	AC_DEFINE(SYS_V_SEMAPHORES_AVAILABLE,1,[Can we use SystemV semaphores?])
])
#semctl
])
#semop
])
#semget

AC_CHECK_FUNC(semtimedop,AC_DEFINE(HAVE_SEMTIMEDOP,1,[Define to 1 if you have semtimedop.])])

AC_MSG_NOTICE([checking POSIX semaphores])

AC_CHECK_HEADER([semaphore.h],
[
AC_DEFINE(HAVE_SEMAPHORE_H,1,[Define to 1 if you have <semaphore.h>])
AC_CHECK_FUNC(sem_open ,
[AC_CHECK_FUNC(sem_wait, 
[AC_CHECK_FUNC(sem_trywait,
[AC_CHECK_FUNC(sem_getvalue,
[AC_CHECK_FUNC(sem_post,
[AC_CHECK_FUNC(sem_close,
	[ AC_MSG_CHECKING([can use POSIX named semaphores])
	AC_TRY_RUN([

/* There are two types of posix semaphores named and unnamed.
  unamed semaphores can either have the pshared flag set or not
 determining whether it can be shared between processes.  Currently (12/27/02),
Linux 
implements only unnamed posix semaphores that are not shared between
processes. This is useless to RCSLIB so on Linux System V semaphores
will be used instead.
*/
#include <semaphore.h>
#include <fcntl.h>
#include <errno.h>

int
main(int argc,const char **argv)
{
	sem_t *s;
	s = sem_open("/sem_open_test.sem",O_CREAT,0777,1);
	if(-1 == ((int)(s)) || 0 == s )
	{
	  if(errno == ENOSYS)
	    {
	      exit(-1);
	    }
	}
	else
	  {
	    sem_close(s);
	  }
	exit(0);
}
],
	[posix_semaphores_available=yes
	AC_DEFINE(POSIX_SEMAPHORES_AVAILABLE,1,[Can we use POSIX semaphores?])
	AC_MSG_RESULT(yes)],
	[AC_MSG_RESULT(no)],
	[posix_semaphores_available=yes
	AC_DEFINE(POSIX_SEMAPHORES_AVAILABLE,1,[Can we use POSIX semaphores?])
	AC_MSG_RESULT([not tested])]
	)
#AC_TRY_RUN
])
#AC_CHECK_FUNC sem_close
])
#AC_CHECK_FUNC sem_post
])
#AC_CHECK_FUNC sem_getvalue
])
#AC_CHECK_FUNC sem_trywait
])
#AC_CHECK_FUNC sem_wait
])
#AC_CHECK_FUNC sem_open
])

fi
# if test "x${ac_cv_have_winsock}" != "xyes" -a "x${ac_cv_have_winsock2}" != "xyes" 

if test x$posix_semaphores_available = xyes -a  x$sys_v_semaphores_available = xyes ;  then

   AC_MSG_NOTICE([Both System V and Posix Semaphores are available])

   if test x$with_posix_ipc = xyes -o x$with_sys_v_ipc = xno ; then
      AC_MSG_NOTICE([Using POSIX Semaphores])
      AC_DEFINE(POSIX_SEMAPHORES,1,[Should we use POSIX Semaphores])
      CONFIGURED_PROGRAMS="${CONFIGURED_PROGRAMS} "'posix_sem_unlink$(EXEEXT)'
   else
      AC_MSG_NOTICE([Using System V Semaphores])
      AC_DEFINE(SYS_V_SEMAPHORES,1,[Should we use System V Semaphores])	
   fi

else

   if test x$posix_semaphores_available = xyes ; then
       AC_MSG_NOTICE([Using POSIX Semaphores])
       AC_DEFINE(POSIX_SEMAPHORES,1,[Should we use POSIX Semaphores])
	CONFIGURED_PROGRAMS="${CONFIGURED_PROGRAMS} "'posix_sem_unlink$(EXEEXT)'
   else
       if test x$sys_v_semaphores_available = xyes ; then
	  AC_MSG_NOTICE([Using System V Semaphores])
	  AC_DEFINE(SYS_V_SEMAPHORES,1,[Should we use POSIX Semaphores])
       else
	  if test "x${ac_cv_have_winsock}" != "xyes" -a "x${ac_cv_have_winsock2}" != "xyes" ; then
	     AC_MSG_NOTICE([Neither POSIX nor System V semaphores seem to be useable]);
	  fi

	  if test "x${windows_shared_memory_available}" = "xyes" ; then
	     	   AC_MSG_NOTICE([Checking for windows semaphores/mutexes.])
		   windows_check_failed=false; 
		   if test "x${windows_check_failed}" != "xtrue" ; then
		      AC_TRY_LINK([
			 #include <windows.h>
			 #include <winbase.h>
			 ],
			 [
				HANDLE handle;
				HANDLE handle2;
				SECURITY_ATTRIBUTES sa;
				SECURITY_DESCRIPTOR sd;

				InitializeSecurityDescriptor (&sd, SECURITY_DESCRIPTOR_REVISION);
				  sa.nLength = sizeof (SECURITY_ATTRIBUTES);
				  sa.lpSecurityDescriptor = &sd;
				  sa.bInheritHandle = TRUE;

				handle=CreateMutex (&sa, FALSE,"config_test_mutex");

				WaitForSingleObject (handle,1000);
				ReleaseMutex (handle);

				handle2=OpenMutex (MUTEX_ALL_ACCESS, TRUE,"config_test_mutex");

				CloseHandle(handle);
				CloseHandle(handle2);
			 ],
			 [
			 # AC_DEFINE(HAVE_INITIALIZESECURITYDESCRIPTOR,[Define if you have the InitializeSecurityDescriptor function.])
			 AC_DEFINE(HAVE_CREATEMUTEX,1,[Define to 1 if you have the CreateMutex function.])
			 AC_DEFINE(HAVE_WAITFORSINGLEOBJECT,1,[Define to 1 if you have the WaitForSingleObject function.])
			 AC_DEFINE(HAVE_RELEASEMUTEX,1,[Define to 1 if you have the ReleaseMutex function.])
			 AC_DEFINE(HAVE_OPENMUTEX,1,[Define to 1 if you have the OpenMutex function.])
			 ],[windows_check_failed=true])
		   fi
		   if test "x${windows_check_failed}" != "xtrue" ; then
		      AC_MSG_NOTICE([Using windows semaphores.])
		      windows_semaphores_available=yes;
		   else
		      AC_MSG_ERROR([NO Semaphore/mutex seems go be available.Consider --disable-shmem])
		   fi
          else
		AC_MSG_ERROR([NO Semaphore/mutex seems go be available. Consider --disable-shmem])
	  fi
       fi
   fi
fi

AC_CHECK_TYPE([union semun],
		     [AC_DEFINE(HAVE_UNION_SEMUN,1,[Is union semun defined])],
		     [],
		     [
#if HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#if HAVE_SYS_IPC_H
#include <sys/ipc.h>
#endif
#if HAVE_SYS_SEM_H
#include <sys/sem.h>
#endif
static union semun semun_var;
])

fi
# end of if shmem enabled.

if test "x${enable_shmem}" = "xyes" ; then
   if test "x${windows_semaphores_available}" = "xyes" ; then
         RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/os_intf/win32_sem.lo"
   else
         RCSLIB_EXTRA_OBJS="${RCSLIB_EXTRA_OBJS} src/os_intf/unix_sem.lo"
   fi
   ## AC_MSG_NOTICE([RCSLIB_EXTRA_OBJS : ${RCSLIB_EXTRA_OBJS}])
fi

if test "x${enable_oe_instrf}" = "xyes" ; then

   AC_MSG_NOTICE([LIBS=${LIBS}])
   AC_MSG_NOTICE([LDFLAGS=${LDFLAGS}])
   AC_MSG_NOTICE([CPPFLAGS=${CPPFLAGS}])

   AC_CHECK_HEADER([oe.h],
	[
	AC_DEFINE(HAVE_OE_H,1,[if we can include oe.h])
	],
	[
	AC_MSG_ERROR([Can not include oe.h, either add a -I flag to the CPPFLAGS environment variable specifying the directory with OE C header files or reconfigure with --disable-oe_intrf])
	])
   AC_CHECK_HEADERS([toe_services.h])
			  
   AC_MSG_NOTICE([Checking for OE libraries])
   AC_CHECK_LIB(m,main)
   AC_CHECK_LIB(rt,main)
   AC_CHECK_LIB(nsl,main)
   AC_CHECK_LIB(pthread,main)
   AC_CHECK_LIB(gnat,main)
   AC_CHECK_LIB(gnarl,main)
   AC_CHECK_LIB(utilities_Ada95,main)
   AC_CHECK_LIB(gccprefix,main)
   AC_CHECK_LIB(gmem,main)
   AC_CHECK_LIB(oe_2_0_Ada95,main)
   AC_CHECK_LIB(oe_2_0_c,main,
	[
		LIBS="-loe_2_0_c ${LIBS}";
		export LIBS;	   
	],
	[
		AC_MSG_ERROR([Can not find library liboe_2_0_c, either add a -L flag to LDFLAGS specifying the directory it should be found in or reconfigure to disable the OE interface.(without  --enable-oe_intrf)])
	])


   AC_MSG_NOTICE([LIBS=${LIBS}])
   AC_MSG_NOTICE([LDFLAGS=${LDFLAGS}])

   AC_MSG_NOTICE([Searching for b~linkhelp.o in directories referenced in LDFLAGS=${LDFLAGS}])

( rm /tmp/rcslib_blink*.sh || echo ) >/dev/null 2>/dev/null

cat >/tmp/rcslib_blink$$.sh <<EOF
#! /bin/sh
for i in ${LDFLAGS} end_of_link_flags ; do
      if test "x\${i}" != "xend_of_link_flags" ; then
	       dir=\`echo \${i} | ${AWK} -- '/^-L.*/ {printf("%s",substr(\$1,3));}'\`
	       if test "x\${dir}" != "x" -a -f \${dir}/b~linkhelp.o ; then 
		     blinkhelp_o=\${dir}/b~linkhelp.o
		     export blinkhelp_o
		     mkdir -p ${PWD}/.configure_obj_links
		     (cd ${PWD}/.configure_obj_links; rm ${PWD}/.configure_obj_links/b_tilde_linkhelp.o; ln -s \${blinkhelp_o} b_tilde_linkhelp.o )
		     LIBS="${PWD}/.configure_obj_links/b_tilde_linkhelp.o ${LIBS}"
		     export LIBS
	       fi
      fi
done

EOF

chmod a+x /tmp/rcslib_blink$$.sh
. /tmp/rcslib_blink$$.sh 2>/tmp/rcslib_blink$$.2.log >/tmp/rcslib_blink$$.1.log

   AC_MSG_NOTICE([found ${blinkhelp_o}])
   AC_MSG_NOTICE([check /tmp/rcslib_blink$$.sh /tmp/rcslib_blink$$.2.log /tmp/rcslib_blink$$.1.log if b~linkhelp.o location is wrong])

   AC_CHECK_FUNC(adainit,
		[
		AC_DEFINE(HAVE_ADAINIT,1,[if we should use the adainit funtion])
		])

   AC_CHECK_FUNC(adafinal,
		[
		AC_DEFINE(HAVE_ADAFINAL,1,[if we should use the adafinal funtion])
		])

   AC_MSG_NOTICE([LIBS=${LIBS}])
   AC_MSG_NOTICE([LDFLAGS=${LDFLAGS}])

   AC_LANG_PUSH(C++)
   AC_CHECK_HEADER([oe.hh],
	[
	AC_DEFINE(HAVE_OE_HH,1,[if we can include oe.hh])
	],
	[
	AC_MSG_ERROR([Can not include oe.hh, either add a -I flag to the CPPFLAGS environment variable specifying the directory with OE C++ header files or reconfigure without --enable-oe_intrf])
	])
   AC_CHECK_HEADERS([toe_services.hh])
   AC_CHECK_LIB(oe_2_0_cpp,main,
	[
		LIBS="-loe_2_0_cpp ${LIBS}";
		export LIBS;	   
	],
	[
		AC_MSG_ERROR([Can not find library liboe_2_0_cpp, either add a -L flag to LDFLAGS environment variable specifying the directory it should be found in or reconfigure to disable the OE interface. without --enable-oe_intrf. You may also want to check that the version of your C++ compiler ${CXX} is compatible with the version used to build liboe_2_0_cpp or search config.log for even more information.])
	])
	
   AC_LANG_POP
   AC_MSG_NOTICE([LIBS=${LIBS}])
   AC_MSG_NOTICE([LDFLAGS=${LDFLAGS}])
   AC_MSG_NOTICE([CPPFLAGS=${CPPFLAGS}])

fi
# end of  -- if test "x${enable_oe_intrf}" = "xyes" ; then

AC_CXX_NAMESPACES
AC_CXX_HAVE_STD

build_platform=autoconf-${build}     
AC_MSG_NOTICE([Setting build platform to ${build_platform}])

AC_DEFINE_UNQUOTED(BUILD_PLATFORM_FOR_TEST,[${build_platform}],[What platform is this built for?])
AC_DEFINE_UNQUOTED(BUILD_PLATFORM,["${build_platform}"],[What platform is this built for? (string for printing)])

AC_MSG_NOTICE([ CONFIGURED_PROGRAMS = ${CONFIGURED_PROGRAMS}])
AC_MSG_NOTICE([ EXTRA_CHECK_PROGRAMS = ${EXTRA_CHECK_PROGRAMS}])
AC_MSG_NOTICE([ CONFIGURED_TESTS = ${CONFIGURED_TESTS}])
#AC_MSG_NOTICE([ CONFIGURED_STATIC_LIBRARIES = ${CONFIGURED_STATIC_LIBRARIES}])
AC_MSG_NOTICE([ CONFIGURED_SUBDIRS = ${CONFIGURED_SUBDIRS}])
AC_MSG_NOTICE([ RCSLIB_EXTRA_OBJS = ${RCSLIB_EXTRA_OBJS} ])
AC_MSG_NOTICE([ RCSRTAILIB_EXTRA_OBJS = ${RCSRTAILIB_EXTRA_OBJS} ])
AC_MSG_NOTICE([ CPPFLAGS = ${CPPFLAGS} ])
AC_MSG_NOTICE([ CFLAGS = ${CFLAGS} ])
AC_MSG_NOTICE([ CXXFLAGS = ${CXXFLAGS} ])
AC_MSG_NOTICE([ LDFLAGS = ${LDFLAGS} ])
AC_MSG_NOTICE([ LIBS = ${LIBS} ])
AC_MSG_NOTICE([ CC = ${CC} ])
AC_MSG_NOTICE([ CXX = ${CXX} ])

#AM_CONDITIONAL(HAVE_RTL,[test "x${ac_cv_have_rtlinux}" = "xYES" -o "x${ac_cv_have_rtlinux}" = "xyes" -o "x${HAVE_RTL}" = "xYES"])
#AM_CONDITIONAL(HAVE_RTAI,[test "x${ac_cv_have_rtai}" = "xYES" -o "x${ac_cv_have_rtai}" = "xyes" -o "x${HAVE_RTAI}" = "xYES"])

LTLIBOBJS="${RCSLIB_EXTRA_OBJS}"

AC_MSG_NOTICE([ POSEMATH_CFLAGS = ${POSEMATH_CFLAGS} ])
AC_MSG_NOTICE([ POSEMATH_CXXFLAGS = ${POSEMATH_CXXFLAGS} ])
AC_MSG_NOTICE([ TEST_CFLAGS = ${TEST_CFLAGS} ])
AC_MSG_NOTICE([ TEST_CXXFLAGS = ${TEST_CXXFLAGS} ])

AM_CONDITIONAL(TEST_FLAGS_SET,[test "x${TEST_CFLAGS}" != "x" -o "x${TEST_CXXFLAGS}" != "x"])

AM_CONDITIONAL(POSEMATH_FLAGS_SET,[test "x${POSEMATH_CFLAGS}" != "x" -o "x${POSEMATH_CXXFLAGS}" != "x"])


AC_SUBST(CONFIGURED_PROGRAMS)
AC_SUBST(EXTRA_CHECK_PROGRAMS)
AC_SUBST(CONFIGURED_TESTS)
AC_SUBST(CONFIGURED_SUBDIRS)
#AC_SUBST(CONFIGURED_STATIC_LIBRARIES)
AC_SUBST(CONFIGURED_EXTRA_INCLUDES)
AC_SUBST(LTLIBOBJS)
AC_SUBST(RCSLIB_EXTRA_OBJS)
AC_SUBST(RCSRTAILIB_EXTRA_OBJS)
AC_SUBST(RCSRTLLIB_EXTRA_OBJS)
AC_SUBST(POSEMATH_CFLAGS)
AC_SUBST(POSEMATH_CXXFLAGS)
AC_SUBST(TEST_CFLAGS)
AC_SUBST(TEST_CXXFLAGS)

AC_CONFIG_FILES([Makefile])
AC_OUTPUT(rcslib.pc)

