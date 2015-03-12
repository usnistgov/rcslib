/* 
The NIST RCS (Real-time Control Systems) 
 library is public domain software, however it is preferred
 that the following disclaimers be attached.

Software Copywrite/Warranty Disclaimer

   This software was developed at the National Institute of Standards and
Technology by employees of the Federal Government in the course of their
official duties. Pursuant to title 17 Section 105 of the United States
Code this software is not subject to copyright protection and is in the
public domain. NIST Real-Time Control System software is an experimental
system. NIST assumes no responsibility whatsoever for its use by other
parties, and makes no guarantees, expressed or implied, about its
quality, reliability, or any other characteristic. We would appreciate
acknowledgement if the software is used. This software can be
redistributed and/or modified freely provided that any derivative works
bear some notice that they are derived from it, and any modified
versions bear some notice that they have been modified.



*/ 

#if HAVE_CONFIG_H
#include "rcs_config.h"
#ifndef PLATNAME
#define PLATNAME BUILD_PLATFORM
#endif
#else
#include <string.h>
#include <stdlib.h>
#endif

#define NO_DBG_MEM
#define RCSVERS_C
#include "rcsvers.hh"


int rcs_version_printed = 0;

const int rcs_major_version_number = RCS_MAJOR_VERSION;
const int rcs_minor_version_number = RCS_MINOR_VERSION;


#if !defined(PLATNAME) 
#if defined(WIN32)
#define PLATNAME "win32m__sc"
#else
#define PLATNAME "noplatname"
#endif
#endif

#ifndef RCS_SVN_VERS
#include "rcs_svn_vers.hh"
#endif

#ifndef RCS_SVN_VERS
#define RCS_SVN_VERS ""
#endif

#ifdef __VERSION__
const char *rcs_version_info_string ="@(#)" "$Info: RCS_LIBRARY_VERSION " RCS_VERSION " Compiled on  " __DATE__ " at " __TIME__  " for the " PLATNAME " platform with compiler version " __VERSION__ " $ " RCS_SVN_VERS " .\n";
#else
const char *rcs_version_info_string ="@(#)" " $Info: RCS_LIBRARY_VERSION " RCS_VERSION " Compiled on  " __DATE__ " at " __TIME__  " for the " PLATNAME " platform. " RCS_SVN_VERS " $ \n";
#endif

#ifndef __unused_parameter__
#ifdef __GNUC__
#if (__GNUC__ >= 3 ) && !defined(MS_WINDOWS_API)
#define __unused_parameter__ __attribute__ ((unused))
#else
#define __unused_parameter__
#endif
#else
#define __unused_parameter__
#endif
#endif

#if !defined(__GNUC__) && !defined(__attribute__)
#define __attribute__(x)
#endif

#ifdef VXWORKS
  void rcs_shm_table_print (void);
  void rcs_sem_table_print (void);
#endif

#ifdef __GNUC__
static char __attribute__((unused)) ident[] =
"$Id: rcsvers.c 430 2006-02-18 21:03:48Z shackle $";
#endif

const char *rcs_version_string;

void  print_rcs_version()
{
  rcs_version_printed = 1;
#if !defined(rtlinux) && !defined(DISABLE_RCS_PRINT)
  rcs_print((char *) rcs_version_info_string);
#endif
}


int rcsinfo(void)
{
#if !defined(rtlinux) && !defined(DISABLE_RCS_PRINT)
  print_rcs_version();

#ifdef ENABLE_RCS_SERVER
  cms_print_servers();
#endif

  print_etime();
#ifdef ENABLE_RCS_DIAG
  nml_print_diag_list();
#endif

#ifdef VXWORKS
  rcs_shm_table_print();
  rcs_sem_table_print();
#endif

  /* #if !defined(rtlinux) && !defined(DISABLE_RCS_PRINT) */
#endif

  return(0);
}







