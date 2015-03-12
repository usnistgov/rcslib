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

#ifndef PHYSMEM_NO_CONFIG_H
#define PHYSMEM_NO_CONFIG_H

#include "rcs_defs.hh"		/* __MSDOS__, RCS_FAR, EXTERN_C_STD_HEADERS  */

#ifdef WIN16
#include <windows.h>		/* LPBYTE, WORD */
typedef LPBYTE LOCAL_ADDRESS_TYPE;
#else
typedef char *LOCAL_ADDRESS_TYPE;
#endif

#if defined(WIN32) && !defined(gnuwin32)
#if defined(WIN32) && !defined(USE_OLD_WINSOCK)
/* Lame problem if windows.h is included before winsock2.h many redefined
 compiler errors result. */
#include <winsock2.h>
#endif
#include <windows.h>		/* HANDLE */
#if 0
#ifdef USE_MAPMAM
#include "mapmem.h"		/* PHYSICAL_MEMORY_INFO */
#endif
#ifdef USE_GPIOCTL
#include "gpioctl.h"		/* GENPORT_WRITE_INPUT */
#endif

/* # if 0 */
#endif

#endif

#if defined(USE_BIT3) && defined(WIN32)
#include "btapi.h"
#endif

/* ADDRESS TYPE MACROS --
* Under Windows Pass one of these as the _address_code for
* PHYSMEM_HANDLE */
#define NT_ISA_MEM_ADDRESS      ((long) 1)
#define NT_ISA_IO_ADDRESS       ((long) 2)

#include "dbg_mem.h"		// DEBUG_MALLOC, DEBUG_FREE

#ifdef EXTERN_C_STD_HEADERS
extern "C"
{
#endif


#include <string.h>		/* memcpy(), memset() */
#ifndef NO_STDIO
#include <stdio.h>		// sprintf()
#endif
#include <stdlib.h>		/* malloc() */

#ifdef  lynxosPC

#include <smem.h>
#include <errno.h>

#endif

#ifdef LINUX_VME
#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/mman.h>
#endif


#ifdef VXWORKS
#include "vxWorks.h"
#include "sysLib.h"		/* sysBusToLocalAdrs() */
#include "vme.h"		/* VME_AM_STD_USR_DATA */
#endif

#ifdef EXTERN_C_STD_HEADERS
}
#endif

#ifdef USE_BIT3
#ifndef USING_BIT3
#define USING_BIT3
#endif
#endif

#ifdef USING_BIT3
#ifndef USE_BIT3
#define USE_BIT3
#endif
#endif

#endif
// PHYSMEM_NO_CONFIG_H
