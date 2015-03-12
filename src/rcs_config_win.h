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

#ifndef RCS_CONFIG_WIN_H
#define RCS_CONFIG_WIN_H

#ifdef RCS_HH
#error This file should never be included in a source that also includes rcs.hh.
#endif

# ifndef WINSOCK_BEFORE_WINDOWS
#  if HAVE_WINDOWS_H
#   include <windows.h>
#  endif
# endif

# if HAVE_WINSOCK2_H
#   include <winsock2.h>
# else
#  if HAVE_WINSOCK_H
#   include <winsock.h>
#  endif
# endif

# ifdef WINSOCK_BEFORE_WINDOWS
# if HAVE_WINDOWS_H
#  include <windows.h>
# endif
# endif

# if HAVE_WINBASE_H
#  include <winbase.h>
# endif

# if HAVE_WINVER_H
#  include <winver.h>
# endif

# if HAVE_PROCESS_H
#  ifndef MULTITHREADED
#   define MULTITHREADED 1
#  endif
#  include <process.h>
# endif

# if HAVE_DOS_H
#  include <dos.h>
# endif

# if HAVE_IO_H
#  include <io.h>
# endif

#if !defined(HAVE_SNPRINTF) && defined(HAVE__SNPRINTF) && !defined(snprintf)
#define snprintf _snprintf
#define HAVE_SNPRINTF 1
#undef HAVE__SNPRINTF
#endif


/* RCS_CONFIG_WIN_H */
#endif
