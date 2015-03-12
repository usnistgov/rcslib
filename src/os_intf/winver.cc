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


/* This line prevents findsource from adding this file to the Makefile */
#define NOT_A_DEFAULT_RCSLIB_SOURCE 1

#include <windows.h>
#include <stdio.h>


static char winverbuf[256];
static int winver_done = 0;

char *
winver ()
{
  char *os_string = "Win32";
  if (winver_done)
    {
      return winverbuf;
    }

  OSVERSIONINFOEX osvi;
  BOOL bOsVersionInfoEx;

  // Try calling GetVersionEx using the OSVERSIONINFOEX structure,
  // which is supported on Windows NT versions 5.0 and later.
  // If that fails, try using the OSVERSIONINFO structure,
  // which is supported on earlier versions of Windows and Windows NT

  ZeroMemory (&osvi, sizeof (OSVERSIONINFOEX));
  osvi.dwOSVersionInfoSize = sizeof (OSVERSIONINFOEX);

  if (!(bOsVersionInfoEx = GetVersionEx ((OSVERSIONINFO *) & osvi)))
    {

      // If OSVERSIONINFOEX doesn't work, try OSVERSIONINFO.

      osvi.dwOSVersionInfoSize = sizeof (OSVERSIONINFO);
      if (!GetVersionEx ((OSVERSIONINFO *) & osvi))
	return (NULL);
    }

  switch (osvi.dwPlatformId)
    {

    case VER_PLATFORM_WIN32_NT:

      os_string = "WinNT";
      break;

    case VER_PLATFORM_WIN32_WINDOWS:

      if ((osvi.dwMajorVersion > 4) ||
	  ((osvi.dwMajorVersion == 4) && (osvi.dwMinorVersion > 0)))
	os_string = "Win98";
      else
	os_string = "Win95";
      break;

    case VER_PLATFORM_WIN32s:

      os_string = "Ms Win32s";
      break;
    }

#if _MSC_VER >= 1400
  sprintf_s (winverbuf,sizeof(winverbuf),
#else
	sprintf(winverbuf,
#endif
	    "%s ver %d.%d (Bld %d)\n",
	    os_string,
	   osvi.dwMajorVersion,
	   osvi.dwMinorVersion, 
		osvi.dwBuildNumber & 0xFFFF);
  winver_done = 1;
  return winverbuf;
};
