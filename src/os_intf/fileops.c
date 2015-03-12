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


/*
  fileops.c

  File operations: is it a file, directory, link, etc.

  Modification history:

  17-Mar-2000  FMP created, to solve problem where fopen() was passed a
  directory, and it succeeded, but we really wanted success only if it
  was a real, useful file.
*/

#if defined (LINUX) || defined (SUN)

#include "fileops.h"		/* these decls */
#include <sys/types.h>
#include <sys/stat.h>

int
rcs_fileop_isfile (const char *path)
{
  struct stat buf;

  /* use stat here, so link to regular file will be considered a file */
  if (stat (path, &buf) < 0)
    {
      return 0;
    }

  if (S_ISREG (buf.st_mode))
    {
      return 1;
    }

  return 0;
}

int
rcs_fileop_isdir (const char *path)
{
  struct stat buf;

  /* use stat here, so link to directory will be considered a directory */
  if (stat (path, &buf) < 0)
    {
      return 0;
    }

  if (S_ISDIR (buf.st_mode))
    {
      return 1;
    }

  return 0;
}

int
rcs_fileop_islink (const char *path)
{
  struct stat buf;

  /* use lstat here, so link to anything will be considered a link */
  if (lstat (path, &buf) < 0)
    {
      return 0;
    }

  if (S_ISLNK (buf.st_mode))
    {
      return 1;
    }

  return 0;
}

#endif /* LINUX, SUN */


#if defined (MS_WINDOWS_API)

#include "fileops.h"		/* these decls */

/* FIXME-- for this plat, 'path' is always considered a file */

int
rcs_fileop_isfile (const char *path)
{
  return 1;
}

int
rcs_fileop_isdir (const char *path)
{
  return 0;
}

int
rcs_fileop_islink (const char *path)
{
  return 0;
}

/* MS_WINDOWS_API */
#endif

#ifndef FILEOPS_H
#include "rcs_empty_source"
#endif


