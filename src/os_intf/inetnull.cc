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


/**********************************************************************
* FILE: inetnull.cc
* Purpose: This file provides the same set of functions as inetfile.cc
* but with local file reading capabilities only.
***********************************************************************/
#ifdef HAVE_CONFIG_H
#include "rcs_config.h"
#include "rcs_config_include.h"
#else
#include "rcs_defs.hh"
#ifdef NO_STDIO
#error This file can not be compiled without stdio, remove it from the project if stdio is not available.
#endif
#include <stdio.h>
#endif

#include "inetfile.hh"

class INET_FILE
{
public:
  FILE * fp;
};

#if 0
static const char *inet_file_agent_name=0;
static const char *inet_file_agent_version=0;
static int inet_file_debug=0;
#endif

/*ARGSUSED*/
int
inet_file_init (
		__unused_parameter__ const char *, 
		__unused_parameter__ char *, 
		__unused_parameter__ int)
{		/*ARGSUSED*/
#if 0
  inet_file_agent_name=agent_name;
  inet_file_agent_version=agent_version;
  inet_file_debug=debug;
#endif
  return 0;
}


INET_FILE *
inet_file_open (const char *url, const char *type)
{

  FILE *fp = NULL;
  INET_FILE *inet_file = NULL;

  fp = fopen (url, type);
  if (NULL == fp)
    {
      return NULL;
    }
  inet_file = new INET_FILE;
  if (NULL != inet_file)
    {
      inet_file->fp = fp;
    }
  return inet_file;
}



char *
inet_file_gets (char *str, int maxlen, INET_FILE * inet_file)
{
  if (NULL == inet_file)
    {
      return NULL;
    }
  return fgets (str, maxlen, inet_file->fp);
}


int
inet_file_close (INET_FILE * inet_file)
{
  if (NULL != inet_file)
    {
      fclose (inet_file->fp);
      delete inet_file;
    }
  return 0;
}

int
inet_file_eof (INET_FILE * inet_file)
{
  if (NULL == inet_file)
    {
      return 1;
    }
  return feof (inet_file->fp);
}


int
inet_file_exit ()
{
  return 0;
}


int
inet_file_rewind (INET_FILE * ifp)
{
  if (NULL == ifp)
    {
      return -1;
    }
  if (ifp->fp != NULL)
    {
      rewind (ifp->fp);
      return 0;
    }
  return -1;
}
