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


/*
   inifile.cc

   INI file reader

   Modification history:

   23-Apr-2004 WPS modified so that inifile.h does not include stdio.h and
   therefore is built not to need the FILE type defined.
   18-Dec-1997  FMP split out C code into _inifile.c
   7-Nov-1997  FMP fixed bug in afterequal so that it terminates at a 0
   25-Jul-1996  FMP added find_section() and ::section()
   11-Jul-1996  Fred Proctor made sure ini_find() returned NULL if a
   section were provided and no match was detected when the section
   was left; fixed bug which required the last line to have a newline
   */


#if HAVE_CONFIG_H
#include "rcs_config.h"
#include "rcs_config_include.h"
#else
#include "rcs_defs.hh"

#ifdef EXTERN_C_STD_HEADERS
extern "C"
{
#endif

#ifndef NO_STDIO
#include <stdio.h>		/* FILE *, fopen(), fclose(), NULL */
#endif
#include <errno.h>
#include <string.h>		/* strstr() */
#include <ctype.h>		/* isspace() */
#include <stdlib.h>		/* exit() */
#include <stdarg.h>		/* va_list */

#ifdef EXTERN_C_STD_HEADERS
}
#endif

// HAVE_CONFIG_H
#endif

#include "inifile.h"
#include "rcs_prnt.hh"

class INIFILE_PRIVATE_DATA
{
  friend class INIFILE;

private:
#ifndef NO_STDIO
    FILE * fp;
#endif

#ifdef INIFILE_USE_INET_FILES
  INET_FILE *ifp;
#endif

  INIFILE_PRIVATE_DATA():
#ifndef NO_STDIO
    fp(0)
#endif
#ifdef INIFILE_USE_INET_FILES
#ifndef NO_STDIO
    ,
#endif
    ifp(0)
#endif
  {
  };


  INIFILE_PRIVATE_DATA(const INIFILE_PRIVATE_DATA &_ipd):
#ifndef NO_STDIO
    fp(_ipd.fp)
#endif
#ifdef INIFILE_USE_INET_FILES
#ifndef NO_STDIO
    ,
#endif
    ifp(_ipd.ifp)
#endif
  {
    rcs_print_error("INIFILE_PRIVATE_DATA copy constructor should never be called.\n");
  };

  INIFILE_PRIVATE_DATA &operator=(const INIFILE_PRIVATE_DATA &_ipd)
  {
#ifndef NO_STDIO
    fp = _ipd.fp;
#endif
#ifdef INIFILE_USE_INET_FILES
    ifp = _ipd.ifp;
#endif
    rcs_print_error("INIFILE_PRIVATE_DATA::operator= should never be called.\n");
    return(*this);
  };

};

INIFILE::INIFILE ():
  ipd(0)
{
  ipd = new INIFILE_PRIVATE_DATA();

#ifndef NO_STDIO
  ipd->fp = NULL;
#endif

#ifdef INIFILE_USE_INET_FILES
  ipd->ifp = NULL;
#endif
}

INIFILE::INIFILE (const char *path):
  ipd(0)
{
  if(!ipd)
    {
      ipd = new INIFILE_PRIVATE_DATA();
    }

#ifndef INIFILE_USE_INET_FILES
  if (NULL == (ipd->fp = fopen (path, "r")))
    {
      fprintf (stderr, "can't open %s -- %s\n", path, strerror(errno));
    }
#else
  if (NULL == (ipd->ifp = inet_file_open (path, "r")))
    {
      rcs_print_error ("can't open %s -- %s\n", path,strerror(errno));
    }
#endif
}

INIFILE::~INIFILE ()
{
  if(ipd)
    {
#ifdef INIFILE_USE_INET_FILES
      if (NULL != ipd->ifp)
	{
	  inet_file_close (ipd->ifp);
	}
#else
#ifndef NO_STDIO
      if (NULL != ipd->fp)
	{
	  fclose (ipd->fp);
	}
#endif
#endif
      delete ipd;
      ipd=0;
    }
}

int
INIFILE::open (const char *path)
{
  if(!ipd)
    {
      ipd = new INIFILE_PRIVATE_DATA();
    }

#ifndef INIFILE_USE_INET_FILES
  if (NULL == (ipd->fp = fopen (path, "r")))
    {
      fprintf (stderr, "can't open %s -- %s\n", path,strerror(errno));
      return -1;
    }
#else
  if (NULL == (ipd->ifp = inet_file_open (path, "r")))
    {
      rcs_print_error ("can't open %s -- %s\n", path, strerror(errno));
      return -1;
    }
#endif
  return 0;
}

int
INIFILE::close ()
{
  int retval = 0;
  if(ipd)
    {
#ifndef NO_STDIO
      if (ipd->fp != NULL)
	{
	  retval = fclose (ipd->fp);
	  ipd->fp = NULL;
	}
#endif
#ifdef INIFILE_USE_INET_FILES
      if (ipd->ifp != NULL)
	{
	  retval = inet_file_close (ipd->ifp);
	  ipd->ifp = NULL;
	}
#endif
    }
  return retval;
}

int INIFILE::isSection(const char * section)
{
  return findSection(ipd->fp, section);
}

const char *
INIFILE::find (const char *tag, const char *section_to_find)
{
  if(!ipd)
    {
      return 0;
    }
#ifndef INIFILE_USE_INET_FILES
  return iniFind (ipd->fp, tag, section_to_find);
#else
  return iniFind (ipd->ifp, tag, section_to_find);
#endif
}

/*
   given 'section' and array of strings, fills strings with what was
   found in the section, one line per string. Comments and blank lines
   are omitted. 'array' is assumed to be allocated, of 'max' entries
   of size INIFILE_MAX_LINELEN.

   Returns number of entries found; 0 if section is there but no entries
   in it, or -1 if section is not there.
*/

int
INIFILE::section (const char *section_to_load, INIFILE_ENTRY array[], int max)
{
  if(!ipd)
    {
      return 0;
    }

#ifndef INIFILE_USE_INET_FILES
  return iniSection (ipd->fp, section_to_load, array, max);
#else
  return iniSection (ipd->ifp, section_to_load, array, max);
#endif
}

int
INIFILE::valid ()
{
  if(!ipd)
    {
      return 0;
    }

#ifndef INIFILE_USE_INET_FILES
  if (NULL == ipd->fp)
    return 0;
#else
  if (NULL == ipd->ifp)
    return 0;
#endif
  return 1;
}

INIFILE::INIFILE(const INIFILE &):
  ipd(0)
{
}
  
INIFILE &INIFILE::operator=(const INIFILE &)
{
  return(*this);
}
