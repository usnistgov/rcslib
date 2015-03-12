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

/*************************************************************************
* File: nmlmsg.cc                                                        *
* Authors: Fred Proctor, Will Shackleford                                *
* Purpose: C++ file for the  Neutral Manufacturing Language (NML).       *
*          Includes:                                                     *
*                    1. Member functions for class NMLmsg.               *
*************************************************************************/


/* Include Files */
#include "rcs_defs.hh"		/* EXTERN_C_STD_HEADERS */

#ifdef EXTERN_C_STD_HEADERS
extern "C"
{
#endif
#include <string.h>		// memset

#ifdef EXTERN_C_STD_HEADERS
}
#endif


#include "cms.hh"
#include "nmlmsg.hh"
#include "rcs_prnt.hh"

/* nmlmsg Functions. */

int
  NMLmsg::automatically_clear =
  1;

/* Constructor */
NMLmsg::NMLmsg (NMLTYPE t, long s):
  type(t),size(s)
{
  //  rcs_print("size=%ld\n",size);
  type = t;
  size = s;
  if (automatically_clear)
    {
      clear ();
    }
  if (size < ((long) sizeof (NMLmsg)))
    {
      rcs_print_error ("NMLmsg: size(=%ld) must be atleast %lu\n", size,
		       (unsigned long) sizeof (NMLmsg));
      size = sizeof (NMLmsg);
    }
  if (type <= 0)
    {
      rcs_print_error ("NMLmsg: type(=%ld) should be greater than zero.\n",
		       (long)type);
    }
}

#ifdef NML_OLD_COMPAT
NMLmsg::NMLmsg (NMLTYPE t, size_t s):
  type(t),size(s)
{
  type = t;
  size = s;
  if (automatically_clear)
    {
      clear ();
    }
  if (size < ((long) sizeof (NMLmsg)))
    {
      rcs_print_error ("NMLmsg: size(=%ld) must be atleast %lu\n", size,
		       (unsigned long) sizeof (NMLmsg));
      size = sizeof (NMLmsg);
    }
  if (type <= 0)
    {
      rcs_print_error ("NMLmsg: type(=%ld) should be greater than zero.\n",
		       (long) type);
    }
}
#endif


NMLmsg::NMLmsg (NMLTYPE t, long s, int noclear):
  type(t),size(s)
{
  if (automatically_clear && !noclear)
    {
      clear ();
    }
  type = t;
  size = s;
  if (size < ((long) sizeof (NMLmsg)))
    {
      rcs_print_error ("NMLmsg: size(=%ld) must be atleast %lu\n", size,
		       (unsigned long) sizeof (NMLmsg));
      size = sizeof (NMLmsg);
    }
  if (type <= 0)
    {
      rcs_print_error ("NMLmsg: type(=%ld) should be greater than zero.\n",
		       (long) type);
    }
}

void
NMLmsg::clear (void)
{
  long temp_size;
  NMLTYPE temp_type;
  temp_size = size;
  temp_type = type;
  if (size < ((long) sizeof (NMLmsg)))
    {
      rcs_print_error ("NMLmsg: size(=%ld) must be atleast %lu\n", size,
		       (unsigned long) sizeof (NMLmsg));
      size = sizeof (NMLmsg);
    }
  else
    {
      memset ((void *) this, 0, size);
      size = temp_size;
      type = temp_type;
    }
}

void
NMLmsg::mask_all (void)
{
  long temp_size;
  NMLTYPE temp_type;
  temp_size = size;
  temp_type = type;
  if (size < ((long) sizeof (NMLmsg)))
    {
      rcs_print_error ("NMLmsg: size(=%ld) must be atleast %lu\n", size,
		       (unsigned long) sizeof (NMLmsg));
      size = sizeof (NMLmsg);
    }
  else
    {
      memset ((void *) this, 0xFF, size);
      size = temp_size;
      type = temp_type;
    }
}


/* Error message stub for base class. */
 /* update should only be called with derived classes. */
void
NMLmsg::update (CMS * cms)
{
  rcs_print_error ("update called for NMLmsg base class.");
  rcs_print_error ("(This is an error.)\n");
  cms->status = CMS_MISC_ERROR;
}
