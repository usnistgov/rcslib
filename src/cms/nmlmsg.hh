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
* File: nmlmsg.hh                                                        *
* Authors: Fred Proctor, Will Shackleford                                *
* Purpose: C++ Header file for the Neutral Manufacturing Language (NML). *
*          Includes:                                                     *
*                    1. NMLmsg Class.                                    *
*                    2. NMLTYPE typedef.                                 *
*************************************************************************/

#ifndef NMLMSG_HH
#define NMLMSG_HH

/* Include Files */
#ifdef EXTERN_C_STD_HEADERS
extern "C"
{
#endif
#ifdef NML_OLD_COMPAT
#include <stddef.h>		/* size_t */
#endif

#ifdef EXTERN_C_STD_HEADERS
};
#endif

/* Definitions from other Header files. */

#include "nmlmacros.h"

class CMS;		/* Use only partial definition to avoid */
				/* depending on cms.hh */
class NML;

/* Class NMLmsg */
/* Base class for all types that can be written to NML. */
/* The constructor is protected so that to users cannot send messages */
/*  without deriving thier own classes from NMLmsg.  */
/* Derived classes should pass the type and size to the NMLmsg constructor. */
/*  and define their own update function. */
class NMLmsg
{
protected:
  NMLmsg (NMLTYPE t, long s);
#ifdef NML_OLD_COMPAT
    NMLmsg (NMLTYPE t, size_t s);
#endif


  // This second constructor never clears the message regardless of what is
  // in nmlmsg. The value of noclear is irrelevent but adding it changes
  // which constructor is called.
    NMLmsg (NMLTYPE t, long s, int noclear);

public:
  void clear (void);
  void mask_all (void);

  static int automatically_clear;	/* controls whether NMLmsgs are set to zero
					   in the constructor. */
  NMLTYPE type;			/* Each derived type should have a unique id */
  long size;			/* The size is used so that the entire */
  /* buffer is not copied unneccesarily. */

  void update (CMS *);

  friend class NML;
};

#ifndef ARRAY_LEN
#define ARRAY_LEN(X) (sizeof(X)/sizeof(X[0]))
#endif

#ifndef ARRAY_LENI
#define ARRAY_LENI(X) ((int) ARRAY_LEN(X))
#endif

/* !defined(NMLMSG_HH) */
#endif
