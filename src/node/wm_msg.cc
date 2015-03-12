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
#endif

#if defined(ENABLE_RCS_NMLMOD)

/* This line prevents findsource from adding this file to the Makefile */
#define NOT_A_DEFAULT_RCSLIB_SOURCE 1

#if HAVE_CONFIG_H
#include "rcs_config_include.h"
#else
#include "rcs_defs.hh"
#endif

#include "nml.hh"
#include "nmlmsg.hh"
#include "cms.hh"
#include "linklist.hh"

#include "wm_msg.hh"

RCS_WM_MSG::RCS_WM_MSG (NMLTYPE t, long sz):
NMLmsg (t, sz)
{
// just avoiding an inline function.
}

int
RCS_WM_MSG_format (NMLTYPE t, void  * buf, CMS  * cms)
{
  switch (t)
    {
    case RCS_GENERIC_WM_TYPE:
      ((RCS_GENERIC_WM *) buf)->update (cms);
      return (1);

    default:
      return (0);
    }
  return (0);
}

RCS_GENERIC_WM::RCS_GENERIC_WM ():
RCS_WM_MSG (RCS_GENERIC_WM_TYPE, sizeof (RCS_GENERIC_WM))
{
// Just avoiding an inline function.
}

void
RCS_GENERIC_WM::update (CMS * cms)
{
// Just avoiding an inline function.
}

RCS_WM_CHANNEL::RCS_WM_CHANNEL (NML_FORMAT_PTR f_ptr, char *name,
				char *process, char *file, int set_to_server):
NML (name, process, file, set_to_server)
{
  format_chain = new RCS_LINKED_LIST;
  prefix_format_chain (f_ptr);
  prefix_format_chain (RCS_WM_MSG_format);
  register_with_server ();
}

RCS_WM_CHANNEL::~RCS_WM_CHANNEL ()
{
  // Something funny happens to vxgdb without this being explicitly defined.
}

// defined(ENABLE_RCS_NMLMOD)
#else
#include "rcs_empty_source"
#endif
