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

#if defined(ENABLE_RCS_PHANTOM) || defined(ENABLE_RCS_PHANTOMMEM)

#ifdef HAVE_CONFIG_H
#include "rcs_config_include.h"
#else
#include "rcs_defs.hh"
#endif

#include "cms.hh"		/* enum CMS_STATUS */
#include "phantom.hh"		/* class PHANTOMMEM */


PHANTOMMEM::PHANTOMMEM (char *bufline, char *procline):
CMS (bufline, procline)
{
}

PHANTOMMEM::~PHANTOMMEM ()
{
}


CMS_STATUS PHANTOMMEM::main_access (
				    __unused_parameter__ void *_local)
{
  switch (internal_access_type)
    {
    case CMS_READ_ACCESS:
    case CMS_PEEK_ACCESS:
      return (status = CMS_READ_OLD);
    case CMS_WRITE_ACCESS:
    case CMS_WRITE_IF_READ_ACCESS:
      return (status = CMS_WRITE_OK);
    case CMS_CHECK_IF_READ_ACCESS:
    case CMS_CLEAR_ACCESS:
    case CMS_ZERO_ACCESS:
      header.was_read = 0;
      return (status);
    default:
      break;
    }
  return (status);
}

//  defined(ENABLE_RCS_PHANTOM)
#else
#include "rcs_empty_source"
#endif


