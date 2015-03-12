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
 File: rcs.hh
 Authors: Fred Proctor, Will Shackleford
 Purpose: C++ Header file for the  RCS Systems Tools.
          Includes:
                1. #includes for all RCS Systems Tools Header Files.
*************************************************************************/

/*************************************************************************
 Notes:
  1. This is the main include file needed by applications using
  the Real-time Control Systems(RCS) Library.
   The library documentation is available on the world wide web at
   http://www.isd.mel.nist.gov/projects/rcslib/


  2. *.hh files are C++ header files. *.h files are C header files.

  3. Borland C++(for DOS or Windows) automatically defines __MSDOS__.
     Micrsoft Visual C++ defines _MSDOS, and rcs_defs.hh defines __MSDOS__ if
     _MSDOS is defined. Certain utilities that are provided by the library
     for most platforms, such as semaphores are not avialable under DOS or
     Windows.

  4. If you have any problems with the RCS library, please do not hesitate
  to call me, Will Shackleford, at (301) 975-4286 or reach me via e-mail at
  shackle@cme.nist.gov.

 Disclaimers:

 Software Disclaimer

 This software was produced by the National Institute of Standards and
 Technology (NIST), an agency of the U.S. government, and by statute is
 not subject to copyright in the United States.  Recipients of this software
 assume all responsibility associated with its operation, modification,
 maintenance, and subsequent redistribution.
 (See NIST Administration Manual 4.09.07 b and Appendix I)


 this paper to specify the experimental procedure adequately.  Such
 identification is not intended to imply recommendation or endorsement by
 the National Institute of Standards and Technology, nor is it intended to
 imply that the materials or equipment identified are necessarily the best
 available for the purpose.

*************************************************************************/

#ifdef RCS_CONFIG_INCLUDE_H
#error This file should never be included in a source that also includes rcs_config_include.h
#endif

#ifndef RCS_HH
#define RCS_HH


/* Include Files */

#if !defined(java) && !defined(rtlinux)

/* Linked Lists etc. */
#include "linklist.hh"          /* class RCS_LINKED_LIST */

/* Portable Print functions. */
#include "rcs_prnt.hh"          /* rcs_print_ functions */

/* Neutral Messaging Language (NML) */
#include "nmlmsg.hh"            /* class NMLmsg */
#include "nml.hh"               /* class NML */
#include "cms.hh"               /* class CMS */
#include "cms_cfg.hh"               /* cmscfg_...(), load_nml_config_file() */
#include "cms_enum_info.h" 	/* struct cms_enum_info; */

#include "cmd_msg.hh"           // RCS_CMD_MSG,  RCS_CMD_CHANNEL
#include "stat_msg.hh"          // RCS_STAT_MSG,  RCS_STAT_CHANNEL
#include "stat_msg_v2.hh"          // RCS_STAT_MSG_V2
#include "stat_msg_v2_n_codegen_protos.hh" // RCS_STAT_MSG_V2
#include "nml_oi.hh"            // nmlErrorFormat(), NML_TEXT, NML_ERROR

#ifdef ENABLE_RCS_DIAG
#include "cmsdiag.hh"           // class CMS_DIAGNOSTICS_INFO
#include "nmldiag.hh"           // class NML_DIAGNOSTICS_INFO
#endif

#if !defined(DOS_WINDOWS) || defined(WIN32)
#include "nml_srv.hh"           /* class NML_SERVER */
#endif


/* System Utilities. */
#include "timer.hh"             /* class RCS_TIMER, etime(),esleep() */

/* Pose/Vector/Matrix Math Classes */
#include "posemath.h"

/* class INIFILE */
#include "inifile.h"

/* class NML_MODULE */
#include "nml_mod.hh"

/* class NML_QR_SERVER, NML_QR_CLIENT, NML_QUERY_MSG */
#include "nmlqr.hh"

#endif 
  // java || rtlinux

#ifdef rtlinux
#define RCS_EXPORT
#ifndef rtlinux_09J
#include "rtlnml.h"
#endif
#include "posemath.h"
#endif

#include "rcs_mnmxtotal.hh"

#include "rcsvers.hh"



#ifndef NO_RCS_ID
#ifndef __GNUC__
#ifndef __attribute__
#define __attribute__(x)
#endif
#endif

// No longer using RCS specific exit.
#ifndef rcs_exit
#define rcs_exit(x) exit(x)
#endif

static const char __attribute__((unused)) *rcs_hh_rcs_id= "@(#)" "$Id: rcs.hh 1786 2011-05-27 19:00:59Z shackle $";
#endif

#if defined(WIN32) && !defined(NO_PRAGMAS) 
#pragma comment(lib,"rcs")
#pragma comment(lib,"ws2_32")
#endif

#endif                          
/* !defined(RCS_HH) */
