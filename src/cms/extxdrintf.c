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

#define XDR_SOURCE 1

#if HAVE_CONFIG_H
#include "rcs_config.h"
#endif

#if defined(ENABLE_RCS_XDR)


#ifdef HAVE_CONFIG_H
#include "rcs_config_include.h"
#else
#include "cms_xup_no_config.h"
#endif
/* HAVE_CONFIG_H */

#include "extxdrintf.h"


void ext_xdr_destroy(void *vp)
{
  xdr_destroy((XDR *)vp);
}

int ext_xdr_getpos(void *vp)
{
  return xdr_getpos((XDR *)vp);
}

void ext_xdr_setpos(void *vp,int pos)
{
  xdr_setpos((XDR *)vp,pos);
}

/*  defined(ENABLE_RCS_XDR) */
#else
#include "rcs_empty_source"
#endif
