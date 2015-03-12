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

#ifndef CMS_XUP_NO_CONFIG_H
#define CMS_XUP_NO_CONFIG_H

#include "rcs_defs.hh"

#if defined(__cplusplus) && defined(EXTERN_C_STD_HEADERS)
extern "C" {
#endif

#ifndef NO_DCE_RPC
#include <rpc/rpc.h>		/* struct XDR */
#else
#if defined(VXWORKS) || defined(irix6)
#include <rpc/types.h>
#include <rpc/xdr.h>		// struct XDR
#else
#include "xdr.h"
#endif
#endif
#include <stdlib.h>

#if defined(__cplusplus) && defined(EXTERN_C_STD_HEADERS)
}
#endif

#if defined(linuxppc) || defined(linuxPPC) || defined(darwin)
#ifdef xdr_destroy
#undef xdr_destroy
#endif
#define xdr_destroy(xdrs)                               \
        if ((xdrs)->x_ops->x_destroy)                   \
                ((void (*)(XDR *)) (*(xdrs)->x_ops->x_destroy))(xdrs)
#ifdef xdr_setpos
#undef xdr_setpos
#endif
#define xdr_setpos(xdrs,pos)                            \
                ((bool_t (*)(XDR *, const u_int)) (*(xdrs)->x_ops->x_setpostn))(xdrs,pos)

#ifdef xdr_getpos
#undef xdr_getpos
#endif
#define xdr_getpos(xdrs)                                \
                ((u_int (*)(XDR *)) (*(xdrs)->x_ops->x_getpostn))(xdrs)

#endif

#if defined(VXWORKS) && defined(NO_DCE_RPC) && !defined(HAVE_XDR_VECTOR)
static inline bool_t
xdr_vector (XDR * xdrs, char *arrp, const u_int size,
	    const u_int elsize, const xdrproc_t elproc)
{
  int i;
  if (NULL == xdrs || NULL == arrp)
    {
      return FALSE;
    }
  for (i = 0; i < size; i++)
    {
      if (!elproc (xdrs, arrp))
	{
	  return FALSE;
	}
      arrp += elsize;
    }
  return TRUE;
}
#endif


#endif
