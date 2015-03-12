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


#ifndef WIN_PRNT_HH
#define WIN_PRNT_HH

#include "rcs_defs.hh"		/* RCS_FAR, RCS_PASCAL */

#if defined(_Windows) || defined(_WINDOWS)

#ifdef __cplusplus
extern "C"
{
#endif

/* Create a window for rcs output. */
  void RCS_FAR *RCS_EXPORT create_default_rcs_print_window ();

  void RCS_FAR *RCS_EXPORT create_rcs_print_window (void RCS_FAR * hInstance,
						    int nCmdShow,
						    void RCS_FAR *
						    hwndParent);

#ifdef __cplusplus
}
#endif

extern void RCS_EXPORT remove_rcs_print_window ();

extern void RCS_EXPORT start_rcs_print_window (void *);

extern unsigned long RCS_EXPORT run_rcs_print_window (void *);


extern void RCS_FAR RCS_PASCAL update_rcs_print_window ();

extern void (*callback_on_rcs_print_window_close) (void);


#endif


#endif /* WIN_PRNT_HH */
