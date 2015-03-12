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



#ifndef RCS_EXIT_HH
#define RCS_EXIT_HH

#ifdef __cplusplus
extern "C"
{
#endif

// NOTE --
// the GNU VxWorks C++ cross-compiler (g++68k) has a bug, that
// prevents me from passing a pointer to a function as the first
// argument of a function.
#ifdef VXWORKS
  int attach_rcs_exit_list (void *);
#else
  int attach_rcs_exit_list (void (*fptr) (int));
#endif
  void rcs_cleanup (int code);
  void rcs_exit (int code);

#ifdef __cplusplus
}
#endif

#endif
