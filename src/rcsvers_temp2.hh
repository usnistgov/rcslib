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



#ifndef RCSVERS_HH
#define RCSVERS_HH


// Version Info
#define RCS_VERSION "4.31"
#define RCS_MAJOR_VERSION (4)
#define RCS_MINOR_VERSION (__RCS_MINOR_VERSION_SUBST_HERE__)

extern int rcs_version_printed;
extern const int rcs_major_version_number;
extern const int rcs_minor_version_number;
extern const char *rcs_version_string;
extern const char *rcs_version_info_string;
extern int rcs_version_compare(const char *);
extern void RCS_EXPORT print_rcs_version();
extern "C" int rcsinfo();

#endif
