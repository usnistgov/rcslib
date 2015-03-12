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

#ifndef FILEOPS_H
#define FILEOPS_H

/* returns non-zero if path arg is a normal file, or link to a
   normal file, 0 otherwise */
extern int rcs_fileop_isfile (const char *path);

/* returns non-zero if path arg is a directory, or link to a
   directory, 0 otherwise */
extern int rcs_fileop_isdir (const char *path);

/* returns non-zero if path arg is a link to anything */
extern int rcs_fileop_islink (const char *path);

#endif
