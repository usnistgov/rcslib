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

/************************************************************************
* File: sendn.h
* Purpose: Provides a header file for the sendn function from
* the book Advanced Programming in the UNIX Environment  by Richard Stevens.
* The sendn function calls the send function repeatedly until n bytes
* have been written to the file descriptor.
*************************************************************************/

#ifndef SENDN_H
#define SENDN_H

#ifdef __cplusplus
extern "C" {
#endif

int sendn (int fd, const void *vptr, int n, int flags, double timeout);

#ifdef __cplusplus
}
#endif

#endif /* SENDN_H */
