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



#ifndef AUTOKEY_H
#define AUTOKEY_H

#ifdef __cplusplus
extern "C"
{
#endif

#define AUTOKEY_NAMELENGTH (32)

  struct AUTOKEY_TABLE_ENTRY
  {
    char name[AUTOKEY_NAMELENGTH];
    unsigned int chksum;
  };

  extern int autokey_getkey (void *table, int max, const char *name);
  extern int autokey_releasekey (void *table, int max, const char *name,
				 int key);

#ifdef __cplusplus
}
#endif


#endif
