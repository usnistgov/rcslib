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


#ifndef _TABLE_H
#define _TABLE_H

#include <stddef.h>		/* size_t */

typedef struct
{
  unsigned long int *table;	/* ptr to first entry */
  int dsize;			/* acutal size of each data, in bytes */
  int lsize;			/* upped size of each data, in longs */
  unsigned long int size;	/* # of slots in table */
  unsigned long int count;	/* # of full slots */
}
_RCS_TABLE;

#ifdef __cplusplus
extern "C"
{
#endif

  extern int table_new (_RCS_TABLE * table, size_t dsize);
  extern int table_delete (_RCS_TABLE * table);
  extern int table_add (_RCS_TABLE * table, unsigned long int key,
			const void *data);
  extern int table_get (_RCS_TABLE * table, unsigned long int key,
			void *data);
  extern int table_clear (_RCS_TABLE * table, unsigned long int key);
  extern int table_clearall (_RCS_TABLE * table);
  extern void table_print (_RCS_TABLE * table);

#ifdef __cplusplus
}
#endif

#endif
