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

#ifndef L64_H
#define L64_H

static const long endian_test_long = 1;
static const char * const endian_test_caddr = (const char *) &endian_test_long;

static inline void
l64_set(void *addr, const long l)
{
  if(sizeof(long) == 8)
    {
      *((long *)addr) = l;
      return;
    }
  else
    {
      if(*endian_test_caddr)
	{
	  long *laddr2low = (long *) addr;
	  *laddr2low = l;
	  long *laddr2high = ((long *) addr)+1;
	  *laddr2high = 0;
	}
      else
	{
	  long *laddr3low = ((long *) addr)+1;
	  *laddr3low = l;
	  long *laddr3high = ((long *) addr);
	  *laddr3high = 0;
	}
    }
}

static inline long 
l64_get(const void *addr, long *highpartptr)
{
  if(sizeof(long) == 8)
    {
      if(highpartptr)
	{
	  *highpartptr=0;
	}
      return *((long *)addr);
    }
  else
    {
      if(*endian_test_caddr)
	{
	  if(highpartptr)
	    {
	      *highpartptr=*(((long *)addr)+1);
	    }
	  return *((long *)addr);
	}
      else
	{
	  if(highpartptr)
	    {
	      *highpartptr=*(((long *)addr));
	    }
	  return *(((long *)addr)+1);
	}
    }
}

static inline void
ul64_set(void *addr, const unsigned long l)
{
  if(sizeof(unsigned long) == 8)
    {
      *((unsigned long *)addr) = l;
      return;
    }
  else
    {
      if(*endian_test_caddr)
	{
	  unsigned long *laddr2low = (unsigned long *) addr;
	  *laddr2low = l;
	  unsigned long *laddr2high = ((unsigned long *) addr)+1;
	  *laddr2high = 0;
	}
      else
	{
	  unsigned long *laddr3low = ((unsigned long *) addr)+1;
	  *laddr3low = l;
	  unsigned long *laddr3high = ((unsigned long *) addr);
	  *laddr3high = 0;
	}
    }
}

static inline unsigned long 
ul64_get(const void *addr, unsigned long *highpartptr)
{
  if(sizeof(unsigned long) == 8)
    {
      if(highpartptr)
	{
	  *highpartptr=0;
	}
      return *((unsigned long *)addr);
    }
  else
    {
      if(*endian_test_caddr)
	{
	  if(highpartptr)
	    {
	      *highpartptr=*(((unsigned long *)addr)+1);
	    }
	  return *((unsigned long *)addr);
	}
      else
	{
	  if(highpartptr)
	    {
	      *highpartptr=*(((unsigned long *)addr));
	    }
	  return *(((unsigned long *)addr)+1);
	}
    }
}

/* L64_H */
#endif
