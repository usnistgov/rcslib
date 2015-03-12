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

#ifndef NTOHHTON_HH
#define  NTOHHTON_HH

static const int endian_test_int=1;
static const char * const endian_test_cptr=(const char *)&endian_test_int;

static inline void
hton_uint32_array_set(void *addr, const unsigned int index, const unsigned long ul)
{
  unsigned long offset = index*4;
  char *cdest_addr = ((char *)addr) +offset;
  const char *csrc_addr = (char *) &ul;
  unsigned int ui=0;

  if(sizeof(unsigned long) == 8 && sizeof(unsigned int) == 4)
    {
      ui=(unsigned int) ul;
      csrc_addr = (char *) &ui;
    }
  if(*endian_test_cptr)
    {
      cdest_addr[0] = csrc_addr[3];
      cdest_addr[1] = csrc_addr[2];
      cdest_addr[2] = csrc_addr[1];
      cdest_addr[3] = csrc_addr[0];
    }
  else
    {
      cdest_addr[0] = csrc_addr[0];
      cdest_addr[1] = csrc_addr[1];
      cdest_addr[2] = csrc_addr[2];
      cdest_addr[3] = csrc_addr[3];
    }
}

static inline unsigned long
ntoh_uint32_array_get(const void *addr, const unsigned int index )
{
  unsigned long offset = index*4;
  const char *csrc_addr = ((const char *)addr) +offset;
  unsigned long ul=0;
  char *cdest_addr = (char *) &ul;
  unsigned int ui=0;

  if(sizeof(unsigned long) == 8 && sizeof(unsigned int) == 4)
    {
      cdest_addr = (char *) &ui;
    }
  if(*endian_test_cptr)
    {
      cdest_addr[0] = csrc_addr[3];
      cdest_addr[1] = csrc_addr[2];
      cdest_addr[2] = csrc_addr[1];
      cdest_addr[3] = csrc_addr[0];
    }
  else
    {
      cdest_addr[0] = csrc_addr[0];
      cdest_addr[1] = csrc_addr[1];
      cdest_addr[2] = csrc_addr[2];
      cdest_addr[3] = csrc_addr[3];
    }
  if(sizeof(unsigned long) == 8 && sizeof(unsigned int) == 4)
    {
      ul=(unsigned long) ui;
    }
  return ul;
}  


static inline void
hton_float64_array_set(void *addr, const unsigned int index, const double  d)
{
  unsigned long offset = index*8;
  char *cdest_addr = ((char *)addr) +offset;
  const char *csrc_addr = (char *) &d;

  if(*endian_test_cptr)
    {
      cdest_addr[0] = csrc_addr[7];
      cdest_addr[1] = csrc_addr[6];
      cdest_addr[2] = csrc_addr[5];
      cdest_addr[3] = csrc_addr[4];
      cdest_addr[4] = csrc_addr[3];
      cdest_addr[5] = csrc_addr[2];
      cdest_addr[6] = csrc_addr[1];
      cdest_addr[7] = csrc_addr[0];
    }
  else
    {
      cdest_addr[0] = csrc_addr[0];
      cdest_addr[1] = csrc_addr[1];
      cdest_addr[2] = csrc_addr[2];
      cdest_addr[3] = csrc_addr[3];
      cdest_addr[4] = csrc_addr[4];
      cdest_addr[5] = csrc_addr[5];
      cdest_addr[6] = csrc_addr[6];
      cdest_addr[7] = csrc_addr[7];
    }
}

static inline double
ntoh_float64_array_get(const void *addr, const unsigned int index )
{
  unsigned long offset = index*8;
  const char *csrc_addr = ((const char *)addr) +offset;
  double d;
  char *cdest_addr = (char *) &d;

  if(*endian_test_cptr)
    {
      cdest_addr[0] = csrc_addr[7];
      cdest_addr[1] = csrc_addr[6];
      cdest_addr[2] = csrc_addr[5];
      cdest_addr[3] = csrc_addr[4];
      cdest_addr[4] = csrc_addr[3];
      cdest_addr[5] = csrc_addr[2];
      cdest_addr[6] = csrc_addr[1];
      cdest_addr[7] = csrc_addr[0];
    }
  else
    {
      cdest_addr[0] = csrc_addr[0];
      cdest_addr[1] = csrc_addr[1];
      cdest_addr[2] = csrc_addr[2];
      cdest_addr[3] = csrc_addr[3];
      cdest_addr[4] = csrc_addr[4];
      cdest_addr[5] = csrc_addr[5];
      cdest_addr[6] = csrc_addr[6];
      cdest_addr[7] = csrc_addr[7];
    }
  return d;
}  
      
//  NTOHHTON_HH
#endif
