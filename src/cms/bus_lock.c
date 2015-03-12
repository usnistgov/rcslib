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

/* This line prevents findsource from adding this file to the Makefile */
#define NOT_A_DEFAULT_RCSLIB_SOURCE 1

#include "bus_lock.h"

#include <vxWorks.h>
#include <taskLib.h>
#include <logLib.h>
#include <intLib.h>
#include <string.h>

struct BL_ADDR_INFO
{
  unsigned long addr;
  int board_type;
};

int bus_lock_use_intlock = 1;
void *test_bus_lock_addr = (void *) 0x4e00000;
int test_buf_size = 0x4000;
int test_bus_lock_board_type = VX_MVME162_BOARD_TYPE;
static char c = 'a';
int testBusLockQuit;
int test_bus_lock_count = 0x4000;
int intLockKey = 0;



void
regWriteChar (char *addr, char c)
{
  *addr = c;
}

char
regReadChar (char *addr)
{
  return *addr;
}

void
regWriteShort (short *addr, short c)
{
  *addr = c;
}

short
regReadShort (short *addr)
{
  return *addr;
}

void
regWriteLong (long *addr, long c)
{
  *addr = c;
}

long
regReadLong (long *addr)
{
  return *addr;
}

void
regWriteULong (unsigned long *addr, unsigned long c)
{
  *addr = c;
}

unsigned long
regReadULong (long *addr)
{
  return *addr;
}




int
testBusLockRead ()
{
  struct BL_ADDR_INFO *info;
  char c1, c2;
  int i;

  info =
    getBusLockInfo (test_bus_lock_board_type,
		    (unsigned long) test_bus_lock_addr);
  if (0 == info)
    {
      return -1;
    }
  for (i = 0; i < test_bus_lock_count && !testBusLockQuit; i++)
    {
      enableBusLock (info);
      c1 = *((char *) test_bus_lock_addr);
      c2 = *((char *) test_bus_lock_addr + test_buf_size - 1);
      disableBusLock (info);
      if (c1 != c2)
	{
	  logMsg (" %c != %c", c1, c2, 0, 0, 0, 0);
	  return -1;
	}
    }
  return(0);
}



int
testBusLockWrite ()
{
  struct BL_ADDR_INFO *info;
  int i;

  info =
    getBusLockInfo (test_bus_lock_board_type,
		    (unsigned long) test_bus_lock_addr);
  if (0 == info)
    {
      return -1;
    }
  for (i = 0; i < test_bus_lock_count && !testBusLockQuit; i++)
    {
      c++;
      if (c > 'z')
	{
	  c = 'a';
	}
      enableBusLock (info);
      memset (test_bus_lock_addr, c, test_buf_size);
      disableBusLock (info);
    }
  freeBusLockInfo (info);
  return 0;
}

struct BL_ADDR_INFO *
getBusLockInfo (int board_type, unsigned long addr)
{
  struct BL_ADDR_INFO *info;

  switch (board_type)
    {
    case VX_MVME162_BOARD_TYPE:
      info = (struct BL_ADDR_INFO *) malloc (sizeof (struct BL_ADDR_INFO));
      if (0 == info)
	{
	  return 0;
	}
      info->addr = addr;
      info->board_type = board_type;
      return info;

    default:
      break;
    }
  return (0);
}

void
freeBusLockInfo (struct BL_ADDR_INFO *info)
{
  if (0 == info)
    {
      return;
    }
  free (info);
}

int
enableBusLock (struct BL_ADDR_INFO *info)
{
  long requestor_control_register_val;
  long DHB = 0;
  int board_type;
  if (0 == info)
    {
      return -1;
    }
#ifdef VXWORKS
  if (bus_lock_use_intlock)
    {
      intLockKey = intLock ();
    }
  else
    {
      taskLock ();
    }
#endif
  board_type = info->board_type;
  switch (board_type)
    {
    case VX_MVME162_BOARD_TYPE:
      while (!DHB)
	{
	  requestor_control_register_val = regReadLong ((long *) 0xFFF40030);
	  DHB = requestor_control_register_val & (1 << 14);
	  requestor_control_register_val |= (1 << 13);
	  regWriteLong (((long *) 0xFFF40030),
			requestor_control_register_val);
	}
      return 0;

    default:
      return -1;
    }
  return -1;
}





int
disableBusLock (struct BL_ADDR_INFO *info)
{
  long requestor_control_register_val;
  int board_type;
  if (0 == info)
    {
      return -1;
    }
  board_type = info->board_type;
  switch (board_type)
    {
    case VX_MVME162_BOARD_TYPE:
      requestor_control_register_val = regReadLong ((long *) 0xFFF40030);
      requestor_control_register_val &= ~(1 << 13);
      regWriteLong (((long *) 0xfff40030), requestor_control_register_val);
#ifdef VXWORKS
      if (bus_lock_use_intlock)
	{
	  intUnlock (intLockKey);
	}
      else
	{
	  taskUnlock ();
	}
#endif
      return 0;
    default:
      break;
    }
#ifdef VXWORKS
  if (bus_lock_use_intlock)
    {
      intUnlock (intLockKey);
    }
  else
    {
      taskUnlock ();
    }
#endif
  return -1;
}
