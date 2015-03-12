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


/*
 Filename: dma.c

 This file provides functions needed to access a DMA controller for
 local to/from VME bus transfers. The interface
 is based on the documentation for the VMEchip2 in the MVME162LX Embedded
 Controller Programmer's Reference Guide (MVME162LXPG/D1), with the April 1996 updates. (MVME162LXPG/DIA2) These functions are used within globmem.cc.

 MODIFICATIONS:
 18-Jun-1998 WPS created.

 */


#include <stdlib.h>		/* malloc(), free() */
#include <string.h>		/* memset */
#include <stdio.h>		/* printf */

#ifdef VXWORKS
#include <vxWorks.h>
#include <logLib.h>		/* logMsg() */
#include <taskLib.h>		/* taskDelay() */
#include <tickLib.h>		/* tickGet() */
#include <usrLib.h>		/* d() */
#endif

#include "dma.h"
#include "dbg_mem.h"		/* DEBUG_FREE,DEBUG_MALLOC,DEBUG_CALLOC */

unsigned long DMA_VMEBUS_REQUEST_LEVEL = 0;

/* DMAC Control Register 1, 2-51 MVME162LX Programmer's Guide */
/* DFAIR=4 (Wait for VME Bus Inactive),  */
/* DRELM=(3-2) (VME Bus Release Mode) { 0=BRx&timer,1=timer,2=BRx,3=BRx|timer} */
/* This variable is OR'd with the DMAC Control Register */
unsigned long MVME162_DMA_CONTROL_OPTIONS = 0;

/* DMAC Ton/Toff and VMEbus Global Timeout Control Register, 2-59 MVME162LX Programmer's Guide */
/* TIME OFF = (23-21) */
/* TIME_ON = (20-18) */
/* VGTO = (17-16) */
unsigned long MVME162_DMA_TIMEOUT_OPTIONS = 0;



/* DMAC Command Table Format, 2-48 MVME162LX Programmer's Guide */
struct MVME162_DMAC_COMMAND
{
  unsigned long control_word;
  void *local_bus_address;
  void *vme_bus_address;
  unsigned long byte_count;
  void *next_command;
};


struct DMA_ADDR_INFO
{
  int board_type;
  unsigned long addr;
  unsigned long am;
  int address_decoder;
  int ignore_dma_errors;
};

void *dma_command_list = 0;
int dma_command_started = 0;
int dma_print_errors = 1;

void *testDMAVMEAddress = ((void *) 0x4e00000);
long testDMABufSize = 0x4000;
int testDMABoardType = VX_MVME162_BOARD_TYPE;
static char c = 'a';
void *local_data = 0;
struct DMA_ADDR_INFO *testDMAinfo = 0;
int MVME162_DMA_DELAY_COUNT = 1000;

#define MAX_MVME162_DMAC_COMMANDS (4)
struct MVME162_DMAC_COMMAND
  mvme162_dmac_commands_list1[MAX_MVME162_DMAC_COMMANDS];
struct MVME162_DMAC_COMMAND
  mvme162_dmac_commands_list2[MAX_MVME162_DMAC_COMMANDS];
struct MVME162_DMAC_COMMAND *mvme162_dmac_commands_list =
  mvme162_dmac_commands_list1;
int num_mvme162_dmac_commands = 0;
int startCommandTicks = 0;

/*  Same arguments as d(), unfortunately d seems to be a shell function we can't call under vxworks5.3.*/
int
displayMemory (void *addr, int nunits, int nwidth)
{
  int i;
  for (i = 0; i < nunits * nwidth / 16; i++)
    {
      switch (nwidth)
	{
	case 4:
#ifdef VXWORKS
	  logMsg (" %8.8X: %8.8X %8.8X %8.8X %8.8X\n",
		  (((unsigned long) addr) + (i * 16)),
		  (*(unsigned long *) (((unsigned long) addr) + (i * 16))),
		  (*(unsigned long *)
		   (((unsigned long) addr) + (i * 16 + 4))),
		  (*(unsigned long *)
		   (((unsigned long) addr) + (i * 16) + 8)),
		  (*(unsigned long *)
		   (((unsigned long) addr) + (i * 16) + 12)), 0);
#else
	  return -1;
#endif
	  break;

	default:
	  return -1;
	}
    }
  return 0;
}

int
setTestDMAData (char c)
{
  if (0 == local_data)
    {
      local_data = (void *) DEBUG_MALLOC (testDMABufSize);
    }
  memset (local_data, c, testDMABufSize);
  return(0);
}

int dma_switch = 0;

int
testDMA ()
{
  unsigned long ticks = 0;
#ifdef VXWORKS
  ULONG startTicks = tickGet ();
#endif
  if (dma_switch)
    {
      if (mvme162_dmac_commands_list == mvme162_dmac_commands_list1)
	{
	  mvme162_dmac_commands_list = mvme162_dmac_commands_list2;
	}
      else
	{
	  mvme162_dmac_commands_list = mvme162_dmac_commands_list1;
	}
    }
  printf ("c=%c\n", c);
  setTestDMAData (c);
  c++;
  if (c > 'z')
    {
      c = 'a';
    }

  dma_command_started = 0;

#ifdef VERBOSE
  displayMemory ((void *) 0xfff40030, 8, 4);
  displayMemory (mvme162_dmac_commands_list, 8, 4);
#ifdef VXWORKS
  logMsg (" getDMAInfo %X %d\n", (int) dma_command_list, dma_command_started,
	  0, 0, 0, 0);
#endif
#endif
  if (0 == testDMAinfo)
    {
      testDMAinfo =
	getDMAInfo (testDMABoardType, ((unsigned long) testDMAVMEAddress));
    }
  if (0 == testDMAinfo)
    {
      return -1;
    }
#ifdef VERBOSE
  displayMemory ((void *) 0xfff40030, 8, 4);
  displayMemory (mvme162_dmac_commands_list, 8, 4);
#ifdef VXWORKS
  logMsg (" checkForDMADone %X %d\n", (int) dma_command_list,
	  dma_command_started, 0, 0, 0, 0);
#endif
#endif
  taskDelay (1);
  taskLock ();
  if (!checkForDMADone (testDMAinfo))
    {
#ifdef VXWORKS
      logMsg (" DMA not Done. %X %d\n", (int) dma_command_list,
	      dma_command_started, 0, 0, 0, 0);
#endif
      displayMemory ((void *) 0xfff40030, 8, 4);
      taskUnlock ();
      return -1;
    }
#ifdef VERBOSE
  displayMemory ((void *) 0xfff40030, 8, 4);
  displayMemory (mvme162_dmac_commands_list, 8, 4);
#ifdef VXWORKS
  logMsg (" DMACLearScheduledTransfersList %X %d\n", (int) dma_command_list,
	  dma_command_started, 0, 0, 0, 0);
#endif
#endif
  if (DMAClearScheduledTransfersList (testDMAinfo) < 0)
    {
#ifdef VXWORKS
      logMsg (" Can't clear scheduled DMA transfer list. %X %d\n",
	      (int) dma_command_list, dma_command_started, 0, 0, 0, 0);
#endif
      freeDMAInfo (testDMAinfo);
      testDMAinfo = 0;
      taskUnlock ();
      return -1;
    }
#ifdef VERBOSE
  displayMemory ((void *) 0xfff40030, 8, 4);
  displayMemory (mvme162_dmac_commands_list, 8, 4);
#ifdef VXWORKS
  logMsg (" DMASheduleLocalToVMETransfer %X %d\n", (int) dma_command_list,
	  dma_command_started, 0, 0, 0, 0);
#endif
#endif
  if (DMAScheduleLocalToVMETransfer
      (testDMAinfo, testDMAVMEAddress, local_data, testDMABufSize) < 0)
    {
#ifdef VXWORKS
      logMsg (" Can't schedule DMA transfer. %X %d\n", (int) dma_command_list,
	      dma_command_started, 0, 0, 0, 0);
#endif
      freeDMAInfo (testDMAinfo);
      testDMAinfo = 0;
      taskUnlock ();
      return -1;
    }
#ifdef VERBOSE
  displayMemory ((void *) 0xfff40030, 8, 4);
  displayMemory (mvme162_dmac_commands_list, 8, 4);
#ifdef VXWORKS
  logMsg (" DMAStartTransfers %X %d\n", (int) dma_command_list,
	  dma_command_started, 0, 0, 0, 0);
#endif
#endif
  if (DMAStartTransfers (testDMAinfo) < 0)
    {
#ifdef VXWORKS
      logMsg (" Can't start DMA transfer. %X %d\n", (int) dma_command_list,
	      dma_command_started, 0, 0, 0, 0);
#endif
      freeDMAInfo (testDMAinfo);
      testDMAinfo = 0;
      taskUnlock ();
      return -1;
    }
#ifdef VERBOSE
  displayMemory ((void *) 0xfff40030, 8, 4);
  displayMemory (mvme162_dmac_commands_list, 8, 4);
#ifdef VXWORKS
  logMsg (" checkForDMADone  %X %d\n", (int) dma_command_list,
	  dma_command_started, 0, 0, 0, 0);
#endif
#endif
  while (ticks < 100)
    {
      switch (checkForDMADone (testDMAinfo))
	{
	case 0:
#ifdef VERBOSE
	  displayMemory ((void *) 0xfff40030, 8, 4);
	  displayMemory (mvme162_dmac_commands_list, 8, 4);
#endif
	  break;

	case 1:
#ifdef VERBOSE
	  displayMemory ((void *) 0xfff40030, 8, 4);
	  displayMemory (mvme162_dmac_commands_list, 8, 4);
#endif
	  printf ("*((char *)%p)=%c\n", 
		  ((void *) testDMAVMEAddress),
		  *((char *) testDMAVMEAddress));
	  taskUnlock ();
	  return ticks;

	default:
	  displayMemory ((void *) 0xfff40000, 64, 4);
	  displayMemory (mvme162_dmac_commands_list, 8, 4);
	  taskUnlock ();
	  return -1;
	}
#ifdef VXWORKS
      ticks = tickGet () - startTicks;
#else
      ticks++;
#endif
    }
#ifdef VERBOSE
  displayMemory ((void *) 0xfff40030, 8, 4);
  displayMemory (mvme162_dmac_commands_list, 8, 4);
  taskUnlock ();
#ifdef VXWORKS
  logMsg ("\ntestDMA finish\n", 0, 0, 0, 0, 0, 0);
#endif
#endif
  printf ("*((char *)%p)=%c\n", 
	  ((void *)testDMAVMEAddress),
	  *((char *) testDMAVMEAddress));
  return ticks;
}

unsigned long orig_data = 0;
char orig_data_char;

int mvme162_dma_init (struct DMA_ADDR_INFO *info);

struct DMA_ADDR_INFO *
getDMAInfo (int board_type, unsigned long addr)
{
  struct DMA_ADDR_INFO *info = 0;
  unsigned long vme_bus_enable_control_reg;
  unsigned long io_control_reg;
  unsigned long a1;
  unsigned long enda1;
  unsigned long starta1;
  int tries = 0;
  int addr_found = 0;
  info =
    (struct DMA_ADDR_INFO *) DEBUG_MALLOC (sizeof (struct DMA_ADDR_INFO));
  if (0 == info)
    {
      return ((struct DMA_ADDR_INFO *) 0);
    }
  info->board_type = board_type;
  info->addr = addr;
  switch (board_type)
    {
    case VX_MVME162_BOARD_TYPE:
      if (0 == info)
	{
	  return 0;
	}
      if (addr & 3)
	{
	  return 0;
	}
      info->address_decoder = 0;
      info->am = 0;
      info->ignore_dma_errors = 1;
      if (num_mvme162_dmac_commands < 1)
	{
	  memset (mvme162_dmac_commands_list, 0,
		  sizeof (struct MVME162_DMAC_COMMAND) *
		  MAX_MVME162_DMAC_COMMANDS);
	  num_mvme162_dmac_commands = 0;
	}
      /* Local Bus to VMEbus Enable Control Register, 2-44 MVME162LX Programmer's Guide */
      vme_bus_enable_control_reg =
	(0x0f0000 & *((unsigned long *) 0xFFF4002C));
      if (vme_bus_enable_control_reg & 0x10000)	/* Address decoder 1 enabled. */
	{
	  /* Local Bus Slave (VMEbus Master) Starting Address/Ending Address Register 1, 2-36 MVME162LX Programmer's Guide */
	  a1 = *((unsigned long *) 0xFFF40014);
	  enda1 = (0xFFFF0000 & a1);
	  starta1 = ((0x0000FFFF & a1) << 16);
	  if (addr > starta1 && addr < enda1)
	    {
	      info->address_decoder = 1;
	      /* Local Bus Slave (VMEbus Master) Attribute Register 1, 2-42 MVME162LX Programmer's Guide */
	      info->am = 0x3f & *((unsigned long *) 0xfff40028);
#ifdef VXWORKS
	      taskDelay (1);
#endif
	      addr_found = 1;
	    }
	}
      if (vme_bus_enable_control_reg & 0x20000 && !addr_found)	/* Address decoder 2 enabled. */
	{
	  /* Local Bus Slave (VMEbus Master) Starting Address/Ending Address Register 2, 2-36 MVME162LX Programmer's Guide */
	  a1 = *((unsigned long *) 0xFFF40018);
	  enda1 = (0xFFFF0000 & a1);
	  starta1 = ((0x0000FFFF & a1) << 16);
	  if (addr > starta1 && addr < enda1)
	    {
	      info->address_decoder = 2;
	      /* Local Bus Slave (VMEbus Master) Attribute Register 2, 2-41 MVME162LX Programmer's Guide */
	      info->am = 0x3f & (*((unsigned long *) 0xfff40028) >> 8);
#ifdef VXWORKS
	      taskDelay (1);
#endif
	      addr_found = 1;
	    }
	}
      if (vme_bus_enable_control_reg & 0x40000 && !addr_found)	/* Address decoder 3 enabled. */
	{
	  /* Local Bus Slave (VMEbus Master) Starting Address/Ending Address Register 3, 2-37 MVME162LX Programmer's Guide */
	  a1 = *((unsigned long *) 0xFFF4001C);
	  enda1 = (0xFFFF0000 & a1);
	  starta1 = ((0x0000FFFF & a1) << 16);
	  if (addr > starta1 && addr < enda1)
	    {
	      info->address_decoder = 1;
	      /* Local Bus Slave (VMEbus Master) Attribute Register 3, 2-40 MVME162LX Programmer's Guide */
	      info->am = 0x3f & (*((unsigned long *) 0xfff40028) >> 16);
#ifdef VXWORKS
	      taskDelay (1);
#endif
	      addr_found = 1;
	    }
	}
      if (vme_bus_enable_control_reg & 0x80000 && !addr_found)	/* Address decoder 4 enabled. */
	{
	  /* Local Bus Slave (VMEbus Master) Starting Address/Ending Address Register 4, 2-38 MVME162LX Programmer's Guide */
	  a1 = *((unsigned long *) 0xFFF40020);
	  enda1 = (0xFFFF0000 & a1);
	  starta1 = ((0x0000FFFF & a1) << 16);
	  if (addr > starta1 && addr < enda1)
	    {
	      info->address_decoder = 1;
	      /* Local Bus Slave (VMEbus Master) Attribute Register 4, 2-39 MVME162LX Programmer's Guide */
	      info->am = 0x3f & (*((unsigned long *) 0xfff40028) >> 24);
#ifdef VXWORKS
	      taskDelay (1);
#endif
	      addr_found = 1;
	    }
	}
      /* Local Bus to VMEbus I/O Control Register */
      if ((addr & (0xff << 24)) == (0xf0 << 24))	/* A24 space. */
	{
	  io_control_reg = *((unsigned long *) 0xfff4002c);
	  if (io_control_reg & (1 << 15))
	    {
	      addr_found = 1;
	      info->address_decoder = 6;
	      info->am = 0x0c;
	    }
	}
      if (addr_found)
	{
	  while (!mvme162_dma_init (info) && tries > 10)
	    {
#ifdef VXWORKS
	      taskDelay (1);
#endif
	      tries++;
	    }
	  return info;
	}


      DEBUG_FREE (info);
      return 0;

    default:
      break;
    }
  return info;
}

int
mvme162_dma_init (struct DMA_ADDR_INFO *info)
{
  int retval = 0;
  unsigned long dmac_status;
  unsigned long addr = info->addr;
  unsigned long ticks = 0;
#ifdef VXWORKS
  ULONG startTicks = tickGet ();
#endif
#ifdef VXWORKS
  taskLock ();
#endif
  orig_data = *((unsigned long *) addr);
  *((unsigned long *) addr) = ~orig_data;
  dma_print_errors = 0;
  while (!checkForDMADone (info) && ticks < 10)
    {
#ifdef VXWORKS
      ticks = tickGet () - startTicks;
#else
      ticks++;
#endif
    }
  DMAClearScheduledTransfersList (info);
  DMAScheduleLocalToVMETransfer (info, (void *) addr, &orig_data, 4);
  DMAStartTransfers (info);
  while (!checkForDMADone (info) && ticks < 10)
    {
#ifdef VXWORKS
      ticks = tickGet () - startTicks;
#else
      ticks++;
#endif
    }
  dma_print_errors = 1;
  /* DMAC Status Register, 2-58 MVME162LX Programmer's Guide */
  dmac_status = *((unsigned long *) 0xFFF40048);
#ifdef VXWORKS
  taskUnlock ();
#endif
  retval = ((dmac_status & 0x7f) == 1)
    && (orig_data == *((unsigned long *) addr));
  if (!retval)
    {
      *((unsigned long *) addr) = orig_data;
    }
  return retval;
}

void
freeDMAInfo (struct DMA_ADDR_INFO *info)
{
  if (0 != info)
    {
      DMAClearScheduledTransfersList (info);
      DEBUG_FREE (info);
    }
}

int
checkForDMADone (struct DMA_ADDR_INFO *info)
{
  unsigned long dmac_status;
  unsigned long dmac_control_word;
  unsigned long dmac_byte_counter;
  unsigned long dmac_table_address_counter;
  int done = 0;
  int error = 0;
  if (info == 0)
    {
      return 0;
    }
  switch (info->board_type)
    {
    case VX_MVME162_BOARD_TYPE:
      /* DMAC Status Register, 2-58 MVME162LX Programmer's Guide */
      dmac_status = *((unsigned long *) 0xFFF40048);
      done = dmac_status & 1;
      error = dmac_status & 0x7e;
      if (!done && !error && !dma_command_started)
	{
	  /* DMAC Control Register 1, 2-51 MVME162LX Programmer's Guide  */
	  dmac_control_word = *((unsigned long *) 0xfff40030);
	  /* DTBL = 5 */
	  if (dmac_control_word & (1 << 5))
	    {
	      /* DMAC Table Address Counter, 2-55 MVME162LX Programmer's Guide  */
	      dmac_table_address_counter = *((unsigned long *) 0xfff40044);

	      /* DMAC Byte Counter, 2-55 MVME162LX Programmer's Guide  */
	      dmac_byte_counter = *((unsigned long *) 0xfff40040);
	      return (((dmac_table_address_counter & 0x3) != 0)
		      && (dmac_byte_counter == 0));
	    }
	  else
	    {
	      /* DMAC Byte Counter, 2-55 MVME162LX Programmer's Guide  */
	      dmac_byte_counter = *((unsigned long *) 0xfff40040);
	      return (dmac_byte_counter == 0);
	    }
	}
      if (error)
	{
	  if (!dma_command_started || info->ignore_dma_errors)
	    {
	      return 1;
	    }
#ifdef VXWORKS
	  if (dma_print_errors)
	    {
	      logMsg ("DMA error 0x%X -- %s %s\n", error,
		      (int) ((error & 0x2) ? "(VMEbus ERR)" : ""),
		      (int) ((error & 0x4) ? "(Command Table ERR)" : ""), 0,
		      0, 0);
	      if (error & 0x8)
		logMsg ("(DLTO - DMA Local Bus Timeout ERR)\n", 0, 0, 0, 0, 0,
			0);
	      if (error & 0x10)
		logMsg ("(DLOB - DMA Offboard ERR)\n", 0, 0, 0, 0, 0, 0);
	      if (error & 0x20)
		logMsg ("(DLPE - DMA Parity ERR)\n", 0, 0, 0, 0, 0, 0);
	      if (error & 0x40)
		logMsg ("(DLBE - DMA Unknown ERR)\n", 0, 0, 0, 0, 0, 0);
	    }
#endif
	  /* Clear DMAC byte counter so next time we'll no we`re done */
	  *((unsigned long *) 0xfff40040) = 0;
	  dma_command_started = 0;
	  return -1;
	}
      if (done || error)
	{
	  dma_command_started = 0;
	}
      return done;

    default:
      break;
    }
  return 0;
}

int
DMAScheduleLocalToVMETransfer (struct DMA_ADDR_INFO *info, void *vme_dest,
			       void *local_src, unsigned long bytes)
{
  struct MVME162_DMAC_COMMAND *mvme162_cmd = 0;
  void **next_cmd;

  if (info == 0)
    {
      return -1;
    }
  switch (info->board_type)
    {
    case VX_MVME162_BOARD_TYPE:
      mvme162_cmd =
	(struct MVME162_DMAC_COMMAND *)
	&(mvme162_dmac_commands_list[num_mvme162_dmac_commands]);
      num_mvme162_dmac_commands++;
      mvme162_cmd->control_word = 0;
      /* TVME | VINC | LINC */
      mvme162_cmd->control_word |= (1 << 9) | (1 << 10) | (1 << 11);
      mvme162_cmd->control_word &= 0xFFFF;
      mvme162_cmd->control_word &= ~0x3f;
      mvme162_cmd->control_word |= info->am;
      mvme162_cmd->local_bus_address = local_src;
      mvme162_cmd->vme_bus_address = vme_dest;
      mvme162_cmd->byte_count = bytes;
      mvme162_cmd->next_command = (void *) 3;
      if (dma_command_list == 0
	  || (((unsigned long) dma_command_list) & 3) != 0)
	{
	  dma_command_list = mvme162_cmd;
	  /* Table Address Counter, 2-55 MVME162LX Programmer's Guide */
	  *((unsigned long *) 0xfff40044) = (unsigned long) dma_command_list;
	}
      else
	{
	  next_cmd =
	    &((struct MVME162_DMAC_COMMAND *) dma_command_list)->next_command;
	  while (*next_cmd != 0 && (((unsigned long) (*next_cmd)) & 3) == 0)
	    {
	      next_cmd =
		&((struct MVME162_DMAC_COMMAND *) (*next_cmd))->next_command;
	    }
	  *next_cmd = mvme162_cmd;
	}
      return 0;

    default:
      break;
    }
  return -1;
}

int
DMAScheduleVMEToLocalTransfer (struct DMA_ADDR_INFO *info, void *local_dest,
			       void *vme_src, unsigned long bytes)
{
  struct MVME162_DMAC_COMMAND *mvme162_cmd = 0;
  void **next_cmd;

  if (info == 0)
    {
      return -1;
    }
  switch (info->board_type)
    {
    case VX_MVME162_BOARD_TYPE:
      if (num_mvme162_dmac_commands >= MAX_MVME162_DMAC_COMMANDS)
	{
	  return -1;
	}
      mvme162_cmd =
	(struct MVME162_DMAC_COMMAND *)
	&(mvme162_dmac_commands_list[num_mvme162_dmac_commands]);
      num_mvme162_dmac_commands++;
      mvme162_cmd->control_word = *((unsigned long *) 0xFFF40034);
      /* VINC | LINC */
      mvme162_cmd->control_word |= (1 << 10) | (1 << 11);
      /* TINC */
      mvme162_cmd->control_word &= ~(1 << 9);
      mvme162_cmd->control_word &= ~(0x3f);
      mvme162_cmd->control_word |= info->am;
      mvme162_cmd->local_bus_address = local_dest;
      mvme162_cmd->vme_bus_address = vme_src;
      mvme162_cmd->byte_count = bytes;
      mvme162_cmd->next_command = (void *) 3;
      if (dma_command_list == 0
	  || (((unsigned long) dma_command_list) & 3) != 0)
	{
	  dma_command_list = mvme162_cmd;
	  /* Table Address Counter, 2-55 MVME162LX Programmer's Guide */
	  *((unsigned long *) 0xfff40044) = (unsigned long) dma_command_list;
	}
      else
	{
	  next_cmd =
	    &((struct MVME162_DMAC_COMMAND *) dma_command_list)->next_command;
	  while (*next_cmd != 0 && (((unsigned long) (*next_cmd)) & 3) == 0)
	    {
	      next_cmd =
		&((struct MVME162_DMAC_COMMAND *) (*next_cmd))->next_command;
	    }
	  *next_cmd = mvme162_cmd;
	}
      return 0;

    default:
      break;
    }
  return -1;
}

int
DMASingleLocalToVMETransfer (struct DMA_ADDR_INFO *info, void *vme_dest,
			     void *local_src, unsigned long bytes)
{
  unsigned long dmac_control_word = 0;

  if (info == 0)
    {
      return -1;
    }
  dma_command_started = 1;
  switch (info->board_type)
    {
    case VX_MVME162_BOARD_TYPE:
      /* Table Address Counter, 2-55 MVME162LX Programmers Guide   */
      *((unsigned long *) 0xfff40044) = 0;

      /* DMAC Local Bus Address Counter, 2-54 MVME162LX Programmer's Guide */
      *((unsigned long *) 0xfff40038) = (unsigned long) local_src;

      /* DMAC VMEbus Address Counter, 2-54  MVME162LX Programmer's Guide */
      *((unsigned long *) 0xfff4003c) = (unsigned long) vme_dest;

      /* DMAC Byte Counter, 2-55 MVME162LX Programmer's Guide */
      *((unsigned long *) 0xfff40040) = bytes;

      /* DMAC Control Register 2, 2-52 MVME162LX Programmer's Guide */
      dmac_control_word = *((unsigned long *) 0xfff40034);
      /* VINC = 11, LINC = 10,  TVME=9 */
      dmac_control_word |= (1 << 11) | (1 << 10) | (1 << 9);
      /* INTE = 15,  */
      dmac_control_word &= ~(1 << 15);
      dmac_control_word &= ~(0x3f);
      dmac_control_word |= info->am;
      *((unsigned long *) 0xfff40034) = dmac_control_word;

      /* DMAC Control Register 1, 2-51 MVME162LX Programmer's Guide */
      dmac_control_word = *((unsigned long *) 0xfff40030);
      /* DEN = 6  */
      dmac_control_word |= (1 << 6);
      /* DWB = 13, DTBL = 5 */
      dmac_control_word &= ~(1 << 13) & ~(1 << 5);
      /* Clear status, MPU Status and Interrupt Count Register, 2-57 MVME162LX Programmer's Guide */
      *((unsigned long *) 0xfff40048) = (1 << 11);
      *((unsigned long *) 0xfff40030) = dmac_control_word;
      return 0;

    default:
      break;
    }
  return -1;
}


int
DMASingleVMEToLocalTransfer (struct DMA_ADDR_INFO *info, void *local_dest,
			     void *vme_src, unsigned long bytes)
{
  unsigned long dmac_control_word = 0;

  if (info == 0)
    {
      return -1;
    }
  dma_command_started = 1;
  switch (info->board_type)
    {
    case VX_MVME162_BOARD_TYPE:
      /* Table Address Counter, 2-55 MVME162LX Programmers Guide   */
      *((unsigned long *) 0xfff40044) = 0;

      /* DMAC Local Bus Address Counter, 2-54 MVME162LX Programmer's Guide */
      *((unsigned long *) 0xfff40038) = (unsigned long) local_dest;

      /* DMAC VMEbus Address Counter, 2-54  MVME162LX Programmer's Guide */
      *((unsigned long *) 0xfff4003c) = (unsigned long) vme_src;

      /* DMAC Byte Counter, 2-55 MVME162LX Programmer's Guide */
      *((unsigned long *) 0xfff40040) = bytes;

      /* DMAC Control Register 2, 2-52 MVME162LX Programmer's Guide */
      dmac_control_word = *((unsigned long *) 0xfff40034);
      /* VINC = 11, LINC = 10 */
      dmac_control_word |= (1 << 11) | (1 << 10);
      /* INTE = 15,  TVME=9  */
      dmac_control_word &= ~(1 << 15) & ~(1 << 9);
      dmac_control_word &= ~(0x3f);
      dmac_control_word |= info->am;
      *((unsigned long *) 0xfff40034) = dmac_control_word;

      /* DMAC Control Register 1, 2-51 MVME162LX Programmer's Guide */
      dmac_control_word = *((unsigned long *) 0xfff40030);
      /* DEN = 6  */
      dmac_control_word |= (1 << 6);
      /* DWB = 13, DTBL = 5 */
      dmac_control_word &= ~(1 << 13) & ~(1 << 5);
      /* Clear status, MPU Status and Interrupt Count Register, 2-57 MVME162LX Programmer's Guide */
      *((unsigned long *) 0xfff40048) = (1 << 11);
      *((unsigned long *) 0xfff40030) = dmac_control_word;
      return 0;

    default:
      break;
    }
  return -1;
}

int
DMAStartTransfers (struct DMA_ADDR_INFO *info)
{
  unsigned long dmac_control_word = 0;
  unsigned long dmac_timeout_reg = 0;
  unsigned long lvreql;

  if (info == 0)
    {
      return -1;
    }
  if (!checkForDMADone (info))
    {
      return -1;
    }
  dma_command_started = 1;
  switch (info->board_type)
    {
    case VX_MVME162_BOARD_TYPE:
      if (dma_command_list == 0
	  || (((unsigned long) dma_command_list) & 3) != 0)
	{
	  return -1;
	}
      /* Table Address Counter, 2-55 MVME162LX Programmer's Guide */
      *((unsigned long *) 0xfff40044) = (unsigned long) dma_command_list;

      /* DMAC Control Register 1, 2-51 MVME162LX Programmer's Guide */
      dmac_control_word = *((unsigned long *) 0xfff40030);
      /* Get the normal VME bus Request Level and use it for the DMA requests
         also. */
      lvreql = (dmac_control_word & (3 << 8)) >> 8;
      dmac_control_word &= ~(0xff);
      /* DEN = 6 (Enable DMAC), DTBL = 5(Use Command Table), */
      /* DFAIR=4 (Wait for VME Bus Inactive),  */
      /* DRELM=(3-2) (VME Bus Release Mode) { 0=BRx&timer,1=timer,2=BRx,3=BRx|timer */
      /* DREQL=(0-1) (VMEbus Request Level) */
      dmac_control_word |=
	(1 << 5) | MVME162_DMA_CONTROL_OPTIONS | (0x3 &
						  DMA_VMEBUS_REQUEST_LEVEL);
      /* DWB = 13 */
      dmac_control_word &= ~(1 << 13);
      /* DHALT =7 */
      dmac_control_word &= ~(1 << 7) & ~(1 << 6);
      /* Clear MPU status, MPU Status and DMA interrupt Count Register */
      *((unsigned long *) 0xfff40048) = (1 << 11);

      /* Clear byte counter */
      *((unsigned long *) 0xfff40040) = 0;
      /* Set DMA options */
      *((unsigned long *) 0xfff40030) = dmac_control_word;
      /* Clear byte counter */
      *((unsigned long *) 0xfff40040) = 0;
      /* DMAC Ton/Toff and VMEbus Global Timeout Control Register, 2-59 MVME162LX Programmer's Guide */
      dmac_timeout_reg = *((unsigned long *) 0xfff4004c);
      dmac_timeout_reg &= ~(0xff << 16);
      dmac_timeout_reg |= MVME162_DMA_TIMEOUT_OPTIONS;
      *((unsigned long *) 0xfff4004c) = dmac_timeout_reg;
      /* Check that the settings are still correct. */
      if (*((unsigned long *) 0xfff40044) !=
	  ((unsigned long) dma_command_list)
	  || *((unsigned long *) 0xfff40030) != dmac_control_word)
	{
	  return -1;
	}
      /* Enable DMA */
      dmac_control_word |= (1 << 6);
      *((unsigned long *) 0xfff40030) = dmac_control_word;
#ifdef VXWORKS
      startCommandTicks = tickGet ();
#endif
      return 0;

    default:
      break;
    }
  return -1;
}

int
DMAClearScheduledTransfersList (struct DMA_ADDR_INFO *info)
{
  unsigned long dmac_control_word;
  unsigned long ticks = 0;
#ifdef VXWORKS
  ULONG startTicks = tickGet ();
#endif

  if (info == 0)
    {
      return -1;
    }

  dma_command_started = 0;
  switch (info->board_type)
    {
    case VX_MVME162_BOARD_TYPE:
      /* last_cmd_ptr = cmd_ptr = dma_command_list;
         while(cmd_ptr != 0 && (((unsigned long) cmd_ptr) & 3) == 0)
         {
         cmd_ptr = ((struct MVME162_DMAC_COMMAND *)cmd_ptr)->next_command;
         DEBUG_FREE(last_cmd_ptr);
         } */
      /* DMAC Control Register 1, 2-51 MVME162LX Programmer's Guide */
      dmac_control_word = *((unsigned long *) 0xfff40030);
      /* DHALT =7 */
      dmac_control_word |= (1 << 7);
      /* DEN = 6 */
      dmac_control_word &= ~(1 << 6);
      *((unsigned long *) 0xfff40030) = dmac_control_word;
      /* DMAC Byte Counter, 2-55 MVME162LX Programmer's Guide */
      while (*((unsigned long *) 0xfff40040) > 0)
	{
	  /* Clear MPU status, MPU Status and DMA interrupt Count Register */
	  if (*((unsigned long *) 0xfff40048) & 0x7f)
	    {
	      break;
	    }
#ifdef VXWORKS
	  ticks = tickGet () - startTicks;
#else
	  ticks++;
#endif
	  if (ticks > 100)
	    {
#ifdef VXWORKS
	      displayMemory ((void *) 0xfff40030, 8, 4);
#endif
	      *((unsigned long *) 0xfff40044) = 0x3;
	      *((unsigned long *) 0xfff40040) = 0;
	      return -1;
	    }
	}
      /* Table Counter */
      *((unsigned long *) 0xfff40044) = 0x3;
      memset (mvme162_dmac_commands_list, 0,
	      sizeof (struct MVME162_DMAC_COMMAND) *
	      MAX_MVME162_DMAC_COMMANDS);
      num_mvme162_dmac_commands = 0;
      dma_command_list = 0;
      break;

    default:
      break;
    }
  return 0;
}
