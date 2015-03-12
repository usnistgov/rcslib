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


#include "rcs_defs.hh"		/* __MSDOS__, _WINDOWS,RCS_FAR */
#include "_physmem.h"

#ifndef WIN32
#ifdef __MSDOS__
#ifdef _WINDOWS
#include <windows.h>

#ifndef __BORLANDC__
#ifdef __cplusplus
extern "C" WORD _0000h;
extern "C" WORD _0040h;
extern "C" WORD _A000h;
extern "C" WORD _B000h;
extern "C" WORD _B800h;
extern "C" WORD _C000h;
extern "C" WORD _D000h;
extern "C" WORD _E000h;
extern "C" WORD _F000h;
#else
extern WORD _0000h;
extern WORD _0040h;
extern WORD _A000h;
extern WORD _B000h;
extern WORD _B800h;
extern WORD _C000h;
extern WORD _D000h;
extern WORD _E000h;
extern WORD _F000h;
#endif
#endif

#else
#include <stdio.h>		/* fprintf() */
#include <dos.h>		/* _REGS, _SREGS */
#endif
#endif
#endif

#include <string.h>		/* memcpy() */

int
read_physmem (unsigned long source, void *destination, long bytes)
{

#ifndef WIN32
#ifdef _WINDOWS

  LPBYTE lpDestination;
  LPBYTE lpSource;
  WORD wSelector;
  int i;

  lpDestination = (LPBYTE) destination;
  lpSource = create_ptr_to_physmem (source, bytes, &wSelector);

  if (NULL == lpSource || NULL == lpDestination)
    {
      return (-1);
    }


  for (i = 0; i < bytes; i++)
    {
      lpDestination[i] = lpSource[i];
    }
  if (wSelector)
    {
      FreeSelector (wSelector);
    }
  return (i);
#else
  char far *ptr_to_destination;
  char far *ptr_to_source;
  unsigned long physical_source_address;
  unsigned long physical_destination_address;
  unsigned long temp_segment, temp_offset;
  static char two_bytes[2];
  char far *ptr_to_two_bytes;
  int i;

  if (((unsigned long) destination) < 0x100000UL)
    {
      ptr_to_source = (char far *)
	((source & 0xf) + ((source & 0xffff0L) << 16));
      ptr_to_destination = (char far *) destination;
      for (i = 0; i < bytes; i++)
	{
	  ptr_to_destination[i] = ptr_to_source[i];
	}
      return (i);
    }
  else
    {
      physical_source_address = source;
      temp_segment = (((unsigned long) destination) & 0xffff0000L) >> 16;
      temp_offset = ((unsigned long) destination) & 0x0000ffffL;
      physical_destination_address = (temp_segment << 4) + temp_offset;
      if (0 == (bytes % 2))
	{
	  return (move_physmem (physical_destination_address,
				physical_source_address, bytes));
	}
      else
	{
	  if (bytes > 1)
	    {
	      if (-1 == move_physmem (physical_destination_address,
				      physical_source_address, bytes))
		{
		  return -1;
		}
	    }
	  ptr_to_two_bytes = (char far *) two_bytes;
	  temp_segment =
	    (((unsigned long) ptr_to_two_bytes) & 0xffff0000L) >> 16;
	  temp_offset = ((unsigned long) ptr_to_two_bytes) & 0x0000ffffL;
	  physical_source_address = source + bytes - 1;
	  physical_destination_address = (temp_segment << 4) + temp_offset;
	  if (-1 == move_physmem (physical_destination_address,
				  physical_source_address, 2))
	    {
	      return -1;
	    }
	  ((char *) destination)[bytes - 1] = (char) two_bytes[0];
	  return (0);
	}
    }
#endif
#endif
  return (-1);			/* No Applicable PLATFORM  */
}

int
write_physmem (unsigned long destination, void *source, long bytes)
{
#ifndef WIN32
#ifdef _WINDOWS

  LPBYTE lpDestination;
  LPBYTE lpSource;
  WORD wSelector;
  DWORD max_size;
  int i;

  lpDestination = create_ptr_to_physmem (destination, bytes, &wSelector);
  lpSource = (LPBYTE) source;

  if (NULL == lpSource || NULL == lpDestination)
    {
      return (-1);
    }


  for (i = 0; i < bytes; i++)
    {
      lpDestination[i] = lpSource[i];
    }

  if (wSelector)
    {
      FreeSelector (wSelector);
    }
  return (i);
#else
  char far *ptr_to_destination;
  char far *ptr_to_source;
  unsigned long physical_source_address;
  unsigned long physical_destination_address;
  unsigned long temp_segment, temp_offset;
  int i;
  static char two_bytes[2];
  char far *ptr_to_two_bytes;

  if (destination < 0x100000UL)
    {
      ptr_to_source = (char far *) source;
      ptr_to_destination = (char far *)
	((destination & 0xf) + ((destination & 0xffff0L) << 16));
      for (i = 0; i < bytes; i++)
	{
	  ptr_to_destination[i] = ptr_to_source[i];
	}
      return (i);
    }
  else
    {
      temp_segment = (((unsigned long) source) & 0xffff0000L) >> 16;
      temp_offset = ((unsigned long) source) & 0x0000ffffL;
      physical_source_address = (temp_segment << 4) + temp_offset;
      physical_destination_address = destination;
      if (0 == (bytes % 2))
	{
	  return (move_physmem (physical_destination_address,
				physical_source_address, bytes));
	}
      else
	{
	  if (bytes > 1)
	    {
	      if (-1 == move_physmem (physical_destination_address,
				      physical_source_address, bytes))
		{
		  return -1;
		}
	    }
	  two_bytes[0] = ((char *) source)[bytes - 1];
	  ptr_to_two_bytes = (char far *) two_bytes;
	  temp_segment =
	    (((unsigned long) ptr_to_two_bytes) & 0xffff0000L) >> 16;
	  temp_offset = ((unsigned long) ptr_to_two_bytes) & 0x0000ffffL;
	  physical_destination_address = destination + bytes - 1;
	  physical_source_address = (temp_segment << 4) + temp_offset;
	  if (-1 == move_physmem (physical_destination_address,
				  physical_source_address, 2))
	    {
	      return -1;
	    }
	  return (0);
	}
    }
#endif
#endif
  return (-1);			/* No Applicable PLATFORM  */
}

#ifdef WIN16

LPBYTE
create_ptr_to_physmem (unsigned long phys_addr, int bytes,
		       WORD FAR * ptr_to_selector)
{
  LPBYTE lpPhysAddr;
  DWORD dwPhysAddrLinear;
  DWORD dwPhysAddrSegment;
  DWORD dwPhysAddrOffset;
  WORD wPhysAddrSelector;
  BOOL boolSynthesizeSelector;
  int max_size;

  if (NULL != ptr_to_selector)
    {
      *ptr_to_selector = 0;
    }

#ifndef __BORLANDC__
  if (phys_addr < 0x100000UL)	/* Is physical address below 1 MB */
    {
      dwPhysAddrSegment = ((phys_addr & 0xf0000UL) >> 4);
      dwPhysAddrOffset = (phys_addr & 0xffff);
      boolSynthesizeSelector = FALSE;
      max_size = 0x10000UL - dwPhysAddrOffset;
      switch (dwPhysAddrSegment)
	{
	case 0:
	  if (dwPhysAddrOffset < 0x400)
	    {
	      lpPhysAddr = (LPBYTE) MAKELONG (dwPhysAddrOffset, &_0000h);
	    }
	  else
	    {
	      dwPhysAddrOffset -= 0x400;
	      max_size += 0x400;
	      lpPhysAddr = (LPBYTE) MAKELONG (dwPhysAddrOffset, &_0040h);
	    }
	  break;

	case 0xA000:
	  lpPhysAddr = (LPBYTE) MAKELONG (dwPhysAddrOffset, &_A000h);
	  break;
	case 0xB000:
	  if (dwPhysAddrOffset < 0x8000)
	    {
	      lpPhysAddr = (LPBYTE) MAKELONG (dwPhysAddrOffset, &_B000h);
	    }
	  else
	    {
	      dwPhysAddrOffset -= 0x8000;
	      max_size += 0x8000;
	      lpPhysAddr = (LPBYTE) MAKELONG (dwPhysAddrOffset, &_B800h);
	    }
	  break;
	case 0xC000:
	  lpPhysAddr = (LPBYTE) MAKELONG (dwPhysAddrOffset, &_C000h);
	  break;
	case 0xD000:
	  lpPhysAddr = (LPBYTE) MAKELONG (dwPhysAddrOffset, &_D000h);
	  break;
	case 0xE000:
	  lpPhysAddr = (LPBYTE) MAKELONG (dwPhysAddrOffset, &_E000h);
	  break;
	case 0xF000:
	  lpPhysAddr = (LPBYTE) MAKELONG (dwPhysAddrOffset, &_F000h);
	  break;
	default:
	  boolSynthesizeSelector = TRUE;
	  dwPhysAddrLinear =
	    ((DWORD) dwPhysAddrSegment << 4L) + dwPhysAddrOffset;
	  break;
	}
    }
  else
#endif
    {
      if (0x100000 < bytes)
	{
	  return (NULL);
	}
      max_size = bytes;
      dwPhysAddrLinear = MapPhysicalToLinear (phys_addr, bytes);
      boolSynthesizeSelector = TRUE;
    }

  if (max_size < bytes)
    {
      return (NULL);
    }

  if (boolSynthesizeSelector)
    {
      wPhysAddrSelector = SynthSelector (dwPhysAddrLinear, max_size);
      if (NULL != ptr_to_selector)
	{
	  *ptr_to_selector = wPhysAddrSelector;
	}
      lpPhysAddr = GetSelectorPointer (wPhysAddrSelector);
    }
  return (lpPhysAddr);
}

/*--------------------------------------------------------------*/
/*  This function is a shell for DPMI Map Physical To Linear.   */
/*  Returns 0 if it failed or the physical address is below     */
/*  1 MB. Returns the linear address if DPMI call succeeded.    */
/*--------------------------------------------------------------*/
DWORD
MapPhysicalToLinear (DWORD dwPhysical, DWORD dwLength)
{
  DWORD dwLinear = 0L;		/* In case memory below 1 MB, we */
  /*  don't want to return garbage. */

  if (dwPhysical >= 0x100000L)	/* Use only if above 1 MB. */
    {
      _asm
      {
	push di push si mov bx, WORD PTR[dwPhysical + 2]	/* Load arguments. */
	mov cx, WORD PTR[dwPhysical] mov si, WORD PTR[dwLength + 2] mov di, WORD PTR[dwLength] mov ax, 800 h int 31 h	/* Issue DPMI call. */
        jc short error_return mov dx, bx mov ax, cx jmp short fine_return}
      error_return:_asm
      {
      xor ax, ax mov dx, ax}
      fine_return:_asm
      {
	mov WORD PTR[dwLinear + 2], dx	/* Return value. */
        mov WORD PTR[dwLinear], ax pop si pop di}
    }
  return dwLinear;
}

/*--------------------------------------------------------------*/
/*   This function will allocate and initialize a selector.     */
/*--------------------------------------------------------------*/
WORD
SynthSelector (DWORD dwLinearAddress, DWORD dwLength)
{
  WORD tempSelector, selector = NULL;
  WORD data_segment;

  /* Allocate *one* temporary selector by using value of DS  */
  /* (DS contains selector of app's or DLL's DGROUP, which   */
  /* is less than 64K.) Then set the selector's base        */
  /* address and limit to the real values, which may be      */
  /* larger than 64K. Because the memory must be accessed by  */
  /* 16-bit code, it is necessary to allocate an array of    */
  /* tiled selectors. The temporary selector is used to     */
  /* force AllocSelector() to allocate an array with the     */
  /* proper number of tiled selectors each with the proper   */
  /* base and limit. Then, we free the single temporary      */
  /* selector.                         */
#ifdef __BORLANDC__
  _asm
  {
  push ds pop ax mov data_segment, ax}
  tempSelector = AllocSelector (data_segment);
#else
  _asm
  {
  push ds call AllocSelector mov tempSelector, ax}
#endif
  if (tempSelector)		/* AllocSelector returns NULL on error. */
    {
      SetSelectorBase (tempSelector, dwLinearAddress);
      SetSelectorLimit (tempSelector, dwLength);

      selector = AllocSelector (tempSelector);

      SetSelectorLimit (tempSelector, 100L);
      FreeSelector (tempSelector);
    }
  return selector;
}

/*--------------------------------------------------------------*/
/*   This function builds a pointer to the memory referenced    */
/*   by the selector.                                           */
/*--------------------------------------------------------------*/
LPSTR
GetSelectorPointer (WORD selector)
{
  return (LPSTR) MAKELONG (0, selector);
}

#else

/*****************************************************************************
**
**      Function:   move_physmem
**
**      Purpose:    The function movphy moves data from/to physical extended
**                  memory.  This is done in word increments up to 64K bytes.
**
**      Args:
**          target      Physical target address.
**          source      Physical source address.
**          wcount      Number of D16 words to transfer.
**
**      Returns:
**          0           If transfer completed normally.
**          nonzero     If BIOS transfer function failed.
**
**      Calls:
**          fprintf, exit, memset, _int86x
**
*****************************************************************************/
int
move_physmem (unsigned long target, unsigned long source, long bcount)
{
  static initialized = 0;	/* Has the CPU been checked. */
  static unsigned char addrln = 0;	/* Number of addr. lines CPU drives   */
  unsigned char gdt[48];	/* Global Descriptor Table,see below  */
  unsigned char far *g_p = gdt;	/* Pointer to GDT loaded into SI & ES */

#ifdef __BORLANDC__
  union REGS r;			/* Structure of INTEL registers       */
  struct SREGS s;		/* Structure of INTEL segment reg's   */
#endif
#ifdef _MSC_VER
  union _REGS r;		/* Structure of INTEL registers       */
  struct _SREGS s;		/* Structure of INTEL segment reg's   */
#endif

  /* Movphy can not transfer more than (32K - 1) words */
  if (bcount >= 0x7ffffL)
    {
      fprintf (stderr, "\nTransfer length too large.\n");
      return (-1);
    }

  /*****************************************************************************
   **
   **      Identify system processor type and its addressing capability.
   **
   *****************************************************************************/

  if (!initialized)
    {
      /*lint -e122 */
#ifdef __BORLANDC__
      asm
      {
#else
      __asm
      {
#endif
	/* 8086 CPU check */
	/* Bits 12-15 are always set on the 8086 processor. */
	pushf;			/* Save EFLAGS.                      */
	pop bx;			/* Store EFLAGS in BX                */
	mov ax, 0f ffh;		/*   clear bits 12-15                */
	and ax, bx;		/*     in EFLAGS                     */
	push ax;		/* Store new EFLAGS value on stack.  */
	popf;			/* Replace current EFLAGS value.     */
	pushf;			/* Set new EFLAGS.                   */
	pop ax;			/* Store new EFLAGS in AX.           */
	and ax, 0f 000 h;	/* If bits 12-15 are set, then the   */
	cmp ax, 0f 000 h;	/*   CPU is an 8086/8088.            */
	mov addrln, 20;		/* Set the number of address lines   */
	/*   for a 8086/8088.                */
	je nodescriptors;


	/* 80286 CPU check */
	/* Bits 12-15 are always clear on the 80286 processor. */

	or bx, 0f 000 h;	/* Try to set bits 12-15             */
	push bx;		/*  */
	popf;			/*  */
	pushf;			/*  */
	pop ax;			/*  */
	and ax, 0f 000 h;	/* If bits 12-15 are cleared, then   */
	/*   the CPU is an 80286             */
	mov addrln, 24;		/* Set the number of address lines   */
	/*   for a 80286.                    */
	jz descriptors;		/*  */


	/* Otherwise it is an i386 or later CPU with 32 bit addressing. */
	mov addrln, 32;		/* Set the number of address lines   */
	/* for either a i386 or later CPU's. */
      }
      /*lint +e122 */


    nodescriptors:
      if (addrln < 24)
	{
	  fprintf (stderr,
		   "\nThis system does not support protected mode.\n");
	  return (-1);
	}

      /* For i286 check against A24 address limit. */
    descriptors:
      if ((addrln < 32) && (source >= (1UL << 24) || target >= (1UL << 24)))
	{
	  fprintf (stderr, "\nA24 Address out of bounds.\n");
	  return (-1);
	}
      initialized = 1;
    }

  /*****************************************************************************
   **
   **      Format of descriptors
   **
   **    Offset    Size                Description
   **
   **     00h        2 Bytes        Bits 0-15 of the segment limit.
   **
   **     02h        3 Bytes        Bits 0-23 of physical address.
   **
   **     05h        1 Byte         i386 & i486 Access flags:
   **                               9Ah = Code segment.
   **                               92h = Writable data segment.
   **                               90h = Read only data segment.
   **
   **                               80286 Access flags:
   **                               9Bh = Code segment.
   **                               93h = Writable data segment.
   **                               91h = Read only data segment.
   **
   **     06h        2 Bytes        Reserved on an 80286, must be zero.
   **
   **                               A32 Processors Only.
   **     06h        1 Byte         Bits 0-3 are the upper 4 bits of the
   **                               segment limit.  Bits 4-7 are set to zero.
   **                               Please note that this byte is not supported
   **                               by Interrupt 0x15 Function 0x87.
   **
   **                               A32 Processors Only.
   **     07h        1 Byte         Bits 24-32 of physical address.
   **
   *****************************************************************************/

  memset (gdt, '\0', sizeof (gdt));	/* Zero out global descripter table  */

  /* Descripter 2: Source */
  gdt[16] = (unsigned char) bcount;
  gdt[17] = (unsigned char) (bcount >> 8);
  gdt[18] = (unsigned char) source;
  gdt[19] = (unsigned char) (source >> 8);
  gdt[20] = (unsigned char) (source >> 16);

  if (addrln < 32)
    {
      gdt[21] = 0x93;		/* A24 Access flag */
    }
  else
    {
      gdt[21] = 0x92;		/* A32 Access flag */
      gdt[23] = (unsigned char) (source >> 24);
    }

  /* Descripter 3: Target */
  gdt[24] = (unsigned char) bcount;
  gdt[25] = (unsigned char) (bcount >> 8);
  gdt[26] = (unsigned char) target;
  gdt[27] = (unsigned char) (target >> 8);
  gdt[28] = (unsigned char) (target >> 16);

  if (addrln < 32)
    {
      gdt[29] = 0x93;		/* A24 Access flag */
    }
  else
    {
      gdt[29] = 0x92;		/* A32 Access flag */
      gdt[31] = (unsigned char) (target >> 24);
    }

  r.h.ah = 0x87;
  r.x.cx = (int) (bcount / 2);
  s.es = (((unsigned long) g_p) & 0xffff0000L) >> 16;
  r.x.si = ((unsigned long) g_p) & 0x0000ffffL;

#ifdef __BORLANDC__
  int86x (0x15, &r, &r, &s);
#endif

#ifdef _MSC_VER
  (void) _int86x (0x15, &r, &r, &s);
#endif

  if (r.h.ah)
    {
      switch (r.h.ah)
	{
	case 1:
	  /* RAM parity error occurred */
	  fprintf (stderr,
		   "\nData transfer failed.  A RAM parity error occurred.\n");
	  break;

	case 2:
	  /* Exception interrupt error occurred */
	  fprintf (stderr,
		   "\nData transfer failed.  Exception interrupt error occurred.\n");
	  break;

	case 3:
	  /* Gating address line 20 failed during switch to protected mode */
	  fprintf (stderr,
		   "\nData transfer failed.  Gate address line 20 failed.\n");
	  break;

	default:
	  /* Unknown reason-word count > 32767 */
	  fprintf (stderr,
		   "\nData transfer failed.  Reason unknown, possibly word count > 32767.\n");
	  break;
	}
      return (-1);
    }


  return (bcount);
}

#endif
