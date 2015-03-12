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
* File: sendn.c
* Purpose: Provides a C file for the sendn function from
* the book Advanced Programming in the UNIX Environment by Richard Stevens.
* The sendn function calls the send function repeatedly until n bytes
* have been written to the file descriptor.
*************************************************************************/

#if HAVE_CONFIG_H
#include "rcs_config.h"
#include "rcs_config_include.h"

#else
/* This is neccessary to avoid muliple definitions of fd_set, etc when both
* RPC via PCNFS and Windows Sockets are to be available. */
#ifdef USE_PCNFS
#undef USE_PCNFS
#endif

#include "rcs_defs.hh"		/* _Windows */

#include <stdlib.h>		/* realloc() */
#include <string.h>		/* strcpy(), strcat() */

#endif

#include "sendline.h"		/* forward prototype, PRINT_SOCKET_WRITE_SIZE */
#include "sendn.h"		/* sendn() */
#include "rcs_prnt.hh"		/* PRINT_SOCKET_WRITE_SIZE, rcs_print_  */
#include "dbg_mem.h"		/* DEBUG_FREE,DEBUG_MALLOC,DEBUG_CALLOC */


#if defined(MSDOS) && !defined(_Windows)
static char sendline_buffer[0x2000];
static long sendline_buffer_size = 0x2000;
#else
static char *sendline_buffer = NULL;
static long sendline_buffer_size = 0;
#endif


/* Write "n" bytes to a descriptor. */
int
sendline (int fd, const char *cptr, int _flags, double _timeout)
{
  int length;
  length = (int) strlen (cptr);
#if defined(MSDOS) && !defined(_Windows)
  if (length + 2 > sendline_buffer_size)
    {
      rcs_print_error ("Since DOS doesn't seem to handle realloc properly\n");
      rcs_print_error
	("sending strings over %d with sendline are prohibited.",
	 sendline_buffer_size);
      return (-1);
    }
#else
  sendline_buffer_size = length + 18 - (length % 16);
  sendline_buffer = realloc (sendline_buffer, sendline_buffer_size);
  if (NULL == sendline_buffer)
    {
      rcs_print_error
	("sendline could not allocate %ld bytes for temperary buffer.\n",
	 sendline_buffer_size);
      rcs_print_error ("String to send was: %s", cptr);
      return (-1);
    }
#endif
  strcpy (sendline_buffer, cptr);
  strcat (sendline_buffer, "\n");
  rcs_print_debug (PRINT_SOCKET_WRITE_SIZE, "sendline %s to %d.\n", cptr, fd);
  return sendn (fd, sendline_buffer, length + 1, _flags, _timeout);
}
