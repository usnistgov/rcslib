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

#include <stdio.h>
#include <stdlib.h>

#include "_tty.h"

int
main (int argc, char **argv)
{
#ifdef UNIX_LIKE_PLAT
  char *ttyDevName = "/dev/ttyb";
#else
  char *ttyDevName = "COM2:";
#endif
  if (argc > 1)
    {
      ttyDevName = argv[1];
    }
  RCS_SERIAL_PORT_HANDLE handle =
    open_serial_communications_port (ttyDevName);

  print_serial_port_configuration (handle);
  set_serial_port_configuration (handle, NULL);
  char buf[80];
  memset (buf, 0, 80);
  int newline_received = 0;
  printf ("Receiving line:\n");
  while (1)
    {
      int bytes_read = read_serial_communications_port (handle, buf, 80);
      if (bytes_read != 0)
	{
	  //printf("nytes_read = %d\n",bytes_read);
	  for (int i = 0; i < bytes_read; i++)
	    {
	      if (buf[i] == '\n' || buf[i] == '\r')
		{
		  newline_received = 1;
		}
	      //printf("%x ",buf[i]);
	    }
	  //printf("\n");
	  fputs (buf, stdout);
	  //printf("\n");
	}
      if (newline_received)
	{
	  printf ("Enter line to send:\n");
	  fgets (buf, 80, stdin);
	  write_serial_communications_port (handle, buf, strlen (buf));
	  newline_received = 0;
	  memset (buf, 0, 80);
	  printf ("Receiving line:\n");
	}
    }
}
