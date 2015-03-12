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



#include "rcs.hh"
#include "nml_test_format.hh"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>


static char buf[256];
static char cmdstring[256];
static char indexstring[256];
static char valuestring[256];

static bool quit=false;

void sigint_handler(int sig)
{
  quit=true;
}

int
main(int argc, const char **argv)
{
  if(argc < 4)
    {
      printf("usage: bufname procname cfgfile\n");
      exit(-1);
    }
  
  signal(SIGINT,sigint_handler);

  NML *nml = new NML(nml_test_format,argv[1],argv[2],argv[3]);
  BOP_MSG *bop_msg;
  while(!quit)
    {
      NMLTYPE rt = nml->blocking_read(-1);
      if(rt != BOP_MSG_TYPE)
	{
	  quit=true;
	  break;
	}
      bop_msg = (BOP_MSG *) nml->get_address();
      printf("0x%8.8X \t0x%8.8X \t0x%8.8X \t0x%8.8X\n",
	     bop_msg->ula[0],bop_msg->ula[1],bop_msg->ula[2],bop_msg->ula[3]);
    }
  delete nml;
}

      
      
