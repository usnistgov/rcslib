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

static char buf[256];
static char cmdstring[256];
static char indexstring[256];
static char valuestring[256];

int
main(int argc, const char **argv)
{
  if(argc < 4)
    {
      printf("usage: bufname procname cfgfile\n");
      exit(-1);
    }

  NML *nml = new NML(nml_test_format,argv[1],argv[2],argv[3]);
  int linenum=0;
  BOP_MSG bop_msg;
  while(!feof(stdin))
    {
      linenum++;
      fgets(buf,sizeof(buf),stdin);
      enum NML_BITWISE_OP_TYPE bop;
      sscanf(buf,"%s %s %s",cmdstring,indexstring,valuestring);
      if(!strcmp(cmdstring,"OR"))
	{
	  bop = NML_BITWISE_OR_OP;
	}
      else if(!strcmp(cmdstring,"AND"))
	{
	  bop = NML_BITWISE_AND_OP;
	}
      else
	{
	  fprintf(stderr,"Bad line %d:%s\n",linenum,buf);
	  continue;
	}
      unsigned long index = strtoul(indexstring,0,0);
      unsigned long value = strtoul(valuestring,0,0);
      if(bop == NML_BITWISE_AND_OP)
	{
	  bop_msg.mask_all();
	}
      else
	{
	  bop_msg.clear();
	}
      bop_msg.ula[index] = value;
      nml->write_with_bitwise_op(bop_msg,bop);
    }
  delete nml;
}

      
      
