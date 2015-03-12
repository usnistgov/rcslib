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


#include "nml_test_unbounded_format.hh"


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void
touch_running_file(int argc,const char **argv)
{
  FILE *f=fopen("nml_test_unbounded_server.running","w");
  if(f)
    {
      fprintf(f,"$Id: nml_test_unbounded_server.cc 942 2007-03-13 15:29:06Z shackle $ compiled on " __DATE__ " at " __TIME__ ".\n");
      for(int i = 0; i < argc; i++)
	{
	  fprintf(f,"%s ",argv[i]);
	}
      fprintf(f,"\n");
      fclose(f);
    }
}

int 
main(int argc, const char **argv)
{
  NML *nml_ptr;
  printf("--stdout-- nml_test_unbounded_server: %s compiled on %s at %s\n",__FILE__,__DATE__,__TIME__);
  fprintf(stderr,"--stderr-- nml_test_unbounded_server: %s compiled on %s at %s\n",__FILE__,__DATE__,__TIME__);
  
  rcs_print("--rcs_print-- nml_test_unbounded_server: %s compiled on %s at %s\n",__FILE__,__DATE__,__TIME__);
  print_rcs_version();
  
  if(argc < 3)
    {
      fprintf(stderr,"usage : bufname procname configfile\n");
      exit(-1);
    }
  
  nml_ptr = new NML(nml_test_unbounded_format,argv[1],argv[2],argv[3],1);
  rcs_print("nml_ptr=%p\n",(void*)nml_ptr);

  if(!nml_ptr)
    {
      fprintf(stderr,"OUT-OF-MEMORY\n");
      exit(127);
    }
  if(!nml_ptr->valid())
    {
      fprintf(stderr,"nml_ptr->valid() check failed.\n");
      exit(-1);
    }
  printf("Starting NML server(s) . . . \n");
  fflush(stdout);
  fflush(stderr);
  touch_running_file(argc,argv);
  run_nml_servers();
  exit(0);
}
