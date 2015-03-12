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


#include "nml_test_format.hh"


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void
touch_running_file(int argc,const char **argv)
{
  FILE *f=fopen("nml_test_server.running","w");
  if(f)
    {
      fprintf(f,"$Id: nml_test_server.cc 1717 2011-03-09 13:40:18Z shackle $ compiled on " __DATE__ " at " __TIME__ ".\n");
      for(int i = 0; i < argc; i++)
	{
	  fprintf(f,"%s ",argv[i]);
	}
      fprintf(f,"\n");
      fclose(f);
    }
}

static int sargc =0;
static const char **sargv=0;

static void 
start_func(void) {
  printf("Starting NML server(s) . . . \n");
  rcsinfo();
  fflush(stdout);
  fflush(stderr);
  touch_running_file(sargc,sargv);
};

int 
main(int argc, const char **argv)
{
  nmlSetToServer();
  nmlSetIgnoreRemote();
  nmlSetIgnoreNoBufline();

  sargc = argc;
  sargv = argv;

  NML *nml_ptr;
  printf("--stdout-- nml_test_server: %s compiled on %s at %s\n",__FILE__,__DATE__,__TIME__);
  fprintf(stderr,"--stderr-- nml_test_server: %s compiled on %s at %s\n",__FILE__,__DATE__,__TIME__);
  
  rcs_print("--rcs_print-- nml_test_server: %s compiled on %s at %s\n",__FILE__,__DATE__,__TIME__);
  print_rcs_version();
  
  if(argc < 3)
    {
      fprintf(stderr,"usage : bufname procname configfile\n");
      exit(-1);
    }
  
  nml_ptr = new NML(nml_test_format,argv[1],argv[2],argv[3]);
  rcs_print("nml_ptr=%p\n",(void*)nml_ptr);
  printf("nml_ptr->cms->isserver=%d\n",
	 nml_ptr->cms->isserver);

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
#if RCS_MAJOR_VERSION >= 2010
  run_nml_servers_with_func(start_func);
#else
  run_nml_servers();
#endif

  exit(0);
}
